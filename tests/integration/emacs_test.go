// Copyright 2020-2023, 2025, 2026 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Integration tests that run Emacs binaries.
package integration_test

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"strconv"
	"testing"
	"testing/quick"

	"github.com/bazelbuild/rules_go/go/runfiles"
	"github.com/google/go-cmp/cmp"

	"github.com/phst/rules_elisp/internal/testutil"
)

var (
	emacs       = testutil.RunfileFlag("//emacs")
	empty       = testutil.RunfileFlag("//tests:empty")
	exit        = testutil.RunfileFlag("//tests/integration:exit")
	signal      = testutil.RunfileFlag("//tests/integration:signal")
	launcher    = testutil.RunfileFlag("//tests/integration/wrap:launcher")
	binaryH     = testutil.RunfileFlag("//elisp/private/tools:binary.h")
	binaryCc    = testutil.RunfileFlag("//elisp/private/tools:binary.cc")
	runfilesElc = testutil.RunfileFlag("//elisp/runfiles:runfiles.elc")
)

func TestRun(t *testing.T) {
	rfEnv, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	env := append(os.Environ(), rfEnv...)
	for _, tc := range []struct {
		name             string
		program          string
		args             []string
		wantCode         int
		wantOut, wantErr string
	}{
		{"emacs --version", *emacs, []string{"--version"}, 0, `^GNU Emacs \d+`, `^$`},
		{"empty binary", *empty, nil, 0, `^$`, `^$`},
		{"signal", *signal, nil, signalCode, `(?s)^\r?
Error: error \("Foo"\).*
  error\("Foo"\).*
  normal-top-level\(\)\r?
$`, `(?m)^Foo\r?\n\z`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			cmd := exec.Command(tc.program, tc.args...)
			cmd.Env = env
			outBuf := new(bytes.Buffer)
			errBuf := new(bytes.Buffer)
			cmd.Stdout = outBuf
			cmd.Stderr = errBuf
			err := cmd.Run()
			if gotCode := exitCode(t, err); gotCode != tc.wantCode {
				t.Errorf("exit code: got %#x, want %#x", gotCode, tc.wantCode)
			}
			gotOut := outBuf.Bytes()
			wantOut := regexp.MustCompile(tc.wantOut)
			if !wantOut.Match(gotOut) {
				t.Errorf("standard output: got:\n%s\nwant something that matches:\n%s", gotOut, wantOut)
			}
			gotErr := errBuf.Bytes()
			wantErr := regexp.MustCompile(tc.wantErr)
			if !wantErr.Match(gotErr) {
				t.Errorf("standard error: got:\n%s\nwant something that matches:\n%s", gotErr, wantErr)
			}
		})
	}
}

func TestRunExitCode(t *testing.T) {
	env, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	// Dealing with exit codes in a portable way is a bit of a mess.  On
	// Unix, exit codes are 32-bit signed integers, cf. _exit and waitid.
	// On Windows, exit codes are 32-bit unsigned integers (DWORDs),
	// cf. ExitProcess and GetExitCodeProcess.  Go correctly obtains the
	// exit status and converts it to a 64-bit signed integer.  Emacs calls
	// exit, which on Windows casts the 32-bit argument to a DWORD and calls
	// ExitProcess.  We need to perform the inverse of that cast here to get
	// the right results.
	run := func(i exitStatus) int {
		cmd := exec.Command(*exit, strconv.Itoa(int(int32(i))))
		cmd.Env = env
		err := cmd.Run()
		return exitCode(t, err)
	}
	cast := func(i exitStatus) int { return int(i) }
	// Test some special magic exit codes.
	for _, s := range []int{-1, +1} {
		for _, m := range []int{0, 1, 0x7F, 0x80, 0xFF, 0x100} {
			i := exitStatus(s * m)
			t.Run(fmt.Sprintf("%#x", i), func(t *testing.T) {
				got := run(i)
				want := cast(i)
				if got != want {
					t.Errorf("got %#x, want %#x", got, want)
				}
			})
		}
	}
	// Test random exit codes.
	if err := quick.CheckEqual(run, cast, nil); err != nil {
		t.Error(err)
	}
}

// Test that running a binary with a wrapper works.
func TestRunWrapped(t *testing.T) {
	cmd := exec.Command(
		*launcher,
		"--option",
		*binaryCc,
		" \t\n\r\f äα𝐴🐈'\\\"",
		"/:"+outputFile,
	)
	cmd.Stderr = t.Output()
	out, err := cmd.Output()
	if err != nil {
		t.Fatal(err)
	}
	var got struct {
		Args     []string
		Manifest string
	}
	if err := json.Unmarshal(out, &got); err != nil {
		t.Fatal(err)
	}
	runfilesLib := *runfilesElc
	gotArgs := got.Args
	dir, err := runfiles.Rlocation("rules_elisp")
	if err != nil {
		dir = "ERROR \u0000" // this can never match
	}
	// The load path setup depends on whether we use manifest-based or
	// directory-based runfiles.
	wantArgs := [][]string{
		// Directory-based runfiles.
		{
			"--quick",
			"--batch",
			"--directory=" + dir,
			"--option",
			*binaryCc,
			" \t\n\r\f äα𝐴🐈'\\\"",
			"/:" + outputFile,
		},
		// Manifest-based runfiles.
		{
			"--quick",
			"--batch",
			"--load=" + runfilesLib,
			"--funcall=elisp/runfiles/install-handler",
			"--directory=/bazel-runfile:rules_elisp",
			"--option",
			*binaryCc,
			" \t\n\r\f äα𝐴🐈'\\\"",
			"/:" + outputFile,
		},
	}
	var diffs []string
	for _, want := range wantArgs {
		diffs = append(diffs, cmp.Diff(gotArgs, want))
	}
	// Pick the smaller difference for reporting.
	if diff := slices.MinFunc(diffs, func(a, b string) int { return len(a) - len(b) }); diff != "" {
		t.Errorf("positional arguments: -got +want:\n%s", diff)
	}
	jsonData := []byte(got.Manifest)
	var gotManifest map[string]any
	if err := json.Unmarshal(jsonData, &gotManifest); err != nil {
		t.Fatalf("can’t decode manifest: %s", err)
	}
	wantManifest := map[string]any{
		"root":        "RUNFILES_ROOT",
		"tags":        []any{"local", "mytag"},
		"loadPath":    []any{"rules_elisp"},
		"inputFiles":  []any{*binaryCc, *binaryH},
		"outputFiles": []any{outputFile},
	}
	if diff := cmp.Diff(
		gotManifest, wantManifest,
		cmp.FilterPath(isInputFile, cmp.Transformer("", resolveRunfile)),
	); diff != "" {
		t.Errorf("manifest: -got +want:\n%s", diff)
	}
}

func isInputFile(p cmp.Path) bool {
	if len(p) < 2 {
		return false
	}
	m, ok := p[1].(cmp.MapIndex)
	if !ok {
		return false
	}
	k := m.Key()
	return k.Kind() == reflect.String && k.String() == "inputFiles"
}

func resolveRunfile(s string) string {
	if filepath.IsAbs(s) {
		return s
	}
	r, err := runfiles.Rlocation(s)
	if err != nil {
		log.Fatalf("error resolving runfile for comparison: %s", err)
	}
	return r
}

func exitCode(t *testing.T, err error) int {
	if err == nil {
		return 0
	}
	var exitErr *exec.ExitError
	if !errors.As(err, &exitErr) {
		t.Fatalf("error has unexpected type %T: %s", err, err)
	}
	return exitErr.ExitCode()
}
