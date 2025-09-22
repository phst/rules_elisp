// Copyright 2020-2023, 2025 Google LLC
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
package tests_test

import (
	"bytes"
	"encoding/json"
	"errors"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"regexp"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
	"github.com/google/go-cmp/cmp"
	"github.com/phst/rules_elisp/private/testutil"
)

var (
	emacs       = testutil.RunfileFlag("//emacs")
	empty       = testutil.RunfileFlag("//tests:empty")
	signal      = testutil.RunfileFlag("//tests:signal")
	launcher    = testutil.RunfileFlag("//tests/wrap:launcher")
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
$`, `^Foo\r?\n$`},
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

// Test that running a binary with a wrapper works.
func TestRunWrapped(t *testing.T) {
	cmd := exec.Command(
		*launcher,
		"--option",
		*binaryCc,
		" \t\n\r\f äα𝐴🐈'\\\"",
		"/:"+outputFile,
	)
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
	wantArgs := []string{"--quick", "--batch"}
	// The load path setup depends on whether we use manifest-based or
	// directory-based runfiles.
	if dir, err := runfiles.Rlocation("phst_rules_elisp"); err == nil {
		// Directory-based runfiles.
		wantArgs = append(wantArgs, "--directory="+dir)
	} else {
		// Manifest-based runfiles.
		wantArgs = append(wantArgs,
			"--load="+runfilesLib,
			"--funcall=elisp/runfiles/install-handler",
			"--directory=/bazel-runfile:phst_rules_elisp",
		)
	}
	wantArgs = append(wantArgs,
		"--option",
		*binaryCc,
		" \t\n\r\f äα𝐴🐈'\\\"",
		"/:"+outputFile,
	)
	if diff := cmp.Diff(gotArgs, wantArgs); diff != "" {
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
		"loadPath":    []any{"phst_rules_elisp"},
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
