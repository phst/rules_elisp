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
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
	"github.com/google/go-cmp/cmp"
)

var (
	emacs       = runfileFlag("//emacs")
	empty       = runfileFlag("//tests:empty")
	launcher    = runfileFlag("//tests/wrap:launcher")
	binaryH     = runfileFlag("//elisp/private/tools:binary.h")
	binaryCc    = runfileFlag("//elisp/private/tools:binary.cc")
	runfilesElc = runfileFlag("//elisp/runfiles:runfiles.elc")
)

// Tests that emacs --version works.
func TestVersion(t *testing.T) {
	rfEnv, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(*emacs, "--version")
	cmd.Env = append(os.Environ(), rfEnv...)
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
}

// Tests that the empty binary produces empty output.
func TestRun(t *testing.T) {
	rfEnv, err := runfiles.Env()
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(*empty)
	cmd.Env = append(os.Environ(), rfEnv...)
	stdout := new(bytes.Buffer)
	stderr := new(bytes.Buffer)
	cmd.Stdout = stdout
	cmd.Stderr = stderr
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
	if stdout.Len() != 0 {
		t.Errorf("unexpected stdout: %q", stdout.Bytes())
	}
	if stderr.Len() != 0 {
		t.Errorf("unexpected stderr: %q", stderr.Bytes())
	}
}

// Test that running a binary with a wrapper works.
func TestRunWrapped(t *testing.T) {
	windows := runtime.GOOS == "windows"
	outputFile := "/:/tmp/output.dat"
	if windows {
		outputFile = `/:C:\Temp\output.dat`
	}
	cmd := exec.Command(
		*launcher,
		"--option",
		*binaryCc,
		" \t\n\r\f äα𝐴🐈'\\\"",
		outputFile,
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
	var wantOutputFile string
	if os.PathSeparator == '/' {
		wantOutputFile = "/tmp/output.dat"
	} else {
		wantOutputFile = `C:\Temp\output.dat`
	}
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
		"/:"+wantOutputFile,
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
		"outputFiles": []any{wantOutputFile},
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

func runfileFlag(name string) *string {
	r := new(runfileFlagValue)
	flag.Var(r, name, fmt.Sprintf("location of %s relative to the runfiles root", name))
	return (*string)(r)
}

type runfileFlagValue string

func (v runfileFlagValue) String() string {
	return string(v)
}

func (v *runfileFlagValue) Set(s string) error {
	p, err := runfiles.Rlocation(s)
	if err != nil {
		return err
	}
	*v = runfileFlagValue(p)
	return nil
}
