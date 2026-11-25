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
	"errors"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"testing"
	"testing/quick"

	"github.com/bazelbuild/rules_go/go/runfiles"

	"github.com/phst/rules_elisp/internal/testutil"
)

var (
	emacsBin  = testutil.RunfileFlag("//emacs")
	emptyBin  = testutil.RunfileFlag("//tests:empty")
	signalBin = testutil.RunfileFlag("//tests/integration:signal")
)

func TestBinaryOutput(t *testing.T) {
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
		{"emacs --version", *emacsBin, []string{"--version"}, 0, `^GNU Emacs \d+`, `^$`},
		{"empty binary", *emptyBin, nil, 0, `^$`, `^$`},
		{"signal", *signalBin, nil, signalCode, `(?s)^\r?
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
			gotCode := 0
			if err := cmd.Run(); err != nil {
				var exitErr *exec.ExitError
				if errors.As(err, &exitErr) {
					gotCode = exitErr.ExitCode()
				} else {
					t.Errorf("error has unexpected type %T: %s", err, err)
				}
			}
			if gotCode != tc.wantCode {
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

var exitBin = testutil.RunfileFlag("//tests/integration:exit")

func TestBinaryExitCode(t *testing.T) {
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
		cmd := exec.Command(*exitBin, strconv.Itoa(int(int32(i))))
		cmd.Env = env
		err := cmd.Run()
		if err == nil {
			return 0
		}
		var exitErr *exec.ExitError
		if !errors.As(err, &exitErr) {
			t.Fatalf("error has unexpected type %T: %s", err, err)
		}
		return exitErr.ExitCode()
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
