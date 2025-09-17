// Copyright 2024, 2025 Philipp Stephani
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// Integration tests for //elisp/proto:module.
//
// Most tests are in //tests/proto:proto-test.el; this file contains tests that
// require a fresh Emacs process.
package module_test

import (
	"bytes"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var cat = flag.String("cat", "", "location of the //tests/proto:cat target")

// Integration test for elisp/proto/insert-stdin.
func TestInsertStdin(t *testing.T) {
	cat, err := runfiles.Rlocation(*cat)
	if err != nil {
		t.Fatal(err)
	}
	outfile := filepath.Join(t.TempDir(), "out")
	cmd := exec.Command(cat, ">", outfile)
	cmd.Stdin = strings.NewReader("stdin \xFF")
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
	got, err := os.ReadFile(outfile)
	if err != nil {
		t.Error(err)
	}
	if string(got) != "stdin \xFF" {
		t.Errorf("unexpected file content: %q", got)
	}
}

// Integration test for elisp/proto/write-stdout.
func TestWriteStdout(t *testing.T) {
	cat, err := runfiles.Rlocation(*cat)
	if err != nil {
		t.Fatal(err)
	}
	infile := filepath.Join(t.TempDir(), "in")
	if err := os.WriteFile(infile, []byte("stdout \xFF"), 0400); err != nil {
		t.Error(err)
	}
	cmd := exec.Command(cat, "<", infile)
	stdout := new(strings.Builder)
	stderr := new(bytes.Buffer)
	cmd.Stdout = stdout
	cmd.Stderr = stderr
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
	if got := stdout.String(); got != "stdout \xFF" {
		t.Errorf("unexpected stdout: %q", got)
	}
	if stderr.Len() != 0 {
		t.Errorf("unexpected stderr: %q", stderr.Bytes())
	}
}
