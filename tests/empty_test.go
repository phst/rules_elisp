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

package ert_test

import (
	"bytes"
	"os"
	"os/exec"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var empty = runfileFlag("empty", "location of //tests:empty relative to the runfiles root")

// Tests that the empty binary produces empty output.
func TestRun(t *testing.T) {
	rf, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	binary := *empty
	cmd := exec.Command(binary)
	cmd.Env = append(os.Environ(), rf.Env()...)
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
