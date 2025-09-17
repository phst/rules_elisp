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

// Integration tests for the Emacs binary.
package ert_test

import (
	"flag"
	"os"
	"os/exec"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var emacs = flag.String("emacs", "", "runfile location of the Emacs binary")

// Tests that emacs --version works.
func TestVersion(t *testing.T) {
	runFiles, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	emacs, err := runFiles.Rlocation(*emacs)
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(emacs, "--version")
	cmd.Env = append(os.Environ(), runFiles.Env()...)
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
}
