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
package ert_test

import (
	"flag"
	"os"
	"os/exec"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var emacsRloc = flag.String("emacs", "", "location of //emacs relative to the runfiles root")

// Tests that emacs --version works.
func TestVersion(t *testing.T) {
	rf, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	emacs, err := rf.Rlocation(*emacsRloc)
	if err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(emacs, "--version")
	cmd.Env = append(os.Environ(), rf.Env()...)
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
}
