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
	"flag"
	"fmt"
	"os"
	"os/exec"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var emacs = runfileFlag("//emacs")

// Tests that emacs --version works.
func TestVersion(t *testing.T) {
	rf, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	emacs := *emacs
	cmd := exec.Command(emacs, "--version")
	cmd.Env = append(os.Environ(), rf.Env()...)
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
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
