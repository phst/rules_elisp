// Copyright 2020-2025 Google LLC
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
	"flag"
	"os"
	"os/exec"
	"runtime"
	"testing"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var (
	launcherRloc = flag.String("launcher", "", "location of //tests/wrap:launcher relative to the runfiles root")
	binaryCcRloc = flag.String("binary.cc", "", "location of //elisp/private/tools:binary.cc relative to the runfiles root")
)

// Test that running a binary with a wrapper works.
func TestRunWrapped(t *testing.T) {
	rf, err := runfiles.New()
	if err != nil {
		t.Fatal(err)
	}
	inputFile, err := rf.Rlocation(*binaryCcRloc)
	if err != nil {
		t.Fatal(err)
	}
	windows := runtime.GOOS == "windows"
	launcher, err := rf.Rlocation(*launcherRloc)
	if err != nil {
		t.Fatal(err)
	}
	outputFile := "/:/tmp/output.dat"
	if windows {
		outputFile = `/:C:\Temp\output.dat`
	}
	cmd := exec.Command(
		launcher,
		"--option",
		inputFile,
		" \t\n\r\f äα𝐴🐈'\\\"",
		outputFile,
	)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Error(err)
	}
}
