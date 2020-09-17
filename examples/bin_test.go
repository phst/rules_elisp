// Copyright 2020 Google LLC
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

package bin_test

import (
	"os"
	"os/exec"

	"github.com/phst/runfiles"
)

func Example() {
	bin, err := runfiles.Path("phst_rules_elisp/examples/bin")
	if err != nil {
		panic(err)
	}
	env, err := runfiles.Env()
	if err != nil {
		panic(err)
	}
	// You can run the programs produced by elisp_binary rules like any
	// other binary.
	cmd := exec.Command(bin, "human")
	cmd.Stdout = os.Stdout
	// Note: Emacs writes to stderr, but the example runner only captures
	// stdout.
	cmd.Stderr = os.Stdout
	// The working directory doesn’t matter.  Binaries still find their
	// runfiles.  Be sure to pass environment variables to find runfiles.
	// We also set GCOV_PREFIX (see
	// https://gcc.gnu.org/onlinedocs/gcc/Cross-profiling.html) to a
	// directory that’s hopefully writable, to avoid logspam when running
	// with “bazel coverage”.
	cmd.Dir = "/"
	cmd.Env = append(env, "PATH="+os.Getenv("PATH"), "GCOV_PREFIX="+os.TempDir())
	if err := cmd.Run(); err != nil {
		panic(err)
	}
	// Output:
	// hi from bin, ("human")
	// hi from lib-2
	// hi from lib-4
	// hi from lib-1
	// hi from data dependency
}
