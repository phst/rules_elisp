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

// Tests for //examples:bin.
package bin_test

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"

	"github.com/bazelbuild/rules_go/go/runfiles"
)

var bin = flag.String("bin", "", "runfile location of the binary under test")

// Example test showing how to work with elisp_binary rules.
func Example() {
	tempDir := os.TempDir()
	bin, err := runfiles.Rlocation(*bin)
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
	// stdout.  We filter out some irrelevant messages that can cause
	// spurious failures.
	r, w := io.Pipe()
	defer r.Close()
	defer w.Close()
	go filter(r, os.Stdout)
	cmd.Stderr = w
	// The working directory doesn’t matter.  Binaries still find their
	// runfiles.
	cmd.Dir = "/"
	// Be sure to pass environment variables to find runfiles.  We also set
	// GCOV_PREFIX (see
	// https://gcc.gnu.org/onlinedocs/gcc/Cross-profiling.html) and
	// LLVM_PROFILE_FILE (see
	// https://clang.llvm.org/docs/SourceBasedCodeCoverage.html) to a
	// directory/file that’s hopefully writable, to avoid logspam when
	// running with “bazel coverage”.
	cmd.Env = append(env,
		"EMACS="+os.Getenv("EMACS"),
		"PATH="+os.Getenv("PATH"),
		"GCOV_PREFIX="+tempDir,
		"LLVM_PROFILE_FILE="+filepath.Join(tempDir, "bazel.%p.profraw"),
	)
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

func filter(r io.Reader, w io.Writer) {
	s := bufio.NewScanner(r)
	for s.Scan() {
		line := s.Text()
		if !irrelevant.MatchString(line) {
			fmt.Fprintln(w, line)
		}
	}
}

// This message can happen depending on the mtime of files in the Bazel
// sandbox.  It shouldn’t influence the test outcome.
var irrelevant = regexp.MustCompile(`^Source file .+ newer than byte-compiled file; using older file$`)
