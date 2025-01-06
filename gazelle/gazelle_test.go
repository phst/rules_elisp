// Copyright 2021, 2022, 2023, 2025 Google LLC
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

package gazelle_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	_ "embed"

	"github.com/bazelbuild/bazel-gazelle/testtools"
)

func TestGazelleBinary(t *testing.T) {
	dir, clean := testtools.CreateFiles(t, []testtools.FileSpec{
		{
			Path:    "MODULE.bazel",
			Content: `module(name = "test")`,
		},
		{
			Path:    "empty.el",
			Content: "",
		},
		{
			Path: "lib-1.el",
			Content: `
(require 'lib-2)
(provide 'lib-1)
(provide 'foo)
`,
		},
		{
			Path: "lib-1-test.el",
			Content: `
(require 'lib-1)
(ert-deftest lib-1-test ())
(provide 'lib-1-test)
`,
		},
		{
			Path:    "pkg/lib-2.el",
			Content: `(provide 'lib-2)`,
		},
		{
			Path:    "a/b/lib-3.el",
			Content: `(provide 'b/lib-3)`,
		},
	})
	t.Cleanup(clean)

	bin := filepath.Join(dir, "gazelle.exe")
	if err := os.WriteFile(bin, gazelleBinary, 0500); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(bin, "update", "-repo_root="+dir)
	cmd.Dir = dir
	t.Log("running Gazelle binary")
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Error(err)
	}
	t.Logf("Gazelle binary finished, output follows:\n%s", output)

	testtools.CheckFiles(t, dir, []testtools.FileSpec{
		{
			Path: "BUILD.bazel",
			Content: `load("@phst_rules_elisp//elisp:defs.bzl", "elisp_library", "elisp_test")

elisp_library(
    name = "empty",
    srcs = ["empty.el"],
)

elisp_library(
    name = "lib_1",
    srcs = ["lib-1.el"],
    deps = ["//pkg:lib_2"],
)

elisp_test(
    name = "lib_1_test",
    srcs = ["lib-1-test.el"],
    deps = [":lib_1"],
)
`,
		}, {
			Path: "pkg/BUILD.bazel",
			Content: `load("@phst_rules_elisp//elisp:defs.bzl", "elisp_library")

elisp_library(
    name = "lib_2",
    srcs = ["lib-2.el"],
    load_path = ["."],
)
`,
		},
		{
			Path: "a/b/BUILD.bazel",
			Content: `load("@phst_rules_elisp//elisp:defs.bzl", "elisp_library")

elisp_library(
    name = "lib_3",
    srcs = ["lib-3.el"],
    load_path = ["/a"],
)
`,
		},
	})
}

//go:embed binary.exe
var gazelleBinary []byte
