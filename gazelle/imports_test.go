// Copyright 2021, 2025 Google LLC
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
	"path"
	"path/filepath"
	"testing"

	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/bazel-gazelle/testtools"
	"github.com/google/go-cmp/cmp"

	"github.com/phst/rules_elisp/gazelle"
)

func TestImports(t *testing.T) {
	lang := gazelle.NewLanguage()
	config := testtools.NewTestConfig(t, nil, []language.Language{lang}, nil)
	for _, tc := range []struct {
		desc, pkg, build string
		want             []resolve.ImportSpec
	}{
		{
			desc:  "unrelated rule kind",
			build: `cc_library(name = "lib", srcs = ["lib.cc"])`,
			want:  nil,
		},
		{
			desc: "root package",
			pkg:  "",
			build: `elisp_library(
    name = "lib",
    srcs = [
        "lib.el",
        "subdir/file.el",
        "//other:qux.el",
        "@external//:bar.el",
    ],
    load_path = ["subdir", "/other", "/unrelated"],
)`,
			want: []resolve.ImportSpec{
				{Lang: "elisp", Imp: "lib"},
				{Lang: "elisp", Imp: "subdir/file"},
				{Lang: "elisp", Imp: "file"},
				{Lang: "elisp", Imp: "other/qux"},
				{Lang: "elisp", Imp: "qux"},
			},
		},
		{
			desc: "subpackage",
			pkg:  "lib",
			build: `elisp_library(
    name = "lib",
    srcs = [
        "lib.el",
        "subdir/file.el",
        "//other:qux.el",
        "@external//:bar.el",
    ],
    load_path = [".", "/other", "/unrelated"],
)`,
			want: []resolve.ImportSpec{
				{Lang: "elisp", Imp: "lib/lib"},
				{Lang: "elisp", Imp: "lib"},
				{Lang: "elisp", Imp: "lib/subdir/file"},
				{Lang: "elisp", Imp: "subdir/file"},
				{Lang: "elisp", Imp: "other/qux"},
				{Lang: "elisp", Imp: "qux"},
			},
		},
		{
			desc: "no custom load path",
			pkg:  "lib",
			build: `elisp_library(
    name = "lib",
    srcs = ["lib.el", "subdir/file.el"],
)`,
			want: []resolve.ImportSpec{
				{Lang: "elisp", Imp: "lib/lib"},
				{Lang: "elisp", Imp: "lib/subdir/file"},
			},
		},
		{
			desc:  "no source files",
			pkg:   "lib",
			build: `elisp_library(name = "lib", srcs = [])`,
			want:  []resolve.ImportSpec{},
		},
		{
			desc:  "protocol buffer library",
			pkg:   "lib",
			build: `elisp_proto_library(name = "my_elisp_proto")`,
			want: []resolve.ImportSpec{
				{Lang: "elisp", Imp: "lib/my.proto"},
			},
		},
	} {
		t.Run(tc.desc, func(t *testing.T) {
			fileName := filepath.FromSlash(path.Join("/workspace", tc.pkg, "BUILD"))
			f, err := rule.LoadData(fileName, tc.pkg, []byte(tc.build))
			if err != nil {
				t.Fatal(err)
			}
			if n := len(f.Rules); n != 1 {
				t.Fatalf("got %d rules in BUILD file, want one", n)
			}
			r := f.Rules[0]
			got := lang.Imports(config, r, f)
			if diff := cmp.Diff(got, tc.want); diff != "" {
				t.Error("-got +want:\n", diff)
			}
		})
	}
}
