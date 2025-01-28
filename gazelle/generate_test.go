// Copyright 2022, 2025 Google LLC
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
	"path/filepath"
	"testing"

	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/bazel-gazelle/testtools"
	"github.com/bazelbuild/buildtools/build"
	"github.com/google/go-cmp/cmp"
	"github.com/phst/rules_elisp/gazelle"
)

func TestGenerateRules(t *testing.T) {
	dir, cleanup := testtools.CreateFiles(t, []testtools.FileSpec{
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
(require 'cl-lib)
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
			Path:    "my.proto",
			Content: "",
		},
		{
			Path:    "doc.org",
			Content: "",
		},
		{
			Path:    "subdir/lib-2.el",
			Content: `(provide 'lib-2)`,
		},
		{
			Path:    "a/b/lib-3.el",
			Content: `(provide 'b/lib-3)`,
		},
	})
	t.Cleanup(cleanup)

	lang := gazelle.NewLanguage()
	cfg := testtools.NewTestConfig(t, nil, []language.Language{lang}, nil)

	for _, tc := range []struct {
		pkg   string
		files []string
		want  language.GenerateResult
	}{
		{
			pkg:   "",
			files: []string{"MODULE.bazel", "lib-1.el", "lib-1-test.el", "empty.el", "my.proto", "doc.org", "nonexisting.el"},
			want: language.GenerateResult{
				Gen: []*rule.Rule{
					newRule("elisp_manual", "doc", attr{"src", "doc.org"}, attr{"out", "doc.texi"}),
					newRule("elisp_library", "empty", strings("srcs", "empty.el")),
					newRule("elisp_library", "lib_1", strings("srcs", "lib-1.el")),
					newRule("elisp_test", "lib_1_test", strings("srcs", "lib-1-test.el")),
					newRule("elisp_proto_library", "my_elisp_proto", strings("deps", ":my_proto")),
				},
				Imports: []any{
					gazelle.Imports{},
					gazelle.Imports{},
					gazelle.Imports{Requires: []gazelle.Feature{"lib-2"}},
					gazelle.Imports{Requires: []gazelle.Feature{"lib-1"}},
					gazelle.Imports{},
				},
			},
		},
		{
			pkg:   "subdir",
			files: []string{"lib-2.el"},
			want: language.GenerateResult{
				Gen: []*rule.Rule{
					newRule("elisp_library", "lib_2", strings("srcs", "lib-2.el"), strings("load_path", ".")),
				},
				Imports: []any{gazelle.Imports{}},
			},
		},
		{
			pkg:   "a/b",
			files: []string{"lib-3.el"},
			want: language.GenerateResult{
				Gen: []*rule.Rule{
					newRule("elisp_library", "lib_3", strings("srcs", "lib-3.el"), strings("load_path", "/a")),
				},
				Imports: []any{gazelle.Imports{}},
			},
		},
	} {
		t.Run(tc.pkg, func(t *testing.T) {
			args := language.GenerateArgs{
				Config:       cfg,
				Dir:          filepath.Join(dir, filepath.FromSlash(tc.pkg)),
				Rel:          tc.pkg,
				RegularFiles: tc.files,
			}
			got := lang.GenerateRules(args)
			if diff := cmp.Diff(got, tc.want, cmp.Transformer("", transformRule)); diff != "" {
				t.Error("-got +want:\n", diff)
			}
		})
	}
}

func newRule(kind, name string, attrs ...attr) *rule.Rule {
	r := rule.NewRule(kind, name)
	for _, a := range attrs {
		r.SetAttr(a.name, a.value)
	}
	return r
}

type attr struct {
	name  string
	value any
}

func strings(name string, values ...string) attr {
	return attr{name, values}
}

func transformRule(r *rule.Rule) ruleInfo {
	i := ruleInfo{
		Kind:        r.Kind(),
		Name:        r.Name(),
		Args:        r.Args(),
		Attr:        make(map[string]build.Expr),
		PrivateAttr: make(map[string]any),
		Comments:    r.Comments(),
	}
	for _, k := range r.AttrKeys() {
		i.Attr[k] = r.Attr(k)
	}
	for _, k := range r.PrivateAttrKeys() {
		i.PrivateAttr[k] = r.PrivateAttr(k)
	}
	return i
}

type ruleInfo struct {
	Kind, Name  string
	Args        []build.Expr
	Attr        map[string]build.Expr
	PrivateAttr map[string]any
	Comments    []string
}
