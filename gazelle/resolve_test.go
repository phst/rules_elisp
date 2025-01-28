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

package gazelle

import (
	"testing"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/bazel-gazelle/testtools"
	"github.com/google/go-cmp/cmp"
)

func TestResolve(t *testing.T) {
	lang := NewLanguage()
	cfg := testtools.NewTestConfig(t, []config.Configurer{new(resolve.Configurer)}, []language.Language{lang}, nil)
	build, err := rule.LoadData("pkg/BUILD", "pkg", []byte(`
elisp_library(
    name = "lib",
    srcs = ["lib.el"],
    load_path = ["."],
)
`))
	if err != nil {
		t.Fatal(err)
	}
	if len(build.Rules) != 1 {
		t.Fatalf("got %d rules, want one", len(build.Rules))
	}
	ix := resolve.NewRuleIndex(func(*rule.Rule, string) resolve.Resolver { return lang })
	ix.AddRule(cfg, build.Rules[0], build)
	ix.Finish()
	rc, cleanup := repo.NewRemoteCache(nil)
	defer cleanup()
	testRule := rule.NewRule("elisp_test", "lib_test")
	imps := requires{"lib"}
	lbl := label.New("", "pkg", "lib_test")

	lang.Resolve(cfg, ix, rc, testRule, imps, lbl)

	got := testRule.AttrStrings("deps")
	want := []string{":lib"}
	if diff := cmp.Diff(got, want); diff != "" {
		t.Error("-got +want:\n", diff)
	}
}
