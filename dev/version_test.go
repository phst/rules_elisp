// Copyright 2023, 2024 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package version_test

import (
	"fmt"
	"regexp"
	"slices"
	"sort"
	"strings"
	"testing"

	_ "embed"

	"github.com/bazelbuild/buildtools/build"
	"github.com/google/go-cmp/cmp"
)

func TestDependencyVersions(t *testing.T) {
	moduleFile, err := build.ParseModule("MODULE.bazel", moduleContent)
	if err != nil {
		t.Fatal(err)
	}
	commitHashVersionRegexp := regexp.MustCompile(`^[.\d]+-\d+-([[:xdigit:]]+)$`)
	moduleDeps := make(map[string]dependency)
	moduleOnlyDeps := []string{
		// Only used as an example.
		"phst_rules_elisp_example",
		// Tools that are only run locally and therefore don’t need a
		// legacy WORKSPACE dependency.
		"hedron_compile_commands",
		"phst_bazelcov",
		"phst_merge_bazel_lockfiles",
		"phst_update_workspace_snippets",
	}
	for _, rule := range moduleFile.Rules("bazel_dep") {
		name := rule.Name()
		version := rule.AttrString("version")
		dev := rule.AttrLiteral("dev_dependency") == "True"
		if name == "" {
			t.Fatalf("invalid bazel_dep rule %q", name)
		}
		if slices.Contains(moduleOnlyDeps, name) {
			continue
		}
		if m := commitHashVersionRegexp.FindStringSubmatch(version); m != nil {
			version = m[1]
		}
		if _, dup := moduleDeps[name]; dup {
			t.Errorf("duplicate module dependency %q", name)
		}
		moduleDeps[name] = dependency{version, dev}
	}
	if len(moduleDeps) == 0 {
		t.Error("no module dependencies found")
	}

	legacyDeps := make(map[string]dependency)
	repositoriesFile, err := build.ParseBzl("elisp/repositories.bzl", repositoriesContent)
	if err != nil {
		t.Fatal(err)
	}
	addLegacyDependencies(t, legacyDeps, repositoriesFile, false)
	workspaceFile, err := build.ParseWorkspace("WORKSPACE", workspaceContent)
	if err != nil {
		t.Fatal(err)
	}
	addLegacyDependencies(t, legacyDeps, workspaceFile, true)
	if len(legacyDeps) == 0 {
		t.Error("no legacy dependencies found")
	}

	if diff := cmp.Diff(moduleDeps, legacyDeps); diff != "" {
		t.Errorf("-module +legacy\n%s", diff)
	}
}

func TestGoDependencies(t *testing.T) {
	moduleFile, err := build.ParseModule("MODULE.bazel", moduleContent)
	if err != nil {
		t.Fatal(err)
	}
	moduleDeps := make(map[string]string)
	for _, rule := range moduleFile.Rules("go_deps.module") {
		path := rule.AttrString("path")
		version := rule.AttrString("version")
		if path == "" || version == "" {
			t.Fatalf("invalid go_deps.module rule %q", path)
		}
		if _, dup := moduleDeps[path]; dup {
			t.Errorf("duplicate Go dependency %q", path)
		}
		moduleDeps[path] = version
	}

	workspaceFile, err := build.ParseWorkspace("WORKSPACE", workspaceContent)
	if err != nil {
		t.Fatal(err)
	}
	legacyDeps := make(map[string]string)
	for _, rule := range workspaceFile.Rules("go_repository") {
		path := rule.AttrString("importpath")
		version := rule.AttrString("version")
		if path == "" || version == "" {
			t.Fatalf("invalid go_repository rule %q", path)
		}
		if _, dup := legacyDeps[path]; dup {
			t.Errorf("duplicate Go dependency %q", path)
		}
		legacyDeps[path] = version
	}

	if diff := cmp.Diff(moduleDeps, legacyDeps); diff != "" {
		t.Errorf("-module +legacy\n%s", diff)
	}
}

func TestOverrides(t *testing.T) {
	file, err := build.ParseModule("MODULE.bazel", moduleContent)
	if err != nil {
		t.Fatal(err)
	}

	deps := make(map[string]bool)
	for _, rule := range file.Rules("bazel_dep") {
		deps[rule.Name()] = rule.AttrLiteral("dev_dependency") == "True"
	}
	if len(deps) == 0 {
		t.Error("no dependencies found")
	}

	for _, rule := range file.Rules("") {
		kind := rule.Kind()
		if !strings.HasSuffix(kind, "_override") {
			continue
		}
		name := rule.AttrString("module_name")
		dev, ok := deps[name]
		if !ok {
			t.Errorf("unknown dependency %s overridden by %s", name, kind)
		}
		// Overrides only work in root modules, so this dependency
		// wouldn’t work outside of our own workspace.
		// μpb must be overridden due to
		// https://github.com/protocolbuffers/protobuf/issues/13741.
		// This is unfortunate but currently required.  The
		// special-casing for μpb should be removed once both
		// https://github.com/protocolbuffers/protobuf/issues/13741 and
		// https://github.com/protocolbuffers/protobuf/issues/14564 are
		// fixed.
		if !dev && name != "upb" {
			t.Errorf("production dependency %s is overridden using %s", name, kind)
		}
	}
}

type dependency struct {
	Version string
	Dev     bool
}

func addLegacyDependencies(t *testing.T, r map[string]dependency, file *build.File, dev bool) {
	modulesByRepoName := map[string]string{
		"bazel-gazelle": "gazelle",
		"bazel-skylib":  "bazel_skylib",
	}
	f := func(x build.Expr, stk []build.Expr) error {
		call, ok := x.(*build.CallExpr)
		if !ok {
			return nil
		}
		rule := file.Rule(call)
		urls := rule.AttrStrings("urls")
		if s := rule.AttrString("url"); s != "" {
			urls = append(urls, s)
		}
		if len(urls) == 0 {
			return nil
		}
		repoName, version, err := parseURLs(urls)
		if err != nil {
			t.Errorf("invalid URLs for rule %q in file %q: %s", rule.Name(), file.DisplayPath(), err)
			return nil
		}
		moduleName := modulesByRepoName[repoName]
		if moduleName == "" {
			moduleName = repoName
		}
		if _, dup := r[moduleName]; dup {
			t.Errorf("duplicate legacy dependency %q in file %q", repoName, file.DisplayPath())
		}
		r[moduleName] = dependency{version, dev}
		return nil
	}
	build.WalkStatements(file, f)
}

func parseURLs(urls []string) (name, version string, _ error) {
	names := make(map[string]bool)
	versions := make(map[string]bool)
	for _, url := range urls {
		n, v, err := parseURL(url)
		if err != nil {
			return "", "", err
		}
		names[n] = true
		versions[v] = true
	}
	name, err := uniqueKey(names)
	if err != nil {
		return "", "", fmt.Errorf("invalid repository names: %s", err)
	}
	version, err = uniqueKey(versions)
	if err != nil {
		return "", "", fmt.Errorf("invalid repository versions: %s", err)
	}
	return
}

func parseURL(url string) (name, version string, err error) {
	s, ok := strings.CutPrefix(url, "https://")
	if !ok {
		return "", "", fmt.Errorf("invalid URL %q", url)
	}
	s = strings.TrimPrefix(s, "mirror.bazel.build/")
	p := strings.Split(s, "/")
	if len(p) < 3 || p[0] != "github.com" || p[1] == "" || p[2] == "" {
		return "", "", fmt.Errorf("invalid URL %q", url)
	}
	name = p[2]
	switch {
	case len(p) == 7 && p[3] == "releases" && p[4] == "download":
		version = p[5]
	case len(p) == 7 && p[3] == "archive" && p[4] == "refs" && p[5] == "tags":
		version = strings.TrimSuffix(strings.TrimSuffix(p[6], ".zip"), ".tar.gz")
	case len(p) == 5 && p[3] == "archive" && len(p[4]) > 7:
		version = p[4][:7]
	}
	version = strings.TrimPrefix(version, "v")
	if name == "" || version == "" {
		return "", "", fmt.Errorf("invalid URL %q", url)
	}
	return
}

func uniqueKey(m map[string]bool) (string, error) {
	var keys []string
	for k := range m {
		keys = append(keys, k)
	}
	if n := len(keys); n != 1 {
		sort.Strings(keys)
		return "", fmt.Errorf("got %d elements (%v), want exactly one", n, keys)
	}
	return keys[0], nil
}

//go:embed MODULE.bazel
var moduleContent []byte

//go:embed WORKSPACE
var workspaceContent []byte

//go:embed repositories.bzl
var repositoriesContent []byte
