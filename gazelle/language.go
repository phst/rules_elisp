// Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

// Package gazelle implements Gazelle support for Emacs Lisp.  It generates and
// maintains elisp_library, elisp_proto_library, elisp_binary, and elisp_test
// rules from the phst_rules_elisp repository.  See https://phst.eu/rules_elisp
// and https://github.com/bazelbuild/bazel-gazelle/blob/master/extend.md.
package gazelle

import (
	"cmp"
	"flag"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// NewLanguage returns a Gazelle language object for Emacs Lisp.
//
// The returned [language.Language] also implements
// [language.ModuleAwareLanguage].
func NewLanguage() language.Language {
	return elisp{}
}

const languageName = "elisp"

var _ language.ModuleAwareLanguage = elisp{}

type elisp struct{}

func (elisp) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {}

func (elisp) CheckFlags(fs *flag.FlagSet, c *config.Config) error { return nil }

func (elisp) KnownDirectives() []string { return nil }

func (elisp) Configure(c *config.Config, rel string, f *rule.File) {
	// We always rely on the imports index to write deps attributes.
	c.IndexLibraries = true
}

func (elisp) Name() string { return languageName }

func (elisp) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (elisp) Kinds() map[string]rule.KindInfo {
	return map[string]rule.KindInfo{
		libraryKind: {
			NonEmptyAttrs: map[string]bool{
				"srcs":      true,
				"outs":      true,
				"deps":      true,
				"load_path": true,
			},
			MergeableAttrs: map[string]bool{"srcs": true},
			ResolveAttrs:   map[string]bool{"deps": true},
		},
		protoLibraryKind: {
			NonEmptyAttrs: map[string]bool{"deps": true},
		},
		binaryKind: {
			NonEmptyAttrs: map[string]bool{
				"src":  true,
				"deps": true,
			},
			ResolveAttrs: map[string]bool{"deps": true},
		},
		testKind: {
			NonEmptyAttrs: map[string]bool{
				"srcs": true,
				"deps": true,
			},
			MergeableAttrs: map[string]bool{"srcs": true},
			ResolveAttrs:   map[string]bool{"deps": true},
		},
		manualKind: {
			NonEmptyAttrs: map[string]bool{"src": true},
		},
	}
}

func (elisp) Loads() []rule.LoadInfo {
	return loads(moduleName)
}

func (e elisp) ApparentLoads(moduleToApparentName func(string) string) []rule.LoadInfo {
	return loads(cmp.Or(moduleToApparentName(moduleName), moduleName))
}

func loads(repo string) []rule.LoadInfo {
	return []rule.LoadInfo{
		load(libraryKind, repo, "elisp"),
		load(protoLibraryKind, repo, "elisp/proto"),
		load(binaryKind, repo, "elisp"),
		load(testKind, repo, "elisp"),
		load(manualKind, repo, "elisp"),
	}
}

func load(kind, repo, pkg string) rule.LoadInfo {
	return rule.LoadInfo{
		Name:    label.New(repo, pkg, kind+".bzl").String(),
		Symbols: []string{kind},
	}
}

func (elisp) Fix(c *config.Config, f *rule.File) {}

const (
	libraryKind      = "elisp_library"
	protoLibraryKind = "elisp_proto_library"
	binaryKind       = "elisp_binary"
	testKind         = "elisp_test"
	manualKind       = "elisp_manual"
)

const moduleName = "phst_rules_elisp"
