// Copyright 2021-2025 Google LLC
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
//
// To suppress generation of elisp_proto_library rules, add a Gazelle directive
//
//	# gazelle:elisp_generate_proto false
//
// to your BUILD file.
package gazelle

import (
	"cmp"
	"flag"
	"log"
	"strconv"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// NewLanguage returns a Gazelle language object for Emacs Lisp.  The name of
// the language is “elisp”.
//
// The returned [language.Language] also implements
// [language.ModuleAwareLanguage].  The [language.Language.GenerateRules] method
// populates the [language.GenerateResult.Imports] slice with [Imports] objects.
func NewLanguage() language.Language {
	return elisp{}
}

var _ language.ModuleAwareLanguage = elisp{}

type elisp struct{}

func (elisp) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {
	initExtension(c)
}

func (elisp) CheckFlags(fs *flag.FlagSet, c *config.Config) error { return nil }

func (elisp) KnownDirectives() []string { return []string{"elisp_generate_proto"} }

func (elisp) Configure(c *config.Config, rel string, f *rule.File) {
	// We always rely on the imports index to write deps attributes.
	c.IndexLibraries = true

	ext := initExtension(c)

	if f != nil {
		for _, d := range f.Directives {
			switch d.Key {
			case "elisp_generate_proto":
				b, err := strconv.ParseBool(d.Value)
				if err != nil {
					log.Printf("%s: invalid value for %s directive: %s", f.Path, d.Key, err)
					break
				}
				ext.generateProto = b
			}
		}
	}
}

func (elisp) Name() string { return "elisp" }

func (elisp) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (elisp) Kinds() map[string]rule.KindInfo {
	return map[string]rule.KindInfo{
		"elisp_library": {
			NonEmptyAttrs: map[string]bool{
				"srcs":      true,
				"outs":      true,
				"deps":      true,
				"load_path": true,
			},
			MergeableAttrs: map[string]bool{"srcs": true},
			ResolveAttrs:   map[string]bool{"deps": true},
		},
		"elisp_proto_library": {
			NonEmptyAttrs: map[string]bool{"deps": true},
		},
		"elisp_binary": {
			NonEmptyAttrs: map[string]bool{
				"src":  true,
				"deps": true,
			},
			ResolveAttrs: map[string]bool{"deps": true},
		},
		"elisp_test": {
			NonEmptyAttrs: map[string]bool{
				"srcs": true,
				"deps": true,
			},
			MergeableAttrs: map[string]bool{"srcs": true},
			ResolveAttrs:   map[string]bool{"deps": true},
		},
		"elisp_manual": {
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
		load("elisp_library", repo, "elisp"),
		load("elisp_proto_library", repo, "elisp/proto"),
		load("elisp_binary", repo, "elisp"),
		load("elisp_test", repo, "elisp"),
		load("elisp_manual", repo, "elisp"),
	}
}

func load(kind, repo, pkg string) rule.LoadInfo {
	return rule.LoadInfo{
		Name:    label.New(repo, pkg, kind+".bzl").String(),
		Symbols: []string{kind},
	}
}

func (elisp) Fix(c *config.Config, f *rule.File) {}

const moduleName = "phst_rules_elisp"
