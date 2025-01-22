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
// maintains elisp_library, elisp_binary, and elisp_test rules from the
// phst_rules_elisp repository.  See https://github.com/phst/rules_elisp and
// https://github.com/bazelbuild/bazel-gazelle/blob/master/extend.md.
package gazelle

import (
	"flag"
	"log"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// NewLanguage returns a Gazelle language object for Emacs Lisp.
func NewLanguage() language.Language {
	return elisp{}
}

const languageName = "elisp"

type elisp struct{}

func (elisp) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {}

func (elisp) CheckFlags(fs *flag.FlagSet, c *config.Config) error { return nil }

func (elisp) KnownDirectives() []string {
	var r []string
	for k := range directives {
		r = append(r, k)
	}
	return r
}

func (elisp) Configure(c *config.Config, rel string, f *rule.File) {
	// We always rely on the imports index to write deps attributes.
	c.IndexLibraries = true

	ext := initExtension(c)

	if f != nil {
		for _, dir := range f.Directives {
			fun, ok := directives[dir.Key]
			if !ok {
				continue
			}
			fun(f, dir, ext)
		}
	}
}

func (elisp) Name() string { return languageName }

func (elisp) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (elisp) Kinds() map[string]rule.KindInfo {
	return map[string]rule.KindInfo{
		libraryKind: {
			NonEmptyAttrs: map[string]bool{
				"src":       true,
				"deps":      true,
				"load_path": true,
			},
			ResolveAttrs: map[string]bool{"deps": true},
		},
		binaryKind: {
			NonEmptyAttrs: map[string]bool{
				"srcs": true,
				"deps": true,
			},
			MergeableAttrs: map[string]bool{"srcs": true},
			ResolveAttrs:   map[string]bool{"deps": true},
		},
		testKind: {
			NonEmptyAttrs: map[string]bool{
				"srcs": true,
				"deps": true,
			},
			MergeableAttrs: map[string]bool{"srcs": true},
			ResolveAttrs:   map[string]bool{"deps": true},
		},
	}
}

func (elisp) Loads() []rule.LoadInfo {
	return []rule.LoadInfo{{
		Name:    "@phst_rules_elisp//elisp:defs.bzl",
		Symbols: []string{libraryKind, binaryKind, testKind},
	}}
}

func (elisp) Fix(c *config.Config, f *rule.File) {}

var directives = map[string]func(*rule.File, rule.Directive, *extension){
	"elisp_provide": processProvide,
}

func processProvide(f *rule.File, dir rule.Directive, ext *extension) {
	before, after, ok := strings.Cut(dir.Value, "=")
	if !ok || before == "" || after == "" {
		log.Printf("%s: invalid directive %s %s", f.Path, dir.Key, dir.Value)
		return
	}
	lbl, err := label.Parse(before)
	if err != nil {
		log.Printf("%s: invalid label %q in directive: %s", f.Path, before, err)
		return
	}
	ext.addProvider(lbl.Abs("", f.Pkg), Feature(after))
}

const (
	libraryKind = "elisp_library"
	binaryKind  = "elisp_binary"
	testKind    = "elisp_test"
)
