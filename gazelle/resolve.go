// Copyright 2021, 2022, 2024, 2025 Google LLC
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
	"cmp"
	"log"
	"slices"
	"sort"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// Resolve implements [resolve.Resolver.Resolve].  It adds a deps attribute to
// the given rule.  imports should be an [Imports] object, and ix should contain
// mappings for the features in imports.
func (elisp) Resolve(
	c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache,
	r *rule.Rule, imports interface{}, from label.Label,
) {
	imp, ok := imports.(Imports)
	if !ok {
		return
	}
	var deps []string
	for _, feat := range imp.Requires {
		lbl := resolveFeature(c, ix, from, feat)
		if lbl == nil {
			log.Printf("%s: no rule for required feature %s found", from, feat)
			continue
		}
		// Make the label relative to the current package if possible.
		deps = append(deps, lbl.Rel(from.Repo, from.Pkg).String())
	}
	if len(deps) > 0 {
		sort.Strings(deps)
		r.SetAttr("deps", deps)
	} else {
		r.DelAttr("deps")
	}
}

func resolveFeature(c *config.Config, ix *resolve.RuleIndex, from label.Label, feat Feature) *label.Label {
	if lbl := getExtension(c).provider(feat); lbl != nil {
		return lbl
	}

	spec := feat.importSpec()
	res := ix.FindRulesByImportWithConfig(c, spec, languageName)
	res = slices.DeleteFunc(res, func(r resolve.FindResult) bool {
		s := r.IsSelfImport(from)
		if s {
			log.Printf("%s: ignoring self-import %s", from, r.Label)
		}
		return s
	})
	if len(res) == 0 {
		return nil
	}
	if len(res) > 1 {
		log.Printf("%s: %d rules for required feature %s found", from, len(res), feat)
	}
	closest := slices.MinFunc(res, func(a, b resolve.FindResult) int {
		return cmp.Compare(distance(a.Label, from), distance(b.Label, from))
	})
	return &closest.Label
}

func distance(a, b label.Label) int {
	if a.Repo != b.Repo {
		return 2
	}
	if a.Pkg != b.Pkg {
		return 1
	}
	return 0
}
