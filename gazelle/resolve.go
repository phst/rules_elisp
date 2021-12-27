// Copyright 2021, 2022 Google LLC
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
	"log"
	"sort"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
)

// Resolve implements Resolver.Resolve.  It adds a deps attribute to the given
// rule.  imports should be an Imports object, and ix should contain mappings
// for the features in imports.
func (elisp) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	imp, ok := imports.(Imports)
	if !ok {
		return
	}
	var deps []string
	for _, feat := range imp.Requires {
		spec := feat.importSpec()
		res := ix.FindRulesByImportWithConfig(c, spec, languageName)
		if len(res) == 0 {
			log.Printf("%s: no rule for required feature %s found", from, feat)
			continue
		}
		if len(res) > 1 {
			log.Printf("%s: %d rules for required feature %s found", from, len(res), feat)
		}
		// Make the label relative to the current package if possible.
		lbl := res[0].Label.Rel(from.Repo, from.Pkg)
		deps = append(deps, lbl.String())
	}
	if len(deps) > 0 {
		sort.Strings(deps)
		r.SetAttr("deps", deps)
	} else {
		r.DelAttr("deps")
	}
}
