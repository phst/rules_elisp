// Copyright 2026 Philipp Stephani
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

package stardoc_test

import (
	"os"

	"github.com/phst/rules_elisp/internal/stardoc"
	spb "github.com/phst/rules_elisp/internal/stardoc_output_go_proto"
)

func ExampleOrg() {
	mod := &spb.ModuleInfo{RuleInfo: []*spb.RuleInfo{{
		RuleName:  "myrule",
		DocString: "An example rule",
		OriginKey: &spb.OriginKey{Name: "myrule", File: "myrule.bzl"},
	}}}
	stardoc.Org(mod, os.Stdout)
	// Output:
	// #+ATTR_TEXINFO: :options Rule myrule ()
	// #+BEGIN_deffn
	//
	// #+BEGIN_SRC bazel-starlark
	// load("myrule.bzl", "myrule")
	// #+END_SRC
	//
	// An example rule
	//
	// #+END_deffn
}
