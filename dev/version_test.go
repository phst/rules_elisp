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
	"strings"
	"testing"

	_ "embed"

	"github.com/bazelbuild/buildtools/build"
)

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
		if !dev {
			t.Errorf("production dependency %s is overridden using %s", name, kind)
		}
	}
}

//go:embed MODULE.bazel
var moduleContent []byte
