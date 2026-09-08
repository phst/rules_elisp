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

package mdorg_test

import (
	"os"
	"strings"
	"testing"

	_ "embed"

	"github.com/google/go-cmp/cmp"
	"github.com/yuin/goldmark/v2/parser"

	"github.com/phst/rules_elisp/internal/mdorg"
)

func TestNewRenderer(t *testing.T) {
	mdorg.NewRenderer()
}

func TestRenderer_Render(t *testing.T) {
	doc := parser.New().Parse(input)
	var b strings.Builder
	if err := mdorg.NewRenderer().Render(&b, input, doc); err != nil {
		t.Fatal(err)
	}
	if diff := cmp.Diff(b.String(), output); diff != "" {
		t.Errorf("-got +want:\n%s", diff)
	}
}

//go:embed testdata/input.md
var input []byte

//go:embed testdata/output.org
var output string

func ExampleRenderer_RenderStringSource() {
	const source = "Hello `world`!"
	doc := parser.New().ParseStringSource(source)
	mdorg.NewRenderer().RenderStringSource(os.Stdout, source, doc)
	// Output:
	// Hello ~world~!
}
