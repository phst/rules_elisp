// Copyright 2021-2026 Google LLC
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

// Package stardoc contains functions to render Starlark documentation.
package stardoc

import (
	"errors"
	"fmt"
	"io"
	"strings"
	"text/template"
	"unicode"

	_ "embed"

	"github.com/yuin/goldmark/v2/parser"
	"golang.org/x/text/cases"
	"golang.org/x/text/language"

	"github.com/phst/rules_elisp/internal/mdorg"
	spb "github.com/phst/rules_elisp/internal/stardoc_output_go_proto"
)

// Org renders documentation for the given Starlark module into the writer as
// Org-mode document.
func Org(module *spb.ModuleInfo, w io.Writer) error {
	generator := &generator{w}
	return generator.run(module)
}

type generator struct {
	file io.Writer
}

var attributeType = map[spb.AttributeType]string{
	spb.AttributeType_NAME:              "Name",
	spb.AttributeType_INT:               "Integer",
	spb.AttributeType_LABEL:             "Label",
	spb.AttributeType_STRING:            "String",
	spb.AttributeType_STRING_LIST:       "List of strings",
	spb.AttributeType_INT_LIST:          "List of integers",
	spb.AttributeType_LABEL_LIST:        "List of labels",
	spb.AttributeType_BOOLEAN:           "Boolean",
	spb.AttributeType_LABEL_STRING_DICT: "Dictionary string → label",
	spb.AttributeType_STRING_DICT:       "Dictionary string → string",
	spb.AttributeType_STRING_LIST_DICT:  "Dictionary string → list of strings",
	spb.AttributeType_OUTPUT:            "Output file",
	spb.AttributeType_OUTPUT_LIST:       "List of output files",
}

func formatAttributeType(t spb.AttributeType) (string, error) {
	s, ok := attributeType[t]
	if !ok {
		return "", fmt.Errorf("unknown attribute type %s", t)
	}
	return s, nil
}

var mandatory = map[bool]string{
	false: "optional",
	true:  "mandatory",
}

func formatMandatory(b bool) string {
	return mandatory[b]
}

// Writes the generated Org Mode output.
func (g *generator) run(module *spb.ModuleInfo) error {
	return g.doRun(module)
}

func (g *generator) doRun(module *spb.ModuleInfo) error {
	tpl := template.New("reference.org.template")
	item := func(template string, data any) (string, error) {
		var w strings.Builder
		err := tpl.ExecuteTemplate(&w, template, data)
		if err != nil {
			return "", err
		}
		s := w.String()
		if s == "" {
			return "", nil
		}
		return fill(s, "- ", "  ") + "\n", nil
	}
	funcs := template.FuncMap{
		"containsRune":    strings.ContainsRune,
		"hasPrefix":       strings.HasPrefix,
		"trimSpace":       strings.TrimSpace,
		"lstrip":          lstrip,
		"capitalize":      capitalize,
		"mandatory":       formatMandatory,
		"attributeType":   formatAttributeType,
		"requireEmpty":    requireEmpty,
		"requireNonEmpty": requireNonEmpty,
		"requirePeriod":   requirePeriod,
		"exactlyOne":      exactlyOne,
		"markdown":        markdown,
		"item":            item,
	}
	tpl = template.Must(tpl.Funcs(funcs).Parse(templateText))
	return tpl.Execute(g.file, module)
}

//go:embed reference.org.template
var templateText string

// Convert a Markdown snippet to Org-mode.
func markdown(text string) (string, error) {
	text = strings.TrimSpace(text)
	if text == "" {
		return "", errors.New("Missing docstring")
	}
	source := []byte(text)
	doc := parser.New().Parse(source)
	renderer := mdorg.NewRenderer()
	var w strings.Builder
	if err := renderer.Render(&w, source, doc); err != nil {
		return "", err
	}
	return w.String() + "\n", nil
}

func fill(text, initialIndent, subsequentIndent string) string {
	var b strings.Builder
	b.WriteString(initialIndent)
	first := true
	for line := range strings.Lines(text) {
		if !first && line != "\n" {
			b.WriteString(subsequentIndent)
		}
		first = false
		b.WriteString(line)
	}
	return b.String()
}

func lstrip(s string) string {
	return strings.TrimLeftFunc(s, unicode.IsSpace)
}

func capitalize(s string) string {
	return cases.Title(language.English, cases.NoLower).String(s)
}

func requireEmpty(s string) (string, error) {
	if s != "" {
		return "", fmt.Errorf("string %q isn’t empty", s)
	}
	return "", nil
}

func requireNonEmpty(s string) (string, error) {
	if s == "" {
		return s, errors.New("empty string")
	}
	return s, nil
}

func requirePeriod(s string) (string, error) {
	if !strings.HasSuffix(s, ".") {
		return "", fmt.Errorf("documentation string %q should end with a period", s)
	}
	return s, nil
}

func exactlyOne(groups []*spb.ProviderNameGroup) (*spb.ProviderNameGroup, error) {
	if n := len(groups); n != 1 {
		return nil, fmt.Errorf("got %d provider name groups, want one", n)
	}
	return groups[0], nil
}
