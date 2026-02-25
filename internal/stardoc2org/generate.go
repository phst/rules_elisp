// Copyright 2021-2026 Google LLC
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

// Helper binary to convert Stardoc output into Org Mode format.
package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net/url"
	"os"
	"regexp"
	"runtime/debug"
	"strings"
	"text/template"
	"unicode"
	"unicode/utf8"

	_ "embed"

	"github.com/yuin/goldmark/v2/ast"
	"github.com/yuin/goldmark/v2/parser"
	"github.com/yuin/goldmark/v2/renderer"
	"golang.org/x/text/cases"
	"golang.org/x/text/language"
	"google.golang.org/protobuf/proto"

	spb "github.com/phst/rules_elisp/internal/stardoc2org/stardoc_output_go_proto"
)

// Main function.
func main() {
	flag.Usage = usage
	flag.Parse()
	if flag.NArg() != 2 {
		usage()
		os.Exit(2)
	}
	input := flag.Arg(0)
	output := flag.Arg(1)
	b, err := os.ReadFile(input)
	if err != nil {
		log.Fatal(err)
	}
	var module spb.ModuleInfo
	if err := proto.Unmarshal(b, &module); err != nil {
		log.Fatal(err)
	}
	file, err := os.OpenFile(output, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0400)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	generator := newGenerator(file)
	if err := generator.run(&module); err != nil {
		log.Fatal(err)
	}
	if err := file.Sync(); err != nil {
		log.Fatal(err)
	}
	if err := file.Close(); err != nil {
		log.Fatal(err)
	}
}

func usage() {
	fmt.Fprintln(os.Stderr, "Usage: generate INPUT OUTPUT")
	flag.PrintDefaults()
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

func newGenerator(file io.Writer) *generator {
	return &generator{file}
}

// Writes the generated Org Mode output.
func (g *generator) run(module *spb.ModuleInfo) (err error) {
	defer runRecover(&err)
	g.doRun(module)
	return
}

func runRecover(err *error) {
	switch x := recover().(type) {
	case nil:
		return
	case error:
		*err = fmt.Errorf("Generator panic: %w", x)
	default:
		*err = fmt.Errorf("Generator panic: %#v", x)
	}
	debug.PrintStack()
}

func (g *generator) doRun(module *spb.ModuleInfo) {
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
	if err := tpl.Execute(g.file, module); err != nil {
		panic(err)
	}
}

//go:embed reference.org.template
var templateText string

// Convert a Markdown snippet to Org-mode.
func markdown(text string) string {
	text = strings.TrimSpace(text)
	if text == "" {
		panic(errors.New("Missing docstring"))
	}
	source := []byte(text)
	doc := parser.New().Parse(source)
	orgRenderer := newOrgRenderer()
	type config struct {
		Config renderer.Config[*strings.Builder, config]
	}
	renderer := new(renderer.HelperBuilder[*strings.Builder, config]).Options(renderer.WithNodeRenderers[*strings.Builder, config](orgRenderer.nodeRenderers())).Build()
	var w strings.Builder
	if err := renderer.Render(&w, source, doc); err != nil {
		panic(err)
	}
	return w.String() + "\n"
}

func fill(text, initialIndent, subsequentIndent string) string {
	text = regexp.MustCompile(`([.!?])\n+`).ReplaceAllString(text, "$1  ")
	text = strings.ReplaceAll(text, "\t", "    ")
	words := regexp.MustCompile(`[ \n]`).Split(text, -1)
	var b strings.Builder
	b.WriteString(initialIndent)
	i := 0
	width := 80 - len(initialIndent)
	for _, w := range words {
		l := utf8.RuneCountInString(w)
		if i+l >= width {
			b.WriteByte('\n')
			b.WriteString(subsequentIndent)
			i = 0
		} else if i > 0 {
			b.WriteByte(' ')
			i++
		}
		b.WriteString(w)
		i += l
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

type orgRenderer struct {
	indent  string
	lastOut string
}

var rendererLanguage = map[string]string{
	"sh": "sh",
	"c":  "c",
}

func newOrgRenderer() *orgRenderer {
	return &orgRenderer{"", ""}
}

func (r *orgRenderer) nodeRenderers() map[ast.NodeKind]renderer.NodeRenderer[*strings.Builder] {
	return map[ast.NodeKind]renderer.NodeRenderer[*strings.Builder]{
		ast.KindDocument:  renderer.NodeRendererFunc(r.document),
		ast.KindText:      renderer.NodeRendererFunc(r.text),
		ast.KindParagraph: renderer.NodeRendererFunc(r.paragraph),
		ast.KindList:      renderer.NodeRendererFunc(r.list),
		ast.KindListItem:  renderer.NodeRendererFunc(r.item),
		ast.KindEmphasis:  renderer.NodeRendererFunc(r.emph),
		ast.KindCodeSpan:  renderer.NodeRendererFunc(r.code),
		ast.KindCodeBlock: renderer.NodeRendererFunc(r.codeBlock),
		ast.KindLink:      renderer.NodeRendererFunc(r.link),
		ast.KindRawHTML:   renderer.NodeRendererFunc(r.htmlInline),

		ast.KindAutoLink:                renderer.NodeRendererFunc(r.unknown),
		ast.KindBlockquote:              renderer.NodeRendererFunc(r.unknown),
		ast.KindHTMLBlock:               renderer.NodeRendererFunc(r.unknown),
		ast.KindHeading:                 renderer.NodeRendererFunc(r.unknown),
		ast.KindImage:                   renderer.NodeRendererFunc(r.unknown),
		ast.KindLinkReferenceDefinition: renderer.NodeRendererFunc(r.unknown),
		ast.KindStrong:                  renderer.NodeRendererFunc(r.unknown),
		ast.KindThematicBreak:           renderer.NodeRendererFunc(r.unknown),
	}
}

func (r *orgRenderer) lit(w *strings.Builder, s string) {
	if _, err := w.WriteString(s); err != nil {
		panic(err)
	}
	r.lastOut = s
}

func (r *orgRenderer) cr(w *strings.Builder) {
	if r.lastOut != "\n" {
		r.lit(w, "\n")
	}
}

func (r *orgRenderer) out(w *strings.Builder, s string) {
	indent := ""
	if strings.HasSuffix(r.lastOut, "\n") {
		indent = r.indent
	}
	r.lit(w, indent+escape(s))
}

func escape(s string) string {
	// See https://orgmode.org/manual/Escape-Character.html.
	return regexp.MustCompile(`([\[\]*/_=~+])`).ReplaceAllString(s, "$1\u200B")
}

func (r *orgRenderer) document(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.Document)
	r.cr(writer)
	return ast.WalkContinue, nil
}

func (r *orgRenderer) text(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.Text)
	if entering {
		s := node.Value.Str(source)
		s = regexp.MustCompile(`\\(.)`).ReplaceAllString(s, "$1")
		r.out(writer, s)
		if node.SoftLineBreak() {
			r.softBreak(writer)
		}
		if node.HardLineBreak() {
			r.lineBreak(writer)
		}
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) softBreak(writer *strings.Builder) {
	r.cr(writer)
}

func (r *orgRenderer) lineBreak(writer *strings.Builder) {
	r.lit(writer, `\\`)
	r.cr(writer)
}

func (r *orgRenderer) paragraph(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.Paragraph)
	if node.Parent().Kind() != ast.KindListItem {
		r.lit(writer, "\n")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) list(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.List)
	if entering {
		r.cr(writer)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) item(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.ListItem)
	if entering {
		if r.indent != "" {
			return ast.WalkStop, errors.New("no support for nested lists")
		}
		r.lit(writer, "- ")
		r.indent = "  "
	} else {
		if r.indent != "  " {
			return ast.WalkStop, errors.New("no support for nested lists")
		}
		r.indent = ""
		r.cr(writer)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) emph(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.Emphasis)
	r.lit(writer, "/")
	return ast.WalkContinue, nil
}

func (r *orgRenderer) code(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.CodeSpan)
	r.lit(writer, "~")
	if entering {
		r.lit(writer, node.Value.Str(source))
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) codeBlock(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.CodeBlock)
	if entering {
		lang, ok := node.Language(source)
		if !ok {
			return ast.WalkStop, errors.New("language not given")
		}
		lang = rendererLanguage[lang]
		if lang == "" {
			return ast.WalkStop, fmt.Errorf("unknown language %q", lang)
		}
		r.lit(writer, fmt.Sprintf("#+BEGIN_SRC %s\n", lang))
		r.lit(writer, node.Value.Str(source))
	} else {
		r.lit(writer, "#+END_SRC\n\n")
		r.lit(writer, "#+TEXINFO: @noindent")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) link(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.Link)
	if entering {
		dest := node.Destination.Str(source)
		// CommonMark helpfully URL-escapes link destinations, but this
		// prevents links to Info nodes containing spaces.
		match := regexp.MustCompile(`^(info:[^#:]+[#:])(.*%.*)$`).FindStringSubmatch(dest)
		if match != nil {
			s, err := url.PathUnescape(match[2])
			if err != nil {
				return ast.WalkStop, err
			}
			dest = match[1] + s
		}
		r.lit(writer, fmt.Sprintf("[[%s][", dest))
	} else {
		r.lit(writer, "]]")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) htmlInline(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.RawHTML)
	if entering {
		tag := node.Value.Str(source)
		org := tags[tag]
		if org == "" {
			return ast.WalkStop, fmt.Errorf("unknown HTML tag %s", tag)
		}
		r.lit(writer, org)
	}
	return ast.WalkContinue, nil
}

// Signal an error if we don’t implement something.
func (r *orgRenderer) unknown(writer *strings.Builder, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	return ast.WalkStop, fmt.Errorf("unknown node type %q", n.Kind())
}

var tags = map[string]string{
	"<code>":  "@@texinfo:@code{@@",
	"</code>": "@@texinfo:}@@",
	"<var>":   "@@texinfo:@var{@@",
	"</var>":  "@@texinfo:}@@",
}
