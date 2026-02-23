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
	"unicode"
	"unicode/utf8"

	"github.com/yuin/goldmark"
	"github.com/yuin/goldmark/ast"
	"github.com/yuin/goldmark/renderer"
	"github.com/yuin/goldmark/util"
	"golang.org/x/text/cases"
	"golang.org/x/text/encoding/charmap"
	"golang.org/x/text/language"
	"google.golang.org/protobuf/proto"

	spb "github.com/phst/rules_elisp/docs/stardoc_output_go_proto"
)

// Main function.
func main() {
	flag.Usage = usage
	var utf8 bool
	flag.BoolVar(&utf8, "utf8", false, "")
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
	generator := newGenerator(file, utf8)
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
	utf8 bool
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

var mandatory = map[bool]string{
	false: "optional",
	true:  "mandatory",
}

func newGenerator(file io.Writer, utf8 bool) *generator {
	return &generator{file, utf8}
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
	doc := module.GetModuleDocstring()
	if len(doc) > 100 && strings.ContainsRune(doc, '\n') {
		g.write(g.markdown(doc))
	}
	for _, rule := range module.GetRuleInfo() {
		g.rule(rule)
	}
	for _, provider := range module.GetProviderInfo() {
		g.provider(provider)
	}
	for _, function := range module.GetFuncInfo() {
		g.function(function)
	}
	for _, aspect := range module.GetAspectInfo() {
		g.aspect(aspect)
	}
	for _, extension := range module.GetModuleExtensionInfo() {
		g.extension(extension)
	}
	for _, rule := range module.GetRepositoryRuleInfo() {
		g.repoRule(rule)
	}
	for _, macro := range module.GetMacroInfo() {
		g.macro(macro)
	}
}

func (g *generator) rule(rule *spb.RuleInfo) {
	name := rule.GetRuleName()
	var elts []string
	for _, a := range rule.GetAttribute() {
		var s string
		if a.GetMandatory() {
			s = a.GetName()
		} else {
			s = fmt.Sprintf("[%s]", a.GetName())
		}
		elts = append(elts, s)
	}
	attrs := strings.Join(elts, ", ")
	g.write(fmt.Sprintf("#+ATTR_TEXINFO: :options Rule %s (%s)\n", name, attrs))
	g.write("#+BEGIN_deffn\n")
	g.load(rule.GetOriginKey())
	g.write(lstrip(g.markdown(rule.GetDocString())))
	g.write(fmt.Sprintf("The ~%s~ rule supports the following attributes:\n\n", name))
	for _, attr := range rule.GetAttribute() {
		g.attribute(attr)
	}
	g.write("#+END_deffn\n\n")
}

func (g *generator) function(function *spb.StarlarkFunctionInfo) {
	name := function.GetFunctionName()
	var elts []string
	for _, p := range function.GetParameter() {
		var s string
		if p.GetMandatory() {
			s = p.GetName()
		} else {
			s = fmt.Sprintf("[%s]", p.GetName())
		}
		elts = append(elts, s)
	}
	params := strings.Join(elts, ", ")
	g.write(fmt.Sprintf("#+ATTR_TEXINFO: :options %s (%s)\n", name, params))
	g.write("#+BEGIN_defun\n")
	g.load(function.GetOriginKey())
	g.write(lstrip(g.markdown(function.GetDocString())))
	for _, param := range function.GetParameter() {
		g.parameter(param)
	}
	returns := function.GetReturn().GetDocString()
	if returns != "" {
		g.write(fmt.Sprintf("Returns: %s\n\n", g.markdown(returns)))
	}
	if function.GetDeprecated().GetDocString() != "" {
		panic(fmt.Errorf("unsupported deprecated function %s", name))
	}
	g.write("#+END_defun\n\n")
}

func (g *generator) parameter(param *spb.FunctionParamInfo) {
	doc := strings.TrimSpace(g.markdown(param.GetDocString()))
	if !strings.HasSuffix(doc, ".") {
		panic(
			fmt.Sprintf("documentation string %q should end with a period", doc))
	}
	suffixes := []string{capitalize(mandatory[param.GetMandatory()])}
	if param.GetDefaultValue() != "" {
		suffixes = append(suffixes, fmt.Sprintf("default: ~%s~", param.GetDefaultValue()))
	}
	suffix := strings.Join(suffixes, "; ")
	g.item(fmt.Sprintf("%s :: %s  %s.", param.GetName(), doc, suffix))
}

func (g *generator) provider(provider *spb.ProviderInfo) {
	name := provider.GetProviderName()
	var elts []string
	for _, f := range provider.GetFieldInfo() {
		elts = append(elts, f.GetName())
	}
	fields := strings.Join(elts, " ")
	g.write(fmt.Sprintf("#+ATTR_TEXINFO: :options Provider %s %s\n", name, fields))
	g.write("#+BEGIN_deftp\n")
	g.load(provider.GetOriginKey())
	g.write(lstrip(g.markdown(provider.GetDocString())))
	g.write(fmt.Sprintf("The ~%s~ provider has the following fields:\n\n", name))
	for _, field := range provider.GetFieldInfo() {
		doc := strings.TrimSpace(g.markdown(field.GetDocString()))
		if !strings.HasSuffix(doc, ".") {
			panic(
				fmt.Errorf("documentation string %q should end with a period", doc))
		}
		g.item(fmt.Sprintf("~%s~ :: %s", field.GetName(), doc))
	}
	g.write("#+END_deftp\n\n")
}

func (g *generator) aspect(aspect *spb.AspectInfo) {
	name := aspect.GetAspectName()
	var elts []string
	for _, a := range aspect.GetAttribute() {
		var s string
		if a.GetMandatory() {
			s = a.GetName()
		} else {
			s = fmt.Sprintf("[%s]", a.GetName())
		}
		elts = append(elts, s)
	}
	attrs := strings.Join(elts, " ")
	g.write(fmt.Sprintf("#+ATTR_TEXINFO: :options Aspect %s %s\n", name, attrs))
	g.write("#+BEGIN_deffn\n")
	g.load(aspect.GetOriginKey())
	g.write(lstrip(g.markdown(aspect.GetDocString())))
	if len(aspect.GetAspectAttribute()) != 0 {
		var elts []string
		for _, a := range aspect.GetAspectAttribute() {
			elts = append(elts, fmt.Sprintf("~%s~", a))
		}
		attrs := strings.Join(elts, ", ")
		g.write(fmt.Sprintf("This aspect propagates along the following attributes: %s\n", attrs))
	}
	for _, attr := range aspect.GetAttribute() {
		g.attribute(attr)
	}
	g.write("#+END_deffn\n\n")
}

func (g *generator) extension(ext *spb.ModuleExtensionInfo) {
	name := ext.GetExtensionName()
	var elts []string
	for _, t := range ext.GetTagClass() {
		elts = append(elts, t.GetTagName())
	}
	tags := strings.Join(elts, " ")
	g.write(
		fmt.Sprintf("#+ATTR_TEXINFO: :options {Module extension} %s %s\n", name, tags))
	g.write("#+BEGIN_deftp\n\n")
	g.write("#+BEGIN_SRC bazel-module\n")
	g.write(
		fmt.Sprintf("%s = use_extension(\"%s\", \"%s\")\n", name, ext.GetOriginKey().GetFile(), name))
	g.write("#+END_SRC\n\n")
	g.write(lstrip(g.markdown(ext.GetDocString())))
	g.write(fmt.Sprintf("The ~%s~ module extension provides the following tag classes:\n\n", name))
	for _, tag := range ext.GetTagClass() {
		g.tagClass(name, tag)
	}
	g.write("#+END_deftp\n\n")
}

func (g *generator) tagClass(extensionName string,
	tag *spb.ModuleExtensionTagClassInfo) {
	name := tag.GetTagName()
	var elts []string
	for _, a := range tag.GetAttribute() {
		var s string
		if a.GetMandatory() {
			s = a.GetName()
		} else {
			s = fmt.Sprintf("[%s]", a.GetName())
		}
		elts = append(elts, s)
	}
	attrs := strings.Join(elts, ", ")
	g.write(fmt.Sprintf("#+ATTR_TEXINFO: :options {Tag class} %s %s (%s)\n", extensionName, name, attrs))
	g.write("#+BEGIN_defop\n")
	g.write(lstrip(g.markdown(tag.GetDocString())))
	g.write(
		fmt.Sprintf("The ~%s~ tag class supports the following attributes:\n\n", name))
	for _, attr := range tag.GetAttribute() {
		g.attribute(attr)
	}
	g.write("#+END_defop\n\n")
}

func (g *generator) repoRule(rule *spb.RepositoryRuleInfo) {
	name := rule.GetRuleName()
	var elts []string
	for _, a := range rule.GetAttribute() {
		var s string
		if a.GetMandatory() {
			s = a.GetName()
		} else {
			s = fmt.Sprintf("[%s]", a.GetName())
		}
		elts = append(elts, s)
	}
	attrs := strings.Join(elts, ", ")
	g.write(
		fmt.Sprintf("#+ATTR_TEXINFO: :options {Repository rule} %s (%s)\n", name, attrs))
	g.write("#+BEGIN_deffn\n")
	g.load(rule.GetOriginKey())
	g.write(lstrip(g.markdown(rule.GetDocString())))
	g.write(fmt.Sprintf("The ~%s~ repository rule supports the following attributes:\n\n", name))
	for _, attr := range rule.GetAttribute() {
		g.attribute(attr)
	}
	if len(rule.GetEnviron()) != 0 {
		var elts []string
		for _, e := range rule.GetEnviron() {
			elts = append(elts, fmt.Sprintf("~%s~", e))
		}
		env := strings.Join(elts, ", ")
		g.write(fmt.Sprintf("It depends on the following environment variables: %s\n\n", env))
	}
	g.write("#+END_deffn\n\n")
}

func (g *generator) macro(macro *spb.MacroInfo) {
	name := macro.GetMacroName()
	var elts []string
	for _, a := range macro.GetAttribute() {
		var s string
		if a.GetMandatory() {
			s = a.GetName()
		} else {
			s = fmt.Sprintf("[%s]", a.GetName())
		}
		elts = append(elts, s)
	}
	attrs := strings.Join(elts, ", ")
	g.write(fmt.Sprintf("#+ATTR_TEXINFO: :options %s (%s)\n", name, attrs))
	g.write("#+BEGIN_defmac\n")
	g.load(macro.GetOriginKey())
	g.write(lstrip(g.markdown(macro.GetDocString())))
	g.write(
		fmt.Sprintf("The ~%s~ macro supports the following attributes:\n\n", name))
	for _, attr := range macro.GetAttribute() {
		g.attribute(attr)
	}
	g.write("#+END_defmac\n\n")
}

func (g *generator) load(key *spb.OriginKey) {
	if key.GetFile() == "" {
		panic(errors.New("unknown file"))
	}
	if key.GetName() == "" {
		panic(fmt.Errorf("unknown symbol name in file %s", key.GetFile()))
	}
	g.write("\n#+BEGIN_SRC bazel-starlark\n")
	g.write(fmt.Sprintf("load(\"%s\", \"%s\")\n", key.GetFile(), key.GetName()))
	g.write("#+END_SRC\n\n")
}

func (g *generator) attribute(attr *spb.AttributeInfo) {
	if strings.HasPrefix(attr.GetDocString(), "Deprecated;") {
		return
	}
	doc := strings.TrimSpace(g.markdown(attr.GetDocString()))
	if !strings.HasSuffix(doc, ".") {
		panic(
			fmt.Errorf("documentation string %q should end with a period", doc))
	}
	s := attributeType[attr.GetType()]
	if s == "" {
		panic(fmt.Errorf("unknown attribute type %s", attr.GetType()))
	}
	suffixes := []string{s, mandatory[attr.GetMandatory()]}
	if attr.GetDefaultValue() != "" {
		suffixes = append(suffixes, fmt.Sprintf("default: ~%s~", attr.GetDefaultValue()))
	}
	if len(attr.GetProviderNameGroup()) != 0 {
		if n := len(attr.GetProviderNameGroup()); n != 1 {
			panic(fmt.Errorf("got %d provider name groups, want one", n))
		}
		group := attr.GetProviderNameGroup()[0]
		var elts []string
		for _, name := range group.GetProviderName() {
			elts = append(elts, fmt.Sprintf("~%s~", name))
		}
		names := strings.Join(elts, ", ")
		suffixes = append(suffixes, fmt.Sprintf("required providers: %s", names))
	}
	suffix := strings.Join(suffixes, "; ")
	g.item(fmt.Sprintf("~%s~ :: %s  %s.", attr.GetName(), doc, suffix))
}

func (g *generator) item(text string) {
	g.write(
		fill(text, "- ", "  ") + "\n")
}

func (g *generator) write(text string) {
	if _, err := io.WriteString(g.file, text); err != nil {
		panic(err)
	}
}

// Convert a Markdown snippet to Org-mode.
func (g *generator) markdown(text string) string {
	text = strings.TrimSpace(text)
	if text == "" {
		panic("Missing docstring")
	}
	// Bazel before 8.1 (including Stardoc) interpreted all files as Latin-1,
	// cf. https://bazel.build/versions/8.0.0/concepts/build-files.  However,
	// our files all use UTF-8, leading to double encoding.  Reverse that
	// effect here.
	if !g.utf8 {
		s, err := charmap.ISO8859_1.NewEncoder().String(text)
		if err != nil {
			panic(err)
		}
		text = s
	}
	var w strings.Builder
	if err := goldmark.New(goldmark.WithRenderer(renderer.NewRenderer(renderer.WithNodeRenderers(util.Prioritized(newOrgRenderer(), 0))))).Convert([]byte(text), &w); err != nil {
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

var _ renderer.NodeRenderer = (*orgRenderer)(nil)

func (r *orgRenderer) RegisterFuncs(reg renderer.NodeRendererFuncRegisterer) {
	reg.Register(ast.KindDocument, r.document)
	reg.Register(ast.KindText, r.text)
	reg.Register(ast.KindParagraph, r.paragraph)
	reg.Register(ast.KindList, r.list)
	reg.Register(ast.KindListItem, r.item)
	reg.Register(ast.KindEmphasis, r.emph)
	reg.Register(ast.KindCodeSpan, r.code)
	reg.Register(ast.KindFencedCodeBlock, r.codeBlock)
	reg.Register(ast.KindLink, r.link)
	reg.Register(ast.KindRawHTML, r.htmlInline)

	reg.Register(ast.KindTextBlock, r.textBlock)

	reg.Register(ast.KindAutoLink, r.unknown)
	reg.Register(ast.KindBlockquote, r.unknown)
	reg.Register(ast.KindCodeBlock, r.unknown)
	reg.Register(ast.KindHTMLBlock, r.unknown)
	reg.Register(ast.KindHeading, r.unknown)
	reg.Register(ast.KindImage, r.unknown)
	reg.Register(ast.KindString, r.unknown)
	reg.Register(ast.KindThematicBreak, r.unknown)
}

func (r *orgRenderer) lit(w util.BufWriter, s string) {
	if _, err := w.WriteString(s); err != nil {
		panic(err)
	}
	r.lastOut = s
}

func (r *orgRenderer) cr(w util.BufWriter) {
	if r.lastOut != "\n" {
		r.lit(w, "\n")
	}
}

func (r *orgRenderer) out(w util.BufWriter, s string) {
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

func (r *orgRenderer) document(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	_ = n.(*ast.Document)
	r.cr(writer)
	return ast.WalkContinue, nil
}

func (r *orgRenderer) text(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	node := n.(*ast.Text)
	if entering {
		s := string(node.Value(source))
		if node.IsRaw() {
			r.lit(writer, s)
		} else {
			s := regexp.MustCompile(`\\(.)`).ReplaceAllString(s, "$1")
			r.out(writer, s)
		}
		if node.SoftLineBreak() {
			r.softBreak(writer)
		}
		if node.HardLineBreak() {
			r.lineBreak(writer)
		}
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) softBreak(writer util.BufWriter) {
	r.cr(writer)
}

func (r *orgRenderer) lineBreak(writer util.BufWriter) {
	r.lit(writer, `\\`)
	r.cr(writer)
}

func (r *orgRenderer) paragraph(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	node := n.(*ast.Paragraph)
	if node.Parent().Kind() != ast.KindListItem {
		r.lit(writer, "\n")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) list(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	_ = n.(*ast.List)
	if entering {
		r.cr(writer)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) item(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	_ = n.(*ast.ListItem)
	if entering {
		if r.indent != "" {
			panic(errors.New("no support for nested lists"))
		}
		r.lit(writer, "- ")
		r.indent = "  "
	} else {
		if r.indent != "  " {
			panic(errors.New("no support for nested lists"))
		}
		r.indent = ""
		r.cr(writer)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) emph(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	_ = n.(*ast.Emphasis)
	r.lit(writer, "/")
	return ast.WalkContinue, nil
}

func (r *orgRenderer) code(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	_ = n.(*ast.CodeSpan)
	r.lit(writer, "~")
	return ast.WalkContinue, nil
}

func (r *orgRenderer) codeBlock(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	node := n.(*ast.FencedCodeBlock)
	if entering {
		lang := rendererLanguage[string(node.Language(source))]
		if lang == "" {
			panic(fmt.Errorf("unknown language %q", lang))
		}
		r.lit(writer, fmt.Sprintf("#+BEGIN_SRC %s\n", lang))
		r.lit(writer, string(node.Lines().Value(source)))
	} else {
		r.lit(writer, "#+END_SRC\n\n")
		r.lit(writer, "#+TEXINFO: @noindent")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) link(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	node := n.(*ast.Link)
	if entering {
		dest := string(node.Destination)
		// CommonMark helpfully URL-escapes link destinations, but this
		// prevents links to Info nodes containing spaces.
		match := regexp.MustCompile(`^(info:[^#:]+[#:])(.*%.*)$`).FindStringSubmatch(dest)
		if match != nil {
			s, err := url.PathUnescape(match[2])
			if err != nil {
				panic(err)
			}
			dest = match[1] + s
		}
		r.lit(writer, fmt.Sprintf("[[%s][", dest))
	} else {
		r.lit(writer, "]]")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) htmlInline(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	node := n.(*ast.RawHTML)
	if entering {
		tag := string(node.Segments.Value(source))
		org := tags[tag]
		if org == "" {
			return ast.WalkStop, fmt.Errorf("unknown HTML tag %s", tag)
		}
		r.lit(writer, org)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) textBlock(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	_ = n.(*ast.TextBlock)
	return ast.WalkContinue, nil
}

// Signal an error if we don’t implement something.
func (r *orgRenderer) unknown(writer util.BufWriter, source []byte, n ast.Node, entering bool) (ast.WalkStatus, error) {
	return ast.WalkStop, fmt.Errorf("unknown node type %q", n.Kind())
}

var tags = map[string]string{
	"<code>":  "@@texinfo:@code{@@",
	"</code>": "@@texinfo:}@@",
	"<var>":   "@@texinfo:@var{@@",
	"</var>":  "@@texinfo:}@@",
}
