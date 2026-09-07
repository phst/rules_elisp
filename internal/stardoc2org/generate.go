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

package main

import (
	"errors"
	"fmt"
	"io"
	"net/url"
	"regexp"
	"strings"

	"github.com/yuin/goldmark/v2/ast"
	"github.com/yuin/goldmark/v2/renderer"
)

type markdownRenderer struct {
	helper *renderer.Helper[io.Writer, rendererConfig]
}

func newRenderer() *markdownRenderer {
	orgRenderer := newOrgRenderer()
	helper := new(renderer.HelperBuilder[io.Writer, rendererConfig]).Options(
		withNodeRenderer(ast.KindDocument, orgRenderer.document),
		withNodeRenderer(ast.KindText, orgRenderer.text),
		withNodeRenderer(ast.KindParagraph, orgRenderer.paragraph),
		withNodeRenderer(ast.KindList, orgRenderer.list),
		withNodeRenderer(ast.KindListItem, orgRenderer.item),
		withNodeRenderer(ast.KindEmphasis, orgRenderer.emph),
		withNodeRenderer(ast.KindCodeSpan, orgRenderer.code),
		withNodeRenderer(ast.KindCodeBlock, orgRenderer.codeBlock),
		withNodeRenderer(ast.KindLink, orgRenderer.link),
		withNodeRenderer(ast.KindRawHTML, orgRenderer.htmlInline),

		withNodeRenderer(ast.KindAutoLink, orgRenderer.unknown),
		withNodeRenderer(ast.KindBlockquote, orgRenderer.unknown),
		withNodeRenderer(ast.KindHTMLBlock, orgRenderer.unknown),
		withNodeRenderer(ast.KindHeading, orgRenderer.unknown),
		withNodeRenderer(ast.KindImage, orgRenderer.unknown),
		withNodeRenderer(ast.KindLinkReferenceDefinition, orgRenderer.unknown),
		withNodeRenderer(ast.KindStrong, orgRenderer.unknown),
		withNodeRenderer(ast.KindThematicBreak, orgRenderer.unknown),
	).Build()
	return &markdownRenderer{helper}
}

func (r *markdownRenderer) Render(w io.Writer, source []byte, n ast.Node, opts ...renderer.RenderOption) error {
	return r.helper.Render(w, source, n, opts...)
}

func (r *markdownRenderer) RenderStringSource(w io.Writer, source string, n ast.Node, opts ...renderer.RenderOption) error {
	return r.helper.RenderStringSource(w, source, n, opts...)
}

var _ renderer.Renderer[io.Writer] = (*markdownRenderer)(nil)

func withNodeRenderer(kind ast.NodeKind, fun func(io.Writer, []byte, ast.Node, bool, renderer.Context) (ast.WalkStatus, error)) renderer.Option[rendererConfig] {
	return renderer.WithNodeRenderer[io.Writer, rendererConfig](kind, renderer.NodeRendererFunc(fun))
}

type rendererConfig struct {
	Config renderer.Config[io.Writer, rendererConfig]
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

func (r *orgRenderer) lit(w io.Writer, s string) error {
	if _, err := io.WriteString(w, s); err != nil {
		return err
	}
	r.lastOut = s
	return nil
}

func (r *orgRenderer) cr(w io.Writer) error {
	if r.lastOut != "\n" {
		return r.lit(w, "\n")
	}
	return nil
}

func (r *orgRenderer) document(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.Document)
	return ast.WalkContinue, r.cr(writer)
}

func (r *orgRenderer) text(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.Text)
	if entering {
		s := node.Value.Str(source)
		s = regexp.MustCompile(`\\(.)`).ReplaceAllString(s, "$1")
		indent := ""
		if strings.HasSuffix(r.lastOut, "\n") {
			indent = r.indent
		}
		// See https://orgmode.org/manual/Escape-Character.html.
		if err := r.lit(writer, indent+regexp.MustCompile(`([\[\]*/_=~+])`).ReplaceAllString(s, "$1\u200B")); err != nil {
			return ast.WalkStop, err
		}
		if node.SoftLineBreak() {
			if err := r.cr(writer); err != nil {
				return ast.WalkStop, err
			}
		}
		if node.HardLineBreak() {
			if err := r.lit(writer, `\\`); err != nil {
				return ast.WalkStop, err
			}
			if err := r.cr(writer); err != nil {
				return ast.WalkStop, err
			}
		}
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) paragraph(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.Paragraph)
	if node.Parent().Kind() != ast.KindListItem {
		return ast.WalkContinue, r.lit(writer, "\n")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) list(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.List)
	if entering {
		return ast.WalkContinue, r.cr(writer)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) item(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.ListItem)
	if entering {
		if r.indent != "" {
			return ast.WalkStop, errors.New("no support for nested lists")
		}
		r.indent = "  "
		return ast.WalkContinue, r.lit(writer, "- ")
	} else {
		if r.indent != "  " {
			return ast.WalkStop, errors.New("no support for nested lists")
		}
		r.indent = ""
		return ast.WalkContinue, r.cr(writer)
	}
}

func (r *orgRenderer) emph(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	_ = n.(*ast.Emphasis)
	return ast.WalkContinue, r.lit(writer, "/")
}

func (r *orgRenderer) code(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.CodeSpan)
	if err := r.lit(writer, "~"); err != nil {
		return ast.WalkStop, err
	}
	if entering {
		return ast.WalkContinue, r.lit(writer, node.Value.Str(source))
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) codeBlock(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
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
		return ast.WalkContinue, errors.Join(
			r.lit(writer, fmt.Sprintf("#+BEGIN_SRC %s\n", lang)),
			r.lit(writer, node.Value.Str(source)),
		)
	} else {
		return ast.WalkContinue, errors.Join(
			r.lit(writer, "#+END_SRC\n\n"),
			r.lit(writer, "#+TEXINFO: @noindent"),
		)
	}
}

func (r *orgRenderer) link(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
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
		return ast.WalkContinue, r.lit(writer, fmt.Sprintf("[[%s][", dest))
	} else {
		return ast.WalkContinue, r.lit(writer, "]]")
	}
}

func (r *orgRenderer) htmlInline(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	node := n.(*ast.RawHTML)
	if entering {
		tag := node.Value.Str(source)
		org := tags[tag]
		if org == "" {
			return ast.WalkStop, fmt.Errorf("unknown HTML tag %s", tag)
		}
		return ast.WalkContinue, r.lit(writer, org)
	}
	return ast.WalkContinue, nil
}

// Signal an error if we don’t implement something.
func (r *orgRenderer) unknown(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	return ast.WalkStop, fmt.Errorf("unknown node type %q", n.Kind())
}

var tags = map[string]string{
	"<code>":  "@@texinfo:@code{@@",
	"</code>": "@@texinfo:}@@",
	"<var>":   "@@texinfo:@var{@@",
	"</var>":  "@@texinfo:}@@",
}
