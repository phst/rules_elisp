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

// Package mdorg contains functionality to convert Markdown to Org-mode.
package mdorg

import (
	"errors"
	"fmt"
	"io"
	"regexp"
	"strings"

	"github.com/yuin/goldmark/v2/ast"
	"github.com/yuin/goldmark/v2/renderer"
)

// Renderer is a [renderer.Renderer] that writes Org-mode text.
// Use [NewRenderer] to create Renderer objects; the zero Renderer is not valid.
type Renderer struct {
	helper *renderer.Helper[io.Writer, rendererConfig]
}

// NewRenderer creates a new [Renderer].
func NewRenderer() *Renderer {
	orgRenderer := newOrgRenderer()
	helper := new(renderer.HelperBuilder[io.Writer, rendererConfig]).Options(
		withNodeRenderer(orgRenderer.document),
		withNodeRenderer(orgRenderer.text),
		withNodeRenderer(orgRenderer.paragraph),
		withNodeRenderer(orgRenderer.list),
		withNodeRenderer(orgRenderer.item),
		withNodeRenderer(orgRenderer.code),
		withNodeRenderer(orgRenderer.codeBlock),
		withNodeRenderer(orgRenderer.link),
		withNodeRenderer(orgRenderer.htmlInline),

		withUnknown(ast.KindAutoLink),
		withUnknown(ast.KindBlockquote),
		withUnknown(ast.KindEmphasis),
		withUnknown(ast.KindHTMLBlock),
		withUnknown(ast.KindHeading),
		withUnknown(ast.KindImage),
		withUnknown(ast.KindLinkReferenceDefinition),
		withUnknown(ast.KindStrong),
		withUnknown(ast.KindThematicBreak),
	).Build()
	return &Renderer{helper}
}

// Render implements [renderer.Renderer.Render].
func (r *Renderer) Render(w io.Writer, source []byte, n ast.Node, opts ...renderer.RenderOption) error {
	return r.helper.Render(w, source, n, opts...)
}

// RenderStringSource implements [renderer.Renderer.RenderStringSource]
func (r *Renderer) RenderStringSource(w io.Writer, source string, n ast.Node, opts ...renderer.RenderOption) error {
	return r.helper.RenderStringSource(w, source, n, opts...)
}

var _ renderer.Renderer[io.Writer] = (*Renderer)(nil)

func withNodeRenderer[T ast.Node](fun func(io.Writer, []byte, T, bool, renderer.Context) (ast.WalkStatus, error)) renderer.Option[rendererConfig] {
	var zero T
	kind := zero.Kind()
	fn := func(w io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
		node := n.(T)
		return fun(w, source, node, entering, rc)
	}
	return renderer.WithNodeRenderer[io.Writer, rendererConfig](kind, renderer.NodeRendererFunc(fn))
}

func withUnknown(kind ast.NodeKind) renderer.Option[rendererConfig] {
	return renderer.WithNodeRenderer[io.Writer, rendererConfig](kind, renderer.NodeRendererFunc(unknown))
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

func (r *orgRenderer) document(writer io.Writer, source []byte, n *ast.Document, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	return ast.WalkContinue, r.cr(writer)
}

func (r *orgRenderer) text(writer io.Writer, source []byte, n *ast.Text, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if n.HasChildren() {
		return ast.WalkStop, fmt.Errorf("node %#v has children", n)
	}
	if entering {
		s := n.Value.Str(source)
		s = regexp.MustCompile(`\\(.)`).ReplaceAllString(s, "$1")
		indent := ""
		if strings.HasSuffix(r.lastOut, "\n") {
			indent = r.indent
		}
		// See https://orgmode.org/manual/Escape-Character.html.
		if err := r.lit(writer, indent+regexp.MustCompile(`([\[\]*/_=~+])`).ReplaceAllString(s, "$1\u200B")); err != nil {
			return ast.WalkStop, err
		}
		if n.SoftLineBreak() {
			if err := r.cr(writer); err != nil {
				return ast.WalkStop, err
			}
		}
		if n.HardLineBreak() {
			return ast.WalkStop, errors.New("unsupported hard line break")
		}
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) paragraph(writer io.Writer, source []byte, n *ast.Paragraph, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if n.Parent().Kind() != ast.KindListItem {
		return ast.WalkContinue, r.lit(writer, "\n")
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) list(writer io.Writer, source []byte, n *ast.List, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if entering {
		return ast.WalkContinue, r.cr(writer)
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) item(writer io.Writer, source []byte, n *ast.ListItem, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
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

func (r *orgRenderer) code(writer io.Writer, source []byte, n *ast.CodeSpan, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if n.HasChildren() {
		return ast.WalkStop, fmt.Errorf("node %#v has children", n)
	}
	if entering {
		return ast.WalkContinue, r.lit(writer, fmt.Sprintf("~%s~", n.Value.Str(source)))
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) codeBlock(writer io.Writer, source []byte, n *ast.CodeBlock, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if n.HasChildren() {
		return ast.WalkStop, fmt.Errorf("node %#v has children", n)
	}
	if entering {
		lang, ok := n.Language(source)
		if !ok {
			return ast.WalkStop, errors.New("language not given")
		}
		lang = rendererLanguage[lang]
		if lang == "" {
			return ast.WalkStop, fmt.Errorf("unknown language %q", lang)
		}
		return ast.WalkContinue, r.lit(writer, fmt.Sprintf("#+BEGIN_SRC %s\n%s#+END_SRC\n\n#+TEXINFO: @noindent", lang, n.Value.Str(source)))
	}
	return ast.WalkContinue, nil
}

func (r *orgRenderer) link(writer io.Writer, source []byte, n *ast.Link, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if entering {
		dest := n.Destination.Str(source)
		return ast.WalkContinue, r.lit(writer, fmt.Sprintf("[[%s][", dest))
	} else {
		return ast.WalkContinue, r.lit(writer, "]]")
	}
}

func (r *orgRenderer) htmlInline(writer io.Writer, source []byte, n *ast.RawHTML, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	if n.HasChildren() {
		return ast.WalkStop, fmt.Errorf("node %#v has children", n)
	}
	if entering {
		tag := n.Value.Str(source)
		org := tags[tag]
		if org == "" {
			return ast.WalkStop, fmt.Errorf("unknown HTML tag %s", tag)
		}
		return ast.WalkContinue, r.lit(writer, org)
	}
	return ast.WalkContinue, nil
}

// Signal an error if we don’t implement something.
func unknown(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	return ast.WalkStop, fmt.Errorf("unknown node type %q", n.Kind())
}

var tags = map[string]string{
	"<code>":  "@@texinfo:@code{@@",
	"</code>": "@@texinfo:}@@",
	"<var>":   "@@texinfo:@var{@@",
	"</var>":  "@@texinfo:}@@",
}
