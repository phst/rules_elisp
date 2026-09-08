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
		withNodeRenderer(orgRenderer.document, doNothing),
		withNodeRenderer(orgRenderer.text, doNothing),
		withNodeRenderer(orgRenderer.paragraph, orgRenderer.paragraph),
		withNodeRenderer(orgRenderer.list, doNothing),
		withNodeRenderer(orgRenderer.item, orgRenderer.endItem),
		withNodeRenderer(orgRenderer.code, doNothing),
		withNodeRenderer(orgRenderer.codeBlock, doNothing),
		withNodeRenderer(orgRenderer.link, orgRenderer.endLink),
		withNodeRenderer(orgRenderer.htmlInline, doNothing),

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

func withNodeRenderer[T ast.Node](enter, leave func(io.Writer, []byte, T, renderer.Context) error) renderer.Option[rendererConfig] {
	var zero T
	kind := zero.Kind()
	fn := func(w io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
		node := n.(T)
		var err error
		if entering {
			err = enter(w, source, node, rc)
		} else {
			err = leave(w, source, node, rc)
		}
		if err != nil {
			return ast.WalkStop, err
		}
		return ast.WalkContinue, nil
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

func (r *orgRenderer) document(writer io.Writer, source []byte, n *ast.Document, rc renderer.Context) error {
	return r.cr(writer)
}

func (r *orgRenderer) text(writer io.Writer, source []byte, n *ast.Text, rc renderer.Context) error {
	if n.HasChildren() {
		return fmt.Errorf("node %#v has children", n)
	}
	s := n.Value.Str(source)
	s = regexp.MustCompile(`\\(.)`).ReplaceAllString(s, "$1")
	indent := ""
	if strings.HasSuffix(r.lastOut, "\n") {
		indent = r.indent
	}
	// See https://orgmode.org/manual/Escape-Character.html.
	if err := r.lit(writer, indent+regexp.MustCompile(`([\[\]*/_=~+])`).ReplaceAllString(s, "$1\u200B")); err != nil {
		return err
	}
	if n.SoftLineBreak() {
		if err := r.cr(writer); err != nil {
			return err
		}
	}
	if n.HardLineBreak() {
		return errors.New("unsupported hard line break")
	}
	return nil
}

func (r *orgRenderer) paragraph(writer io.Writer, source []byte, n *ast.Paragraph, rc renderer.Context) error {
	if n.Parent().Kind() != ast.KindListItem {
		return r.lit(writer, "\n")
	}
	return nil
}

func (r *orgRenderer) list(writer io.Writer, source []byte, n *ast.List, rc renderer.Context) error {
	return r.cr(writer)
}

func (r *orgRenderer) item(writer io.Writer, source []byte, n *ast.ListItem, rc renderer.Context) error {
	if r.indent != "" {
		return errors.New("no support for nested lists")
	}
	r.indent = "  "
	return r.lit(writer, "- ")
}

func (r *orgRenderer) endItem(writer io.Writer, source []byte, n *ast.ListItem, rc renderer.Context) error {
	if r.indent != "  " {
		return errors.New("no support for nested lists")
	}
	r.indent = ""
	return r.cr(writer)
}

func (r *orgRenderer) code(writer io.Writer, source []byte, n *ast.CodeSpan, rc renderer.Context) error {
	if n.HasChildren() {
		return fmt.Errorf("node %#v has children", n)
	}
	return r.lit(writer, fmt.Sprintf("~%s~", n.Value.Str(source)))
}

func (r *orgRenderer) codeBlock(writer io.Writer, source []byte, n *ast.CodeBlock, rc renderer.Context) error {
	if n.HasChildren() {
		return fmt.Errorf("node %#v has children", n)
	}
	lang, ok := n.Language(source)
	if !ok {
		return errors.New("language not given")
	}
	lang = rendererLanguage[lang]
	if lang == "" {
		return fmt.Errorf("unknown language %q", lang)
	}
	return r.lit(writer, fmt.Sprintf("#+BEGIN_SRC %s\n%s#+END_SRC\n\n#+TEXINFO: @noindent", lang, n.Value.Str(source)))
}

func (r *orgRenderer) link(writer io.Writer, source []byte, n *ast.Link, rc renderer.Context) error {
	dest := n.Destination.Str(source)
	return r.lit(writer, fmt.Sprintf("[[%s][", dest))
}
func (r *orgRenderer) endLink(writer io.Writer, source []byte, n *ast.Link, rc renderer.Context) error {
	return r.lit(writer, "]]")
}

func (r *orgRenderer) htmlInline(writer io.Writer, source []byte, n *ast.RawHTML, rc renderer.Context) error {
	if n.HasChildren() {
		return fmt.Errorf("node %#v has children", n)
	}
	tag := n.Value.Str(source)
	org := tags[tag]
	if org == "" {
		return fmt.Errorf("unknown HTML tag %s", tag)
	}
	return r.lit(writer, org)
}

func doNothing[T ast.Node](io.Writer, []byte, T, renderer.Context) error {
	return nil
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
