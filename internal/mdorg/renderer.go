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
	s := new(renderState)
	helper := new(renderer.HelperBuilder[io.Writer, rendererConfig]).Options(
		withNodeRenderer[*ast.Document](doNothing, doNothing),
		withChildlessNodeRenderer(s.text),
		withNodeRenderer(doNothing, s.paragraph),
		withNodeRenderer[*ast.List](doNothing, doNothing),
		withNodeRenderer(s.item, s.endItem),
		withChildlessNodeRenderer(s.code),
		withChildlessNodeRenderer(s.codeBlock),
		withNodeRenderer(s.link, s.endLink),
		withChildlessNodeRenderer(s.htmlInline),

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

func withNodeRenderer[T ast.Node](enter, leave func(io.Writer, []byte, T) error) renderer.Option[rendererConfig] {
	var zero T
	kind := zero.Kind()
	fn := func(w io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
		node, ok := n.(T)
		if !ok {
			return ast.WalkStop, fmt.Errorf("node of kind %s has wrong type %T, want %T", n.Kind(), n, node)
		}
		if entering {
			return ast.WalkContinue, enter(w, source, node)
		} else {
			return ast.WalkContinue, leave(w, source, node)
		}
	}
	return renderer.WithNodeRenderer[io.Writer, rendererConfig](kind, renderer.NodeRendererFunc(fn))
}

func withChildlessNodeRenderer[T ast.Node](fun func(io.Writer, []byte, T) error) renderer.Option[rendererConfig] {
	enter := func(w io.Writer, source []byte, n T) error {
		if n.HasChildren() {
			return fmt.Errorf("node %#v has children", n)
		}
		return fun(w, source, n)
	}
	leave := doNothing[T]
	return withNodeRenderer(enter, leave)
}

func withUnknown(kind ast.NodeKind) renderer.Option[rendererConfig] {
	return renderer.WithNodeRenderer[io.Writer, rendererConfig](kind, renderer.NodeRendererFunc(unknown))
}

type rendererConfig struct {
	Config renderer.Config[io.Writer, rendererConfig]
}

type renderState struct {
	indent  string
	lastOut string
}

var rendererLanguage = map[string]string{
	"sh": "sh",
	"c":  "c",
}

func (r *renderState) lit(w io.Writer, s string) error {
	if _, err := io.WriteString(w, s); err != nil {
		return err
	}
	r.lastOut = s
	return nil
}

func (r *renderState) text(writer io.Writer, source []byte, n *ast.Text) error {
	if n.HardLineBreak() {
		return errors.New("unsupported hard line break")
	}
	s := n.Value.Str(source)
	s = regexp.MustCompile(`\\(.)`).ReplaceAllString(s, "$1")
	if n.SoftLineBreak() {
		s += "\n"
	}
	indent := ""
	if strings.HasSuffix(r.lastOut, "\n") {
		indent = r.indent
	}
	// See https://orgmode.org/manual/Escape-Character.html.
	return r.lit(writer, indent+regexp.MustCompile(`([\[\]*/_=~+])`).ReplaceAllString(s, "$1\u200B"))
}

func (r *renderState) paragraph(writer io.Writer, source []byte, n *ast.Paragraph) error {
	if s := n.NextSibling(); s != nil && s.Kind() == ast.KindParagraph {
		return r.lit(writer, "\n\n")
	}
	return r.lit(writer, "\n")
}

func (r *renderState) item(writer io.Writer, source []byte, n *ast.ListItem) error {
	if r.indent != "" {
		return errors.New("no support for nested lists")
	}
	r.indent = "  "
	return r.lit(writer, "- ")
}

func (r *renderState) endItem(writer io.Writer, source []byte, n *ast.ListItem) error {
	if r.indent != "  " {
		return errors.New("no support for nested lists")
	}
	r.indent = ""
	return nil
}

func (r *renderState) code(writer io.Writer, source []byte, n *ast.CodeSpan) error {
	return r.lit(writer, fmt.Sprintf("~%s~", n.Value.Str(source)))
}

func (r *renderState) codeBlock(writer io.Writer, source []byte, n *ast.CodeBlock) error {
	lang, ok := n.Language(source)
	if !ok {
		return errors.New("language not given")
	}
	lang = rendererLanguage[lang]
	if lang == "" {
		return fmt.Errorf("unknown language %q", lang)
	}
	return r.lit(writer, fmt.Sprintf("#+BEGIN_SRC %s\n%s#+END_SRC\n#+TEXINFO: @noindent\n", lang, n.Value.Str(source)))
}

func (r *renderState) link(writer io.Writer, source []byte, n *ast.Link) error {
	dest := n.Destination.Str(source)
	return r.lit(writer, fmt.Sprintf("[[%s][", dest))
}
func (r *renderState) endLink(writer io.Writer, source []byte, n *ast.Link) error {
	return r.lit(writer, "]]")
}

func (r *renderState) htmlInline(writer io.Writer, source []byte, n *ast.RawHTML) error {
	tag := n.Value.Str(source)
	org := tags[tag]
	if org == "" {
		return fmt.Errorf("unknown HTML tag %s", tag)
	}
	return r.lit(writer, org)
}

func doNothing[T ast.Node](io.Writer, []byte, T) error {
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
