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
	helper *renderer.Helper[io.Writer, config]
}

// NewRenderer creates a new [Renderer].
func NewRenderer() *Renderer {
	s := new(renderState)
	helper := new(renderer.HelperBuilder[io.Writer, config]).Options(
		withChildlessNodeRenderer(s.doText),
		withNodeRenderer(doNothing, s.endParagraph),
		withNodeRenderer(s.beginListItem, s.endListItem),
		withChildlessNodeRenderer(s.doCodeSpan),
		withChildlessNodeRenderer(s.doCodeBlock),
		withNodeRenderer(s.beginLink, s.endLink),
		withChildlessNodeRenderer(s.emitRawHTML),

		withUnsupported(ast.KindAutoLink),
		withUnsupported(ast.KindBlockquote),
		withUnsupported(ast.KindEmphasis),
		withUnsupported(ast.KindHTMLBlock),
		withUnsupported(ast.KindHeading),
		withUnsupported(ast.KindImage),
		withUnsupported(ast.KindLinkReferenceDefinition),
		withUnsupported(ast.KindStrong),
		withUnsupported(ast.KindThematicBreak),
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

func withNodeRenderer[T ast.Node](enter, leave func(io.Writer, []byte, T) error) renderer.Option[config] {
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
	return renderer.WithNodeRenderer[io.Writer, config](kind, renderer.NodeRendererFunc(fn))
}

func withChildlessNodeRenderer[T ast.Node](fun func(io.Writer, []byte, T) error) renderer.Option[config] {
	enter := func(w io.Writer, source []byte, n T) error {
		if n.HasChildren() {
			return fmt.Errorf("node %#v has children", n)
		}
		return fun(w, source, n)
	}
	leave := doNothing[T]
	return withNodeRenderer(enter, leave)
}

func withUnsupported(kind ast.NodeKind) renderer.Option[config] {
	return renderer.WithNodeRenderer[io.Writer, config](kind, renderer.NodeRendererFunc(doUnsupported))
}

type config struct {
	Config renderer.Config[io.Writer, config]
}

type renderState struct {
	inLine bool
	inItem bool
}

var rendererLanguage = map[string]string{
	"sh": "sh",
	"c":  "c",
}

func (r *renderState) write(w io.Writer, s string) error {
	if strings.ContainsAny(s, "\r\n\u0085\u2028\u2029") {
		return fmt.Errorf("newline in string %q", s)
	}
	if !r.inLine && r.inItem {
		s = "  " + s
	}
	_, err := io.WriteString(w, s)
	r.inLine = true
	return err
}

func (r *renderState) doText(writer io.Writer, source []byte, n *ast.Text) error {
	if n.HardLineBreak() {
		return errors.New("unsupported hard line break")
	}
	s := n.Value.Str(source)
	s = escapedChar.ReplaceAllString(s, "$1")
	// See https://orgmode.org/manual/Escape-Character.html.
	if err := r.write(writer, specialChar.ReplaceAllString(s, "$1\u200B")); err != nil {
		return err
	}
	if n.SoftLineBreak() {
		if _, err := io.WriteString(writer, "\n"); err != nil {
			return err
		}
		r.inLine = false
	}
	return nil
}

var (
	escapedChar = regexp.MustCompile(`\\(.)`)
	specialChar = regexp.MustCompile(`([\[\]*/_=~+])`)
)

func (r *renderState) endParagraph(writer io.Writer, source []byte, n *ast.Paragraph) error {
	if !r.inLine {
		return errors.New("trying to end paragraph at beginning of line")
	}
	var o string
	if s := n.NextSibling(); s != nil && s.Kind() == ast.KindParagraph {
		o = "\n\n"
	} else {
		o = "\n"
	}
	_, err := io.WriteString(writer, o)
	r.inLine = false
	return err
}

func (r *renderState) beginListItem(writer io.Writer, source []byte, n *ast.ListItem) error {
	if r.inLine {
		return errors.New("trying to begin list item in the middle of a line")
	}
	if r.inItem {
		return errors.New("no support for nested lists")
	}
	_, err := io.WriteString(writer, "- ")
	r.inLine = true
	r.inItem = true
	return err

}

func (r *renderState) endListItem(writer io.Writer, source []byte, n *ast.ListItem) error {
	if !r.inItem {
		return errors.New("imbalanced list item")
	}
	r.inItem = false
	return nil
}

func (r *renderState) doCodeSpan(writer io.Writer, source []byte, n *ast.CodeSpan) error {
	return r.write(writer, fmt.Sprintf("~%s~", n.Value.Str(source)))
}

func (r *renderState) doCodeBlock(writer io.Writer, source []byte, n *ast.CodeBlock) error {
	if r.inLine {
		return errors.New("trying to begin code block in the middle of a line")
	}
	if r.inItem {
		return errors.New("no support for code blocks in lists")
	}
	lang, ok := n.Language(source)
	if !ok {
		return errors.New("language not given")
	}
	lang = rendererLanguage[lang]
	if lang == "" {
		return fmt.Errorf("unknown language %q", lang)
	}
	_, err := fmt.Fprintf(writer, "#+BEGIN_SRC %s\n%s#+END_SRC\n#+TEXINFO: @noindent\n", lang, n.Value.Str(source))
	return err
}

func (r *renderState) beginLink(writer io.Writer, source []byte, n *ast.Link) error {
	dest := n.Destination.Str(source)
	return r.write(writer, fmt.Sprintf("[[%s][", dest))
}

func (r *renderState) endLink(writer io.Writer, source []byte, n *ast.Link) error {
	return r.write(writer, "]]")
}

func (r *renderState) emitRawHTML(writer io.Writer, source []byte, n *ast.RawHTML) error {
	tag := n.Value.Str(source)
	org := tags[tag]
	if org == "" {
		return fmt.Errorf("unknown HTML tag %s", tag)
	}
	return r.write(writer, org)
}

func doNothing[T ast.Node](io.Writer, []byte, T) error {
	return nil
}

// Signal an error if we don’t implement something.
func doUnsupported(writer io.Writer, source []byte, n ast.Node, entering bool, rc renderer.Context) (ast.WalkStatus, error) {
	return ast.WalkStop, fmt.Errorf("unsupported node type %q", n.Kind())
}

var tags = map[string]string{
	"<code>":  "@@texinfo:@code{@@",
	"</code>": "@@texinfo:}@@",
	"<var>":   "@@texinfo:@var{@@",
	"</var>":  "@@texinfo:}@@",
}
