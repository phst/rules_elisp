# Copyright 2021, 2022, 2023 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Helper binary to convert Stardoc output into Org Mode format."""

import argparse
from collections.abc import Callable
import html.parser
import io
import pathlib
import re
import textwrap
from typing import Optional

import commonmark
import commonmark.node
import commonmark.render.renderer

from docs import stardoc_output_pb2

# The generated protocol buffer modules generate their members dynamically, so
# Pylint can’t find them.
# pylint: disable=no-member

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('input', type=pathlib.Path)
    parser.add_argument('output', type=pathlib.Path)
    opts = parser.parse_args()
    module = stardoc_output_pb2.ModuleInfo.FromString(opts.input.read_bytes())
    # Force Unix-style line endings for consistent results.  See
    # https://github.com/bazelbuild/stardoc/issues/110.
    with opts.output.open(mode='xt', encoding='utf-8', newline='\n') as file:
        generator = _Generator(file)
        generator.run(module)


class _Generator:

    _ATTRIBUTE_TYPE = {
        stardoc_output_pb2.NAME: 'Name',
        stardoc_output_pb2.INT: 'Integer',
        stardoc_output_pb2.LABEL: 'Label',
        stardoc_output_pb2.STRING: 'String',
        stardoc_output_pb2.STRING_LIST: 'List of strings',
        stardoc_output_pb2.INT_LIST: 'List of integers',
        stardoc_output_pb2.LABEL_LIST: 'List of labels',
        stardoc_output_pb2.BOOLEAN: 'Boolean',
        stardoc_output_pb2.LABEL_STRING_DICT: 'Dictionary string → label',
        stardoc_output_pb2.STRING_DICT: 'Dictionary string → string',
        stardoc_output_pb2.STRING_LIST_DICT:
            'Dictionary string → list of strings',
        stardoc_output_pb2.OUTPUT: 'Output file',
        stardoc_output_pb2.OUTPUT_LIST: 'List of output files',
    }

    _MANDATORY = {
        False: 'optional',
        True: 'mandatory',
    }

    def __init__(self, file: io.TextIOBase):
        self._file = file

    def run(self, module: stardoc_output_pb2.ModuleInfo) -> None:
        """Writes the generated Org Mode output."""
        self._write(_markdown(module.module_docstring))
        for rule in module.rule_info:
            self._rule(rule)
        for provider in module.provider_info:
            self._provider(provider)
        for func in module.func_info:
            self._function(func)
        for aspect in module.aspect_info:
            self._aspect(aspect)

    def _rule(self, rule: stardoc_output_pb2.RuleInfo) -> None:
        name = rule.rule_name
        attrs = ', '.join(a.name if a.mandatory else f'[{a.name}]'
                          for a in rule.attribute)
        self._write(f'#+ATTR_TEXINFO: :options Rule {name} ({attrs})\n')
        self._write('#+BEGIN_deffn\n')
        self._write(_markdown(rule.doc_string).lstrip())
        self._write(f'The ~{rule.rule_name}~ rule '
                    f'supports the following attributes:\n\n')
        for attr in rule.attribute:
            self._attribute(attr)
        self._write('#+END_deffn\n\n')

    def _function(self, func: stardoc_output_pb2.StarlarkFunctionInfo) -> None:
        name = func.function_name
        params = ', '.join(p.name if p.mandatory else f'[{p.name}]'
                           for p in func.parameter)
        self._write(f'#+ATTR_TEXINFO: :options {name} ({params})\n')
        self._write('#+BEGIN_defun\n')
        self._write(_markdown(func.doc_string).lstrip())
        for param in func.parameter:
            self._parameter(param)
        returns = getattr(func, 'return').doc_string
        if returns:
            self._write(f'Returns: {returns}\n\n')
        if func.deprecated.doc_string:
            raise ValueError(
                f'unsupported deprecated function {func.function_name}')
        self._write('#+END_defun\n\n')

    def _parameter(self, param: stardoc_output_pb2.FunctionParamInfo) -> None:
        doc = _markdown(param.doc_string).strip()
        if not doc.endswith('.'):
            raise ValueError(
                f'documentation string {doc!r} should end with a period')
        suffixes = [self._MANDATORY[param.mandatory]]
        if param.default_value:
            suffixes.append(f'default: ~{param.default_value}~')
        suffix = '; '.join(suffixes)
        self._item(f'{param.name} :: {doc}.  {suffix}.')

    def _provider(self, provider: stardoc_output_pb2.ProviderInfo) -> None:
        name = provider.provider_name
        fields = ', '.join(f.name for f in provider.field_info)
        self._write(f'#+ATTR_TEXINFO: :options Provider {name} ({fields})\n')
        self._write('#+BEGIN_deffn\n')
        self._write(_markdown(provider.doc_string).lstrip())
        self._write(f'The ~{provider.provider_name}~ provider '
                    f'has the following fields:\n\n')
        for field in provider.field_info:
            doc = _markdown(field.doc_string).strip()
            if not doc.endswith('.'):
                raise ValueError(
                    f'documentation string {doc!r} should end with a period')
            self._item(f'~{field.name}~ :: {doc}')
        self._write('#+END_deffn\n\n')

    def _aspect(self, aspect: stardoc_output_pb2.AspectInfo) -> None:
        name = aspect.aspect_name
        attrs = ', '.join(a.name if a.mandatory else f'[{a.name}]'
                          for a in aspect.attribute)
        self._write(f'#+ATTR_TEXINFO: :options Aspect {name} ({attrs})\n')
        self._write('#+BEGIN_deffn\n')
        self._write(_markdown(aspect.doc_string).lstrip())
        if aspect.aspect_attribute:
            attrs = ', '.join(f'~{a}~' for a in aspect.aspect_attribute)
            self._write(f'This aspect propagates along the following '
                        f'attributes: {attrs}\n')
        for attr in aspect.attribute:
            self._attribute(attr)
        self._write('#+END_deffn\n\n')

    def _attribute(self, attr: stardoc_output_pb2.AttributeInfo) -> None:
        doc = _markdown(attr.doc_string).strip()
        if not doc.endswith('.'):
            raise ValueError(
                f'documentation string {doc!r} should end with a period')
        suffixes = [self._ATTRIBUTE_TYPE[attr.type],
                    self._MANDATORY[attr.mandatory]]
        if attr.default_value:
            suffixes.append(f'default: ~{attr.default_value}~')
        if attr.provider_name_group:
            (group,) = attr.provider_name_group
            names = ', '.join(f'~{name}~' for name in group.provider_name)
            suffixes.append(f'required providers: {names}')
        suffix = '; '.join(suffixes)
        self._item(f'~{attr.name}~ :: {doc}  {suffix}.')

    def _item(self, text: str) -> None:
        self._write(
            _fill(text, initial_indent='- ', subsequent_indent='  ') + '\n')

    def _write(self, text: str) -> None:
        self._file.write(text)


def _markdown(text: str) -> str:
    """Convert a Markdown snippet to Org-mode."""
    # Bazel (including Stardoc) interprets all files as Latin-1,
    # cf. https://bazel.build/concepts/build-files.  However, our files all use
    # UTF-8, leading to double encoding.  Reverse that effect here.
    text = text.strip().encode('latin-1').decode('utf-8')
    document = commonmark.Parser().parse(text)
    text = _OrgRenderer().render(document)
    return text + '\n'


def _fill(text: str, *,
          initial_indent: str = '', subsequent_indent: str = '') -> str:
    return textwrap.fill(
        text, width=80, tabsize=4, fix_sentence_endings=True,
        initial_indent=initial_indent, subsequent_indent=subsequent_indent,
        break_long_words=False, break_on_hyphens=False)


class _OrgRenderer(commonmark.render.renderer.Renderer):

    _LANGUAGE = {'bash': 'sh'}

    def __init__(self):
        super().__init__()
        self._indent = ''

    def out(self, s) -> None:
        indent = self._indent if self.last_out.endswith('\n') else ''
        self.lit(indent + self._escape(s))

    @staticmethod
    def _escape(string: str) -> str:
        # See https://orgmode.org/manual/Escape-Character.html.
        return re.sub(r'([\[\]*/_=~+])', '\\1\N{ZERO WIDTH SPACE}', string)

    # The interface is mandated by CommonMark.  Silence false-positive lint
    # errors.
    # pylint: disable=missing-function-docstring, unused-argument

    def document(self, node: commonmark.node.Node, entering: bool) -> None:
        self.cr()

    def text(self, node: commonmark.node.Node, entering: bool) -> None:
        self.out(node.literal)

    def softbreak(self, node: commonmark.node.Node, entering: bool) -> None:
        self.cr()

    def linebreak(self, node: commonmark.node.Node, entering: bool) -> None:
        self.lit(r'\\')
        self.cr()

    def paragraph(self, node: commonmark.node.Node, entering: bool) -> None:
        if node.parent.t != 'item':
            self.lit('\n')

    def list(self, node: commonmark.node.Node, entering: bool) -> None:
        if entering:
            self.cr()

    def item(self, node: commonmark.node.Node, entering: bool) -> None:
        if entering:
            assert not self._indent  # no support for nested lists
            self.lit('- ')
            self._indent = '  '
        else:
            assert self._indent == '  '  # no support for nested lists
            self._indent = ''
            self.cr()

    def code(self, node: commonmark.node.Node, entering: bool) -> None:
        self.lit(f'~{node.literal}~')

    def code_block(self, node: commonmark.node.Node, entering: bool) -> None:
        lang = self._LANGUAGE[node.info]
        self.lit(f'#+BEGIN_SRC {lang}\n{node.literal}#+END_SRC\n\n')
        self.lit('#+TEXINFO: @noindent')

    def link(self, node: commonmark.node.Node, entering: bool) -> None:
        self.lit(f'[[{node.destination}][' if entering else ']]')

    def html_inline(self, node: commonmark.node.Node, entering: bool) -> None:
        writer = io.StringIO()
        parser = _HTMLParser(writer)
        parser.feed(node.literal)
        self.lit(writer.getvalue())

    # Signal an error if we don’t implement something.
    def __getattr__(self, name) -> Callable[[commonmark.node.Node, bool], None]:
        if not name or name.startswith('_'):
            raise AttributeError(name)
        # Assume this is supposed to be a rendering method.
        return self._unknown

    def _unknown(self, node: commonmark.node.Node, entering: bool) -> None:
        raise NotImplementedError(f'unknown node type {node.t!r}')


class _HTMLParser(html.parser.HTMLParser):  # pylint: disable=abstract-method
    # Work around https://bugs.python.org/issue31844.

    _TAGS = {
        'code': ('@@texinfo:@code{@@', '@@texinfo:}@@'),
        'var': ('@@texinfo:@var{@@', '@@texinfo:}@@'),
    }

    def __init__(self, writer: io.TextIOBase):
        super().__init__()
        self._writer = writer

    def handle_starttag(self, tag: str,
                        attrs: list[tuple[str, Optional[str]]]) -> None:
        if attrs:
            raise NotImplementedError('got attributes {attrs} for <{tag}> tag')
        start, _ = self._TAGS[tag]
        self._writer.write(start)

    def handle_endtag(self, tag: str) -> None:
        _, end = self._TAGS[tag]
        self._writer.write(end)

    def handle_data(self, data: str) -> None:
        self._writer.write(data)


if __name__ == '__main__':
    main()
