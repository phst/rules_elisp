# Copyright 2021, 2022 Google LLC
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
import html.parser
import io
import pathlib
import textwrap
from typing import List, Tuple

import marko
import marko.block
import marko.element
import marko.inline

from phst_rules_elisp.docs import stardoc_output_pb2

def _main() -> None:
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('input', type=pathlib.Path)
    parser.add_argument('output', type=pathlib.Path)
    opts = parser.parse_args()
    module = stardoc_output_pb2.ModuleInfo.FromString(opts.input.read_bytes())
    # Force Unix-style line endings for consistent results.  See
    # https://github.com/bazelbuild/stardoc/issues/110.
    with opts.output.open(mode='xt', encoding='utf-8', newline='\n') as file:
        _markdown(file, module.module_docstring)
        for rule in module.rule_info:
            file.write(f'* ~{rule.rule_name}~ rule\n\n')
            _markdown(file, rule.doc_string)
            for attr in rule.attribute:
                file.write(f'** ~{attr.name}~ attribute\n\n')
                _markdown(file, attr.doc_string + _MANDATORY[attr.mandatory])
                file.write(f'- Type :: {_ATTRIBUTE_TYPE[attr.type]}\n')
                if attr.default_value:
                    file.write(f'- Default :: ~{attr.default_value}~\n')
                if attr.provider_name_group:
                    group, = attr.provider_name_group
                    names = ', '.join(
                        f'~{name}~' for name in group.provider_name)
                    file.write(f'- Required providers :: {names}\n')
                file.write('\n')
        for provider in module.provider_info:
            file.write(f'* ~{provider.provider_name}~ provider\n\n')
            _markdown(file, provider.doc_string)
            for field in provider.field_info:
                file.write(f'** ~{field.name}~ field\n\n')
                _markdown(file, field.doc_string)
        for func in module.func_info:
            file.write(f'* ~{func.function_name}~ function\n\n')
            _markdown(file, func.doc_string)
            for param in func.parameter:
                file.write(f'** ~{param.name}~ parameter\n\n')
                _markdown(file, func.doc_string + _MANDATORY[param.mandatory])
                if param.default_value:
                    file.write(f'- Default :: ~{param.default_value}~\n')
            returns = getattr(func, 'return').doc_string
            if returns:
                file.write(f'- Returns :: {returns}\n\n')
            if func.deprecated.doc_string:
                raise ValueError(
                    f'unsupported deprecated function {func.function_name}')
        if module.aspect_info:
            raise ValueError('aspects are not yet supported')

_ATTRIBUTE_TYPE = {
    stardoc_output_pb2.NAME: 'name',
    stardoc_output_pb2.INT: 'integer',
    stardoc_output_pb2.LABEL: 'label',
    stardoc_output_pb2.STRING: 'string',
    stardoc_output_pb2.STRING_LIST: 'list of strings',
    stardoc_output_pb2.INT_LIST: 'list of integers',
    stardoc_output_pb2.LABEL_LIST: 'list of labels',
    stardoc_output_pb2.BOOLEAN: 'Boolean',
    stardoc_output_pb2.LABEL_STRING_DICT: 'dictionary string → label',
    stardoc_output_pb2.STRING_DICT: 'dictionary string → string',
    stardoc_output_pb2.STRING_LIST_DICT: 'dictionary string → list of strings',
    stardoc_output_pb2.OUTPUT: 'output file',
    stardoc_output_pb2.OUTPUT_LIST: 'list of output files',
}

_MANDATORY = {
    False: '  Optional.',
    True: '  Mandatory.',
}

def _markdown(writer: io.TextIOBase, text: str):
    """Convert a Markdown snippet to Org-mode."""
    # Bazel (including Stardoc) interprets all files as Latin-1,
    # cf. https://docs.bazel.build/versions/4.1.0/build-ref.html#BUILD_files.
    # However, our files all use UTF-8, leading to double encoding.  Reverse
    # that effect here.
    text = text.strip().encode('latin-1').decode('utf-8')
    text = marko.Markdown(renderer=_OrgRenderer).convert(text)
    writer.write(text)

class _OrgRenderer(marko.Renderer):
    _LANGUAGE = {'bash': 'sh'}

    # The interface is mandated by Marko.  Silence false-positive lint errors.
    # pylint: disable=missing-function-docstring, no-self-use

    def render_raw_text(self, element: marko.inline.RawText) -> str:
        return element.children

    def render_blank_line(self, element: marko.block.BlankLine) -> str:
        del element
        return ''

    def render_line_break(self, element: marko.inline.LineBreak) -> str:
        return '\n' if element.soft else '\\\\\n'

    def render_paragraph(self, element: marko.block.Paragraph) -> str:
        return self.render_children(element) + '\n\n'

    def render_list_item(self, element: marko.block.ListItem) -> str:
        text = textwrap.indent(self.render_children(element), '  ').lstrip()
        return f'- {text}'

    def render_code_span(self, element: marko.inline.CodeSpan) -> str:
        return f'~{element.children}~'

    def render_fenced_code(self, element: marko.block.FencedCode) -> str:
        lang = self._LANGUAGE[element.lang]
        code = self.render_children(element)
        return f'#+BEGIN_SRC {lang}\n{code}#+END_SRC\n\n'

    def render_link(self, element: marko.inline.Link) -> str:
        text = self.render_children(element)
        return f'[[{element.dest}][{text}]]'

    def render_inline_html(self, element: marko.inline.InlineHTML) -> str:
        writer = io.StringIO()
        parser = _HTMLParser(writer)
        parser.feed(element.children)
        return writer.getvalue()


class _HTMLParser(html.parser.HTMLParser):  # pylint: disable=abstract-method
    # Work around https://bugs.python.org/issue31844.

    _TAGS = {'var': ('‘', '’')}

    def __init__(self, writer: io.TextIOBase):
        super().__init__()
        self._writer = writer

    def handle_starttag(self, tag: str, attrs: List[Tuple[str, str]]) -> None:
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
    _main()
