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
import re
from typing import Callable, List, Optional, Tuple

import commonmark
import commonmark.node
import commonmark.render.renderer

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
        generator = _Generator(file)
        generator.run(module)

class _Generator:
    _LICENSE = """# Copyright 2020, 2021, 2022 Google LLC
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
"""

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
        stardoc_output_pb2.STRING_LIST_DICT:
            'dictionary string → list of strings',
        stardoc_output_pb2.OUTPUT: 'output file',
        stardoc_output_pb2.OUTPUT_LIST: 'list of output files',
    }

    _MANDATORY = {
        False: '  Optional.',
        True: '  Mandatory.',
    }

    def __init__(self, file: io.TextIOBase):
        self._file = file

    def run(self, module: stardoc_output_pb2.ModuleInfo) -> None:
        """Writes the generated Org Mode output."""
        self._write(self._LICENSE)
        self._write('\n')
        self._markdown(module.module_docstring)
        for rule in module.rule_info:
            self._rule(rule)
        for provider in module.provider_info:
            self._provider(provider)
        for func in module.func_info:
            self._function(func)
        for aspect in module.aspect_info:
            self._aspect(aspect)

    def _rule(self, rule: stardoc_output_pb2.RuleInfo) -> None:
        self._heading(1, f'~{rule.rule_name}~ rule', rule.rule_name)
        self._findex(rule.rule_name)
        self._write('\n')
        self._markdown(rule.doc_string)
        for attr in rule.attribute:
            self._attribute(attr)

    def _function(self, func: stardoc_output_pb2.StarlarkFunctionInfo) -> None:
        self._heading(1, f'~{func.function_name}~ function', func.function_name)
        self._findex(func.function_name)
        self._write('\n')
        self._markdown(func.doc_string)
        for param in func.parameter:
            self._parameter(param)
        returns = getattr(func, 'return').doc_string
        if returns:
            self._write(f'- Returns :: {returns}\n\n')
        if func.deprecated.doc_string:
            raise ValueError(
                f'unsupported deprecated function {func.function_name}')

    def _parameter(self, param: stardoc_output_pb2.FunctionParamInfo) -> None:
        self._heading(2, f'~{param.name}~ parameter', None)
        self._write('\n')
        self._markdown(param.doc_string + self._MANDATORY[param.mandatory])
        if param.default_value:
            self._write(f'- Default :: ~{param.default_value}~\n')

    def _provider(self, provider: stardoc_output_pb2.ProviderInfo) -> None:
        self._heading(1, f'~{provider.provider_name}~ provider',
                      provider.provider_name)
        self._findex(provider.provider_name)
        self._write('\n')
        self._markdown(provider.doc_string)
        for field in provider.field_info:
            self._heading(2, f'~{field.name}~ field', None)
            self._write('\n')
            self._markdown(field.doc_string)

    def _aspect(self, aspect: stardoc_output_pb2.AspectInfo) -> None:
        self._heading(1, f'~{aspect.aspect_name}~ aspect', aspect.aspect_name)
        self._findex(aspect.aspect_name)
        self._write('\n')
        self._markdown(aspect.doc_string)
        if aspect.aspect_attribute:
            attrs = ', '.join(f'~{a}~' for a in aspect.aspect_attribute)
            self._write(f'This aspect propagates along the following '
                        f'attributes: {attrs}\n')
        for attr in aspect.attribute:
            self._attribute(attr)

    def _attribute(self, attr: stardoc_output_pb2.AttributeInfo) -> None:
        self._heading(2, f'~{attr.name}~ attribute', None)
        self._write('\n')
        self._markdown(attr.doc_string + self._MANDATORY[attr.mandatory])
        self._write(f'- Type :: {self._ATTRIBUTE_TYPE[attr.type]}\n')
        if attr.default_value:
            self._write(f'- Default :: ~{attr.default_value}~\n')
        if attr.provider_name_group:
            group, = attr.provider_name_group
            names = ', '.join(f'~{name}~' for name in group.provider_name)
            self._write(f'- Required providers :: {names}\n')
        self._write('\n')

    def _heading(self, level: int, heading: str, node: Optional[str]) -> None:
        prefix = level * '*'
        self._write(f'{prefix} {heading}\n')
        self._write(':PROPERTIES:\n')
        if node is not None:
            # Work around apparent Org Mode bug.
            node = node.replace('_', r'\under{}')
            self._write(f':ALT_TITLE: {node}\n')
        else:
            self._write(':UNNUMBERED: notoc\n')
        self._write(':END:\n')

    def _findex(self, entry: str) -> None:
        self._write(f'#+FINDEX: {entry}\n')

    def _markdown(self, text: str):
        """Convert a Markdown snippet to Org-mode."""
        # Bazel (including Stardoc) interprets all files as Latin-1,
        # https://docs.bazel.build/versions/4.1.0/build-ref.html#BUILD_files.
        # However, our files all use UTF-8, leading to double encoding.  Reverse
        # that effect here.
        text = text.strip().encode('latin-1').decode('utf-8')
        document = commonmark.Parser().parse(text)
        text = _OrgRenderer().render(document)
        self._write(text + '\n')

    def _write(self, text: str) -> None:
        self._file.write(text)

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

    _TAGS = {'var': ('@@texinfo:@var{@@', '@@texinfo:}@@')}

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
