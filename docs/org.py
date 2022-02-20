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
import pathlib
import re

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
        file.write(f'{_markdown(module.module_docstring)}\n\n')
        for rule in module.rule_info:
            file.write(f'* ~{rule.rule_name}~ rule\n\n')
            file.write(f'{_markdown(rule.doc_string)}\n\n')
            for attr in rule.attribute:
                file.write(f'** ~{attr.name}~ attribute\n\n')
                file.write(f'{_markdown(attr.doc_string)}\n')
                file.write(f'{_MANDATORY[attr.mandatory]}.\n\n')
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
            file.write(f'{_markdown(provider.doc_string)}\n\n')
            for field in provider.field_info:
                file.write(f'** ~{field.name}~ field\n\n')
                file.write(f'{_markdown(field.doc_string)}\n\n')
        for func in module.func_info:
            file.write(f'* ~{func.function_name}~ function\n\n')
            file.write(f'{_markdown(func.doc_string)}\n\n')
            for param in func.parameter:
                file.write(f'** ~{param.name}~ parameter\n\n')
                file.write(f'{_markdown(func.doc_string)}\n')
                file.write(f'{_MANDATORY[param.mandatory]}.\n\n')
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

def _markdown(text: str) -> str:
    """Convert a Markdown snippet to Org-mode."""
    # Bazel (including Stardoc) interprets all files as Latin-1,
    # cf. https://docs.bazel.build/versions/4.1.0/build-ref.html#BUILD_files.
    # However, our files all use UTF-8, leading to double encoding.  Reverse
    # that effect here.
    text = text.strip().encode('latin-1').decode('utf-8')
    # Use primitive text replacement, which should be good enough for the time
    # being.
    text = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'[[\2][\1]]', text)
    text = re.sub(r'```(\w*\n[^`]+\n *)```', r'#+BEGIN_SRC \1#+END_SRC', text)
    text = re.sub(r'`([^`]+)`', r'~\1~', text)
    return text

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
    False: 'Optional',
    True: 'Mandatory',
}

if __name__ == '__main__':
    _main()
