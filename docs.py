#!/usr/bin/python3

# Copyright 2020 Google LLC
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

"""Builds all Stardoc documents and copies them into the workspace."""

import pathlib
import re
import shutil
import subprocess

from typing import Any, Dict, Optional, Text, Tuple

def main() -> None:
    workspace = pathlib.Path(bazel('info', 'workspace',
                                   stdout=subprocess.PIPE).rstrip())
    targets = bazel('query', '--output=label',
                    r'filter("_doc\.md$", kind("generated file", //...:*))',
                    stdout=subprocess.PIPE).splitlines()
    print(f'found {len(targets)} generated Markdown files')
    bazel('build', '--', *targets)
    for target in targets:
        package, stem = re.match(r'^//([^:]+):(.+)_doc\.md$', target).groups()
        source = workspace / 'bazel-bin' / package / (stem + "_doc.md")
        dest = workspace / package / (stem + ".md")
        print(f'copying {source} to {dest}')
        shutil.copyfile(source, dest)

def bazel(*args: Tuple[Text], **kwargs: Dict[Text, Any]) -> Optional[Text]:
    return subprocess.run(['bazel', '--bazelrc=/dev/null'] + list(args),
                          check=True, stdin=subprocess.DEVNULL,
                          encoding='utf-8', **kwargs).stdout

if __name__ == '__main__':
    main()
