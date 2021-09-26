# Copyright 2021 Google LLC
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

"""Functions to write Emacs Lisp wrapper manifests.

This is an internal helper library for the Emacs Lisp Bazel rules.  Don’t rely
on it in any way outside the rule implementation."""

import argparse
import contextlib
import json
import os
import os.path
import pathlib
import tempfile
from typing import Generator, IO, Iterable, List, Optional

@contextlib.contextmanager
def add(mode: str, args: List[str]) -> Generator[Optional[IO[str]], None, None]:
    """Create a temporary file for a manifest if needed.

If a file was created, appropriate arguments are added to args."""
    if mode not in ('direct', 'wrap'):
        raise ValueError(f'invalid mode {mode}')
    if mode == 'direct':
        yield None
    else:
        with tempfile.NamedTemporaryFile(
                mode='wt', encoding='utf-8',
                prefix='manifest-', suffix='.json') as file:
            args += ['--manifest=' + os.path.abspath(file.name), '--']
            yield file.file

def write(opts: argparse.Namespace, input_files: Iterable[pathlib.Path],
          output_files: Iterable[pathlib.Path], file: IO[str]) -> None:
    """Write manifest file contents to the given file object."""
    _check_relative(opts.load_directory)
    _check_relative(opts.load_file)
    data_files = sorted(opts.data_file)
    _check_relative(data_files)
    all_input_files = (tuple(input_files) +
                       tuple(opts.load_file) +
                       tuple(data_files))
    json_data = {
        'root': 'RUNFILES_ROOT',
        'loadPath': tuple(map(os.fspath, opts.load_directory)),
        'inputFiles': tuple(map(os.fspath, all_input_files)),
        'outputFiles': tuple(map(os.fspath, output_files)),
        'tags': sorted(opts.rule_tag),
    }
    json.dump(json_data, file)
    file.flush()

def _check_relative(files: Iterable[pathlib.PurePath]) -> None:
    for file in files:
        if file.is_absolute():
            raise ValueError(r'filename {file} is absolute')
