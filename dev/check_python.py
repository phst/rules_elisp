# Copyright 2021-2025 Google LLC
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

"""Runs Pylint."""

import argparse
import json
import os
import os.path
import pathlib
import sys
import subprocess


def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--params', type=pathlib.Path, required=True)
    parser.add_argument('--out', type=pathlib.Path, required=True)
    parser.add_argument('--import', type=pathlib.Path, action='append',
                        default=[], dest='path')
    parser.add_argument('--workspace-name', type=str, required=True)
    subparsers = parser.add_subparsers(required=True, dest='program')
    pylint = subparsers.add_parser('pylint', allow_abbrev=False)
    pylint.add_argument('--pylintrc', type=pathlib.Path, required=True)
    args = parser.parse_args()
    workspace_name = args.workspace_name
    dirs = [d for d in sys.path if os.path.basename(d) == workspace_name]
    if len(dirs) != 1:
        raise ValueError(f'no unique workspace directory: {dirs}')
    # Set a fake PYTHONPATH so that Pylint can find imports for the main and
    # external repositories.
    params = json.loads(args.params.read_text(encoding='utf-8'))
    srcs = []
    for file in params['srcs']:
        src = pathlib.Path(file['src'])
        # Don’t attempt to check generated protocol buffer files.
        if not file['ext'] and not src.name.endswith('_pb2.py'):
            srcs.append(src)
    if not srcs:
        raise FileNotFoundError('no source files found')
    srcset = frozenset(srcs)
    repository_path = [str(d) for d in args.path]
    env = dict(os.environ,
               PYTHONPATH=os.pathsep.join(sys.path + repository_path))
    if args.program == 'pylint':
        result = subprocess.run(
            [sys.executable, '-m', 'pylint',
             # We’d like to add “--” after the options, but that’s not possible
             # due to https://github.com/PyCQA/pylint/issues/7003.
             '--persistent=no', '--rcfile=' + str(args.pylintrc.resolve())]
            + [str(file) for file in sorted(srcset)],
            check=False, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            encoding='utf-8', errors='backslashreplace')
        if result.returncode:
            print(result.stdout)
            sys.exit(result.returncode)
    args.out.touch()


if __name__ == '__main__':
    main()
