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
import os
import os.path
import pathlib
import shlex
import sys
import subprocess


def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--out', type=pathlib.Path, required=True)
    parser.add_argument('--src', type=pathlib.Path, action='append',
                        default=[], dest='sources')
    parser.add_argument('--import', type=pathlib.Path, action='append',
                        default=[], dest='path')
    parser.add_argument('--pylintrc', type=pathlib.Path, required=True)
    args = parser.parse_args()
    # Set a fake PYTHONPATH so that Pylint can find imports for the main and
    # external repositories.
    srcs = args.sources
    if not srcs:
        raise FileNotFoundError('no source files found')
    srcset = frozenset(srcs)
    repository_path = [str(d) for d in args.path]
    pythonpath = os.pathsep.join(sys.path + repository_path)
    env = dict(os.environ, PYTHONPATH=pythonpath)
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
        print('$', f'PYTHONPATH={shlex.quote(pythonpath)}',
              shlex.join(result.args))
        print(result.stdout)
        sys.exit(result.returncode)
    args.out.touch()


if __name__ == '__main__':
    main()
