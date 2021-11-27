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

"""Runs Pytype."""

import argparse
import logging
import os
import pathlib
import subprocess
import sys

from phst_rules_elisp import run_pylint

def _main() -> None:
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--params', type=pathlib.Path, required=True)
    args = parser.parse_args()
    logging.getLogger('phst_rules_elisp').setLevel(logging.INFO)
    # Set a fake PYTHONPATH so that Pytype can find imports for the main and
    # external workspaces.
    workspace = run_pylint.Workspace(args.params)
    # Pytype wants a Python binary available under the name “python”.  See the
    # function pytype.tools.environment.check_python_exe_or_die.
    bindir = workspace.tempdir / 'bin'
    bindir.mkdir()
    (bindir / 'python').symlink_to(sys.executable)
    env = dict(os.environ,
               PATH=os.pathsep.join([str(bindir)] + os.get_exec_path()),
               PYTHONPATH=os.pathsep.join(sys.path + workspace.path))
    result = subprocess.run(
        [sys.executable, '-m', 'pytype',
         '--no-cache', '--'] + sorted(map(str, workspace.srcs)),
        check=False, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        encoding='utf-8', errors='backslashreplace')
    if result.returncode:
        print(result.stdout)
        sys.exit(result.returncode)
    # Only clean up the workspace if we exited successfully, to help with
    # debugging.
    workspace.success()

if __name__ == '__main__':
    _main()
