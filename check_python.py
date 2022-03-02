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

"""Runs Pylint and Pytype."""

import argparse
import json
import os
import pathlib
import shutil
import sys
import subprocess
import tempfile

class Workspace:
    """Represents a temporary workspace for Pylint and Pytype."""

    def __init__(self, params_file: pathlib.Path) -> None:
        params = json.loads(params_file.read_text(encoding='utf-8'))
        workspace_name = 'phst_rules_elisp'
        srcs = []
        tempdir = pathlib.Path(tempfile.mkdtemp(prefix='pylint-'))
        for file in params['srcs']:
            dest = tempdir / workspace_name / file['rel']
            dest.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(file['src'], dest)
            # Don’t attempt to check generated protocol buffer files.
            if not file['ext'] and not dest.name.endswith('_pb2.py'):
                srcs.append(dest)
        if not srcs:
            raise FileNotFoundError('no source files found')
        for dirpath, _, _ in os.walk(tempdir):
            dirpath = pathlib.Path(dirpath)
            if dirpath != tempdir:
                # Mimic the Bazel behavior.  Also see
                # https://github.com/bazelbuild/bazel/issues/10076.
                (dirpath / '__init__.py').touch()
        self.srcs = frozenset(srcs)
        self.path = [str(tempdir)] + [str(tempdir / d) for d in params['path']]
        self.tempdir = tempdir
        self._output = pathlib.Path(params['out'])

    def success(self) -> None:
        """Clean up the temporary directory."""
        shutil.rmtree(self.tempdir)
        self.tempdir = None
        self._output.touch()


def _main() -> None:
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--params', type=pathlib.Path, required=True)
    parser.add_argument('--pylintrc', type=pathlib.Path, required=True)
    parser.add_argument('--pytype', action='store_true', default=False)
    args = parser.parse_args()
    # Set a fake PYTHONPATH so that Pylint and Pytype can find imports for the
    # main and external workspaces.
    workspace = Workspace(args.params)
    # Pytype wants a Python binary available under the name “python”.  See the
    # function pytype.tools.environment.check_python_exe_or_die.
    bindir = workspace.tempdir / 'bin'
    bindir.mkdir()
    (bindir / 'python').symlink_to(sys.executable)
    cwd = workspace.tempdir / 'phst_rules_elisp'
    env = dict(os.environ,
               PATH=os.pathsep.join([str(bindir)] + os.get_exec_path()),
               PYTHONPATH=os.pathsep.join(sys.path + workspace.path))
    result = subprocess.run(
        [sys.executable, '-m', 'pylint',
         '--persistent=no', '--rcfile=' + str(args.pylintrc.resolve()), '--']
        + [str(file.relative_to(cwd))
           for file in sorted(workspace.srcs)],
        check=False, cwd=cwd, env=env,
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        encoding='utf-8', errors='backslashreplace')
    if result.returncode:
        print(result.stdout)
        sys.exit(result.returncode)
    if os.name == 'posix' and args.pytype:
        result = subprocess.run(
            [sys.executable, '-m', 'pytype',
             '--no-cache', '--'] + [str(file.relative_to(cwd))
                                    for file in sorted(workspace.srcs)],
            check=False, cwd=cwd, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            encoding='utf-8', errors='backslashreplace')
        if result.returncode:
            print(result.stdout)
            sys.exit(result.returncode)
    # Only clean up the workspace if we exited successfully, to help with
    # debugging.
    workspace.success()


if __name__ == '__main__':
    _main()
