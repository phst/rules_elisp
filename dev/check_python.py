# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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
import os.path
import pathlib
import platform
import shutil
import sys
import subprocess
import tempfile


def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--params', type=pathlib.Path, required=True)
    parser.add_argument('--out', type=pathlib.Path, required=True)
    parser.add_argument('--import', type=pathlib.PurePosixPath, action='append',
                        default=[], dest='path')
    parser.add_argument('--workspace-name', type=str, required=True)
    subparsers = parser.add_subparsers(required=True, dest='program')
    pylint = subparsers.add_parser('pylint', allow_abbrev=False)
    pylint.add_argument('--pylintrc', type=pathlib.Path, required=True)
    subparsers.add_parser('pytype', allow_abbrev=False)
    args = parser.parse_args()
    workspace_name = args.workspace_name
    dirs = [d for d in sys.path if os.path.basename(d) == workspace_name]
    if len(dirs) != 1:
        raise ValueError(f'no unique workspace directory: {dirs}')
    module_space = pathlib.Path(dirs[0]).parent
    module_space_stat = module_space.stat()
    # Set a fake PYTHONPATH so that Pylint and Pytype can find imports for the
    # main and external repositories.
    params = json.loads(args.params.read_text(encoding='utf-8'))
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
        path = pathlib.Path(dirpath)
        if not path.samefile(tempdir):
            # Mimic the Bazel behavior.  Also see
            # https://github.com/bazelbuild/bazel/issues/10076.
            (path / '__init__.py').touch()
    srcset = frozenset(srcs)
    repository_path = [str(tempdir / d) for d in args.path]
    # Pytype wants a Python binary available under the name “python”.  See the
    # function pytype.tools.environment.check_python_exe_or_die.
    bindir = tempdir / 'bin'
    bindir.mkdir()
    (bindir / 'python').symlink_to(sys.executable)
    orig_path = []
    for entry in sys.path:
        try:
            entry_stat = os.stat(entry)
            # We have to remove the module space from the path, otherwise
            # extension importing gets messed up because Astroid attempts to
            # construct a module name based on the directory path, which fails
            # if a directory name contains dots.  Work around that by not
            # allowing imports relative to the module space.
            if not os.path.samestat(entry_stat, module_space_stat):
                orig_path.append(entry)
        except FileNotFoundError:
            pass  # ignore nonexisting entries
    cwd = tempdir / workspace_name
    env = dict(os.environ,
               PATH=os.pathsep.join([str(bindir)] + os.get_exec_path()),
               PYTHONPATH=os.pathsep.join(orig_path + repository_path))
    if args.program == 'pylint':
        result = subprocess.run(
            [sys.executable, '-m', 'pylint',
             # We’d like to add “--” after the options, but that’s not possible
             # due to https://github.com/PyCQA/pylint/issues/7003.
             '--persistent=no', '--rcfile=' + str(args.pylintrc.resolve())]
            + [str(file.relative_to(cwd)) for file in sorted(srcset)],
            check=False, cwd=cwd, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            encoding='utf-8', errors='backslashreplace')
        if result.returncode:
            print(result.stdout)
            sys.exit(result.returncode)
    if platform.system() != 'Windows' and args.program == 'pytype':
        result = subprocess.run(
            [sys.executable, '-m', 'pytype',
             '--pythonpath=' + os.pathsep.join(repository_path),
             '--no-cache', '--'] + [str(file.relative_to(cwd))
                                    for file in sorted(srcset)],
            check=False, cwd=cwd, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            encoding='utf-8', errors='backslashreplace')
        if result.returncode:
            print(result.stdout)
            sys.exit(result.returncode)
    # Only clean up the workspace if we exited successfully, to help with
    # debugging.
    shutil.rmtree(tempdir)
    args.out.touch()


if __name__ == '__main__':
    main()
