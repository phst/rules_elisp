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

"""Runs Pylint."""

import logging
import os
import pathlib
import shutil
import sys
import tempfile

from pylint import lint  # pytype: disable=import-error

class Workspace:
    """Represents a temporary workspace for Pylint and Pytype."""

    def __init__(self) -> None:
        # https://docs.bazel.build/user-manual.html#run
        srcdir = pathlib.Path(os.getenv('BUILD_WORKSPACE_DIRECTORY'))
        external_repos = srcdir / f'bazel-{srcdir.name}' / 'external'
        workspace_name = 'phst_rules_elisp'
        srcs = []
        tempdir = pathlib.Path(tempfile.mkdtemp(prefix='pylint-'))
        for dirpath, dirnames, filenames in os.walk(srcdir):
            dirpath = pathlib.Path(dirpath)
            if dirpath == srcdir:
                # Filter out convenience symlinks on Windows.
                dirnames[:] = [d for d in dirnames
                               if not d.startswith('bazel-')]
            for file in filenames:
                if file.endswith('.py'):
                    src = dirpath / file
                    rel = src.relative_to(srcdir)
                    dest = tempdir / workspace_name / rel
                    dest.parent.mkdir(parents=True, exist_ok=True)
                    # Mimic the Bazel behavior.  Also see
                    # https://github.com/bazelbuild/bazel/issues/10076.
                    (dest.parent / '__init__.py').touch()
                    src.link_to(dest)
                    srcs.append(dest)
        if not srcs:
            raise FileNotFoundError(f'no source files in {srcdir} found')
        _logger.info('using temporary workspace: %s', tempdir)
        self.tempdir = tempdir
        self.srcdir = srcdir
        self.srcs = frozenset(srcs)
        self.external_repos = external_repos

    def clean(self) -> None:
        """Clean up the temporary directory."""
        shutil.rmtree(self.tempdir)
        self.tempdir = None

def _main() -> None:
    logging.getLogger('phst_rules_elisp').setLevel(logging.INFO)
    # Set a fake PYTHONPATH so that Pylint can find imports for the main and
    # external workspaces.
    workspace = Workspace()
    sys.path += [str(workspace.external_repos), str(workspace.tempdir)]
    try:
        lint.Run(['--rcfile=' + str(workspace.srcdir / '.pylintrc'), '--']
                 + sorted(map(str, workspace.srcs)))
    except SystemExit as ex:
        # Only clean up the workspace if we exited successfully, to help with
        # debugging.
        if ex.code == 0:
            workspace.clean()
        raise

_logger = logging.getLogger('phst_rules_elisp.run_pylint')

if __name__ == '__main__':
    _main()
