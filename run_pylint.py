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

import argparse
import json
import logging
import os
import pathlib
import shutil
import sys
import tempfile

from pylint import lint

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
            shutil.copy(file['src'], dest)
            if not file['ext']:
                srcs.append(dest)
        if not srcs:
            raise FileNotFoundError('no source files found')
        for dirpath, _, _ in os.walk(tempdir):
            dirpath = pathlib.Path(dirpath)
            if dirpath != tempdir:
                # Mimic the Bazel behavior.  Also see
                # https://github.com/bazelbuild/bazel/issues/10076.
                (dirpath / '__init__.py').touch()
        _logger.info('using temporary workspace: %s', tempdir)
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
    parser.add_argument('--rcfile', type=pathlib.Path, required=True)
    args = parser.parse_args()
    logging.getLogger('phst_rules_elisp').setLevel(logging.INFO)
    # Set a fake PYTHONPATH so that Pylint can find imports for the main and
    # external workspaces.
    workspace = Workspace(args.params)
    sys.path += workspace.path
    try:
        lint.Run(['--persistent=no', '--rcfile=' + str(args.rcfile), '--']
                 + sorted(map(str, workspace.srcs)))
    except SystemExit as ex:
        # Only clean up the workspace if we exited successfully, to help with
        # debugging.
        if ex.code == 0:
            workspace.success()
        raise

_logger = logging.getLogger('phst_rules_elisp.run_pylint')

if __name__ == '__main__':
    _main()
