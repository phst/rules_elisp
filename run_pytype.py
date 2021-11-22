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

import logging
import sys

from pytype.tools.analyze_project import main  # pylint: disable=import-error

from phst_rules_elisp import run_pylint

def _main() -> None:
    logging.getLogger('phst_rules_elisp').setLevel(logging.INFO)
    # Set a fake PYTHONPATH so that Pytype can find imports for the main and
    # external workspaces.  We’d want to set the Python path to only
    # {external_repos}:{tempdir}, but for some reason that breaks Pytype.
    workspace = run_pylint.Workspace()
    sys.path += [str(workspace.external_repos), str(workspace.tempdir),
                 str(workspace.srcdir)]
    sys.argv = [sys.argv[0],
                '--output=' + str(workspace.srcdir / '.pytype'),
                '--'] + sorted(map(str, workspace.srcs))
    code = main.main()
    # Only clean up the workspace if we exited successfully, to help with
    # debugging.
    if code == 0:
        workspace.clean()
    sys.exit(code)

if __name__ == '__main__':
    _main()
