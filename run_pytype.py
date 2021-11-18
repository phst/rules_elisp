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

import os
import pathlib
import sys
import tempfile

from pytype.tools.analyze_project import main  # pylint: disable=import-error

def _main() -> None:
    # https://docs.bazel.build/user-manual.html#run
    srcdir = pathlib.Path(os.getenv('BUILD_WORKSPACE_DIRECTORY'))
    external_repos = srcdir / f'bazel-{srcdir.name}' / 'external'
    workspace_name = 'phst_rules_elisp'
    srcs = sorted(srcdir.glob('**/*.py'))
    # Set a fake PYTHONPATH so that Pytype can find imports for the main and
    # external workspaces.  We’d want to set the Python path to only
    # {external_repos}:{tempdir}, but for some reason that breaks Pytype.
    with tempfile.TemporaryDirectory(prefix='pytype-') as tempdir_name:
        tempdir = pathlib.Path(tempdir_name)
        (tempdir / workspace_name).symlink_to(srcdir, target_is_directory=True)
        sys.path += [str(external_repos), str(tempdir), str(srcdir)]
        sys.argv = [sys.argv[0], '--'] + list(map(str, srcs))
        sys.exit(main.main())

if __name__ == '__main__':
    _main()
