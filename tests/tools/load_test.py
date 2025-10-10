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

"""Unit tests for elisp.private.tools.load."""

import pathlib
import platform
import tempfile

from absl.testing import absltest  # type: ignore[import-not-found]

from elisp.private.tools import load
from elisp.private.tools import runfiles


class AddPathTest(absltest.TestCase):
    """Unit tests for the load.add_path function."""

    def test_directory(self) -> None:
        """Unit test for directory-based runfiles."""
        with tempfile.TemporaryDirectory() as directory:
            args = list(
                load.path(runfiles.Runfiles({'RUNFILES_DIR': directory}),
                          [pathlib.PurePosixPath('foo'),
                           pathlib.PurePosixPath('bar \t\n\r\f äα𝐴🐈\'\0\\"')],
                          pathlib.PurePosixPath('unused/runfiles.elc')))
        base = pathlib.Path(directory)
        self.assertListEqual(
            args,
            ['--directory=' + str(base / 'foo'),
             '--directory=' + str(base / 'bar \t\n\r\f äα𝐴🐈\'\0\\"')])

    def test_manifest(self) -> None:
        """Unit test for manifest-based runfiles."""
        runfiles_dir = pathlib.Path(
            r'C:\Runfiles' if platform.system() == 'Windows' else '/runfiles')
        runfiles_elc = runfiles_dir / 'runfiles.elc'
        with tempfile.NamedTemporaryFile(
                delete_on_close=False,
                mode='xt', encoding='ascii', newline='\n') as file:
            # Runfiles manifests contain POSIX-style filenames even on Windows.
            file.write(
                'repository/runfiles.elc ' + runfiles_elc.as_posix() + '\n')
            file.flush()
            args = list(load.path(
                runfiles.Runfiles({'RUNFILES_MANIFEST_FILE': file.name}),
                [pathlib.PurePosixPath('foo'),
                 pathlib.PurePosixPath('bar \t\n\r\f äα𝐴🐈\'\0\\"')],
                pathlib.PurePosixPath('repository/runfiles.elc')))
        self.assertListEqual(
            args,
            ['--load=' + str(runfiles_elc),
             '--funcall=elisp/runfiles/install-handler',
             '--directory=/bazel-runfile:foo',
             '--directory=/bazel-runfile:bar \t\n\r\f äα𝐴🐈\'\0\\"'])


if __name__ == '__main__':
    absltest.main()
