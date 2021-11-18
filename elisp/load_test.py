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

"""Unit tests for elisp.load."""

import pathlib
import tempfile
import unittest

from phst_rules_elisp.elisp import load
from phst_rules_elisp.elisp import runfiles

class AddPathTest(unittest.TestCase):
    """Unit tests for the load.add_path function."""

    def test_directory(self) -> None:
        """Unit test for directory-based runfiles."""
        args = ['--foo']
        with tempfile.TemporaryDirectory() as directory:
            load.add_path(runfiles.Runfiles({'RUNFILES_DIR': directory}), args,
                          [pathlib.PurePosixPath('foo'),
                           pathlib.PurePosixPath('bar \t\n\r\f äα𝐴🐈\'\0\\"')])
        self.assertListEqual(
            args,
            ['--foo',
             f'--directory={directory}/foo',
             f'--directory={directory}/bar \t\n\r\f äα𝐴🐈\'\0\\"'])

    def test_manifest(self) -> None:
        """Unit test for manifest-based runfiles."""
        args = ['--foo']
        with tempfile.TemporaryDirectory() as directory:
            manifest = pathlib.Path(directory) / 'manifest'
            manifest.write_text('phst_rules_elisp/elisp/runfiles/runfiles.elc '
                                '/runfiles/runfiles.elc\n',
                                encoding='ascii')
            load.add_path(
                runfiles.Runfiles({'RUNFILES_MANIFEST_FILE': str(manifest)}),
                args,
                [pathlib.PurePosixPath('foo'),
                 pathlib.PurePosixPath('bar \t\n\r\f äα𝐴🐈\'\0\\"')])
        self.assertListEqual(
            args,
            ['--foo',
             '--load=/runfiles/runfiles.elc',
             '--funcall=elisp/runfiles/install-handler',
             '--directory=/bazel-runfile:foo',
             '--directory=/bazel-runfile:bar \t\n\r\f äα𝐴🐈\'\0\\"'])

if __name__ == '__main__':
    unittest.main()
