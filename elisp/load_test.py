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

from bazel_tools.tools.python.runfiles import runfiles

from phst_rules_elisp.elisp import load

class AddPathTest(unittest.TestCase):
    """Unit tests for the load.add_path function."""

    def test_directory(self) -> None:
        """Unit test for directory-based runfiles."""
        args = ['--foo']
        with tempfile.TemporaryDirectory() as directory:
            load.add_path(runfiles.CreateDirectoryBased(directory), args,
                          [pathlib.Path('foo'), pathlib.Path('bar')])
        self.assertListEqual(args,
                             ['--foo',
                              f'--directory={directory}/foo',
                              f'--directory={directory}/bar'])

    def test_manifest(self) -> None:
        """Unit test for manifest-based runfiles."""
        args = ['--foo']
        with tempfile.NamedTemporaryFile('wt', encoding='ascii') as manifest:
            manifest.write('phst_rules_elisp/elisp/runfiles/runfiles.elc '
                           '/runfiles/runfiles.elc\n')
            manifest.flush()
            load.add_path(runfiles.CreateManifestBased(manifest.name), args,
                          [pathlib.Path('foo'), pathlib.Path('bar')])
        self.assertListEqual(
            args,
            ['--foo',
             '--load=/runfiles/runfiles.elc',
             '--funcall=elisp/runfiles/install-handler',
             '--directory=/bazel-runfile:foo',
             '--directory=/bazel-runfile:bar'])

if __name__ == '__main__':
    unittest.main()
