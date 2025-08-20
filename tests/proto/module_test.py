# Copyright 2024, 2025 Philipp Stephani
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Unit tests for //elisp/proto:module.

Most tests are in //tests/proto:proto-test.el; this file contains tests that
require a fresh Emacs process.
"""

import pathlib
import subprocess

from absl import flags  # type: ignore[import-not-found]
from absl.testing import absltest  # type: ignore[import-not-found]

from elisp.private.tools import runfiles


FLAGS = flags.FLAGS

flags.DEFINE_string('cat', None, 'location of the //tests/proto:cat target',
                    required=True)


class ModuleTest(absltest.TestCase):
    """Unit tests for //elisp/proto:module."""

    def setUp(self):
        super().setUp()
        self._cat = runfiles.Runfiles().resolve(
            pathlib.PurePosixPath(FLAGS.cat))
        self._tempdir = pathlib.Path(self.create_tempdir())

    def test_insert_stdin(self):
        """Unit test for elisp/proto/insert-stdin."""
        outfile = self._tempdir / 'out'
        result = subprocess.run([self._cat, '>', outfile], check=True,
                                input=b'stdin \xFF', capture_output=True)
        self.assertEmpty(result.stdout)
        self.assertEmpty(result.stderr)
        self.assertEqual(outfile.read_bytes(), b'stdin \xFF')

    def test_write_stdout(self):
        """Unit test for elisp/proto/write-stdout."""
        infile = self._tempdir / 'in'
        infile.write_bytes(b'stdout \xFF')
        result = subprocess.run([self._cat, '<', infile], check=True,
                                capture_output=True)
        self.assertEqual(result.stdout, b'stdout \xFF')
        self.assertEmpty(result.stderr)


if __name__ == '__main__':
    absltest.main()
