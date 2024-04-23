# Copyright 2024 Philipp Stephani
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

Most tests are in //elisp/proto:proto-test.el; this file contains tests that
require a fresh Emacs process.
"""

import pathlib
import subprocess

from absl import flags
from absl.flags import FLAGS
from absl.testing import absltest

from elisp import runfiles


flags.DEFINE_string('cat', None, 'location of the //elisp/proto:cat target',
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
        infile = self._tempdir / 'in'
        infile.write_bytes(b'stdin \xFF')
        outfile = self._tempdir / 'out'
        with infile.open('rb') as stdin:
            result = subprocess.run([self._cat, '>', outfile], check=True,
                                    stdin=stdin, capture_output=True)
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
