# Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Unit tests for the elisp_binary rule."""

import pathlib
import platform
import subprocess

from absl import flags
from absl.testing import absltest

from elisp.private.tools import runfiles


FLAGS = flags.FLAGS

flags.DEFINE_string(
    'launcher', None, 'location of the //elisp:launcher target', required=True)
flags.DEFINE_string(
    'binary-cc', None, 'location of the //elisp:binary.cc file', required=True)


class BinaryTest(absltest.TestCase):
    """Unit tests for the elisp_binary rule."""

    def test_run_wrapped(self) -> None:
        """Test that running a binary with a wrapper works."""
        run_files = runfiles.Runfiles()
        input_file = run_files.resolve(
            pathlib.PurePosixPath(getattr(FLAGS, 'binary-cc')))
        windows = platform.system() == 'Windows'
        args = [
            run_files.resolve(pathlib.PurePosixPath(FLAGS.launcher)),
            '--option',
            input_file,
            ''' \t\n\r\f äα𝐴🐈'\\\"''',
            r'/:C:\Temp\output.dat' if windows else '/:/tmp/output.dat',
        ]
        subprocess.run(args, check=True)


if __name__ == '__main__':
    absltest.main()
