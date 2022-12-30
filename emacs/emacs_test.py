# Copyright 2020, 2021, 2022 Google LLC
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

"""Unit tests for the Emacs binary."""

import argparse
import os
import pathlib
import subprocess
import sys
from typing import Optional
import unittest

from elisp import runfiles

class EmacsTest(unittest.TestCase):
    """Unit tests for the Emacs binary."""

    emacs: Optional[pathlib.PurePosixPath] = None

    def test_version(self) -> None:
        """Tests that emacs --version works."""
        run_files = runfiles.Runfiles()
        emacs = run_files.resolve('phst_rules_elisp' / self.emacs)
        env = dict(os.environ)
        env.update(run_files.environment())
        process = subprocess.run([str(emacs), '--version'], env=env,
                                 check=False)
        self.assertEqual(process.returncode, 0)


def _main() -> None:
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--emacs', type=pathlib.PurePosixPath, required=True)
    args = parser.parse_args()
    EmacsTest.emacs = args.emacs
    unittest.main(argv=sys.argv[:1])


if __name__ == '__main__':
    _main()
