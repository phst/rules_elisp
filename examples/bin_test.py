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

"""Tests for //examples:bin."""

import os
import pathlib
import platform
import re
import subprocess
import tempfile
import unittest

from phst_rules_elisp.elisp import runfiles

class BinaryTest(unittest.TestCase):
    """Example test showing how to work with elisp_binary rules."""

    def test_run(self) -> None:
        """Runs the test binary and checks its output."""
        run_files = runfiles.Runfiles()
        binary = pathlib.PurePosixPath('phst_rules_elisp/examples/bin')
        if platform.system() == 'Windows':
            binary = binary.with_suffix('.exe')
        binary = run_files.resolve(binary)
        # Be sure to pass environment variables to find runfiles.  We also set
        # GCOV_PREFIX (see
        # https://gcc.gnu.org/onlinedocs/gcc/Cross-profiling.html) to a
        # directory that’s hopefully writable, to avoid logspam when running
        # with “bazel coverage”.
        env = dict(run_files.environment(), GCOV_PREFIX=tempfile.gettempdir())
        for var in ('EMACS', 'PATH', 'SYSTEMROOT'):
            value = os.getenv(var)
            if value:
                env[var] = value
        # You can run the programs produced by elisp_binary rules like any other
        # binary.
        try:
            result = subprocess.run(
                [binary, 'human'], check=True,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                encoding='utf-8', env=env,
                # The working directory doesn’t matter.  Binaries still find
                # their runfiles.
                cwd='/')
        except subprocess.CalledProcessError as ex:
            print('Output of failing process:')
            print(ex.output, flush=True)
            raise
        lines = result.stdout.rstrip('\n').splitlines()
        # This message can happen depending on the mtime of files in the Bazel
        # sandbox.  It shouldn’t influence the test outcome.
        irrelevant = re.compile(
            r'^Source file .+ newer than byte-compiled file; using older file$')
        # We filter out some irrelevant messages that can cause spurious
        # failures.
        lines = [line for line in lines if not irrelevant.match(line)]
        self.assertListEqual(lines,
                             ['hi from bin, ("human")',
                              'hi from lib-2',
                              'hi from lib-4',
                              'hi from lib-1',
                              'hi from data dependency'])


if __name__ == '__main__':
    unittest.main()
