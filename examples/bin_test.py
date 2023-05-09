# Copyright 2020, 2021, 2022, 2023 Google LLC
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
import re
import shutil
import subprocess
import tempfile

from absl import flags
from absl.testing import absltest

from elisp import runfiles

FLAGS = flags.FLAGS

flags.DEFINE_string('bin', None, 'runfile location of the binary under test',
                    required=True)

class BinaryTest(absltest.TestCase):
    """Example test showing how to work with elisp_binary rules."""

    def setUp(self) -> None:
        super().setUp()
        self._tempdir = pathlib.Path(tempfile.mkdtemp(
            prefix='bin_test_', dir=os.getenv('TEST_TMPDIR') or None))
        self.addCleanup(shutil.rmtree, self._tempdir)

    def test_run(self) -> None:
        """Runs the test binary and checks its output."""
        run_files = runfiles.Runfiles()
        binary = run_files.resolve(pathlib.PurePosixPath(FLAGS.bin))
        # Be sure to pass environment variables to find runfiles.  We also set
        # GCOV_PREFIX (see
        # https://gcc.gnu.org/onlinedocs/gcc/Cross-profiling.html) and
        # LLVM_PROFILE_FILE (see
        # https://clang.llvm.org/docs/SourceBasedCodeCoverage.html) to a
        # directory/file that’s hopefully writable, to avoid logspam when
        # running with “bazel coverage”.
        env = dict(run_files.environment(),
                   GCOV_PREFIX=str(self._tempdir),
                   LLVM_PROFILE_FILE=str(self._tempdir / 'bazel.%p.profraw'))
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
    absltest.main()
