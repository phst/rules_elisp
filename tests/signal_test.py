# Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

"""Unit tests for an Emacs Lisp binary that signals an error."""

import pathlib
import platform
import subprocess

from absl import flags
from absl.testing import absltest

from elisp import runfiles

FLAGS = flags.FLAGS

flags.DEFINE_string(
    'binary', None, 'runfile location of the Emacs Lisp binary to test',
    required=True)


class SignalTest(absltest.TestCase):
    """Unit tests for an Emacs Lisp binary that signals an error."""

    def test_run(self) -> None:
        """Tests that the binary exits as expected.

        These tests test some implementation details to catch some unwanted
        behavior like launcher warnings on standard error.
        """
        run_files = runfiles.Runfiles()
        binary = run_files.resolve(pathlib.PurePosixPath(FLAGS.binary))
        process = subprocess.run([str(binary)], env=run_files.environment(),
                                 stdin=subprocess.DEVNULL, capture_output=True,
                                 check=False, encoding='utf-8')
        # Emacs exits with a code of −1 in case of a signal, which gets
        # converted to an exit status with all bits set: 8 bits on Unix systems,
        # 32 bits on Windows.
        self.assertEqual(process.returncode,
                         0xFFFFFFFF if platform.system() == 'Windows' else 0xFF)
        if process.stdout:
            # Emacs 29: error message on standard error, backtrace on standard
            # output.
            self.assertEqual(process.stderr, 'Foo\n')
            self.assertStartsWith(process.stdout, '\nError: error ("Foo")\n')
            self.assertEndsWith(process.stdout, '\n  normal-top-level()\n')
            self.assertContainsSubsequence(process.stdout.splitlines(),
                                           ['Error: error ("Foo")',
                                            '  error("Foo")',
                                            '  normal-top-level()'])
        else:
            # Emacs 28: error message followed with backtrace on standard error,
            # empty standard output.
            self.assertStartsWith(
                process.stderr,
                'Debugger entered--Lisp error: (error "Foo")\n')
            self.assertEndsWith(process.stderr, '\n  normal-top-level()\n\n')
            self.assertContainsSubsequence(
                process.stderr.splitlines(),
                ['Debugger entered--Lisp error: (error "Foo")',
                 '  error("Foo")',
                 '  normal-top-level()'])


if __name__ == '__main__':
    absltest.main()
