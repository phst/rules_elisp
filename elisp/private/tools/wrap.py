# Copyright 2020, 2021, 2022, 2024, 2025 Google LLC
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

"""Binary wrap is a test helper program for //elisp:binary_test, which see."""

import argparse
import io
import json
import os
import pathlib
import platform
import sys
from typing import Any
import unittest

from elisp.private.tools import runfiles

def main() -> None:
    """Main function."""
    if isinstance(sys.stdout, io.TextIOWrapper):  # typical case
        sys.stdout.reconfigure(encoding='utf-8', errors='backslashreplace',
                               line_buffering=True)
    print('Args:', sys.argv)
    print('Environment:', os.environ)
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--manifest', type=pathlib.Path, required=True)
    parser.add_argument('rest', nargs='+')
    args = parser.parse_args()
    run_files = runfiles.Runfiles()
    output_file = pathlib.PurePath(r'C:\Temp\output.dat'
                                   if platform.system() == 'Windows'
                                   else '/tmp/output.dat')

    class Test(unittest.TestCase):
        """Unit tests for the command line and manifest."""

        maxDiff = 5000

        def test_args(self) -> None:
            """Test that the Emacs command line is as expected."""
            got = args.rest
            want = ['--quick', '--batch']
            # The load path setup depends on whether we use manifest-based or
            # directory-based runfiles.
            try:
                directory = run_files.resolve(
                    pathlib.PurePosixPath('phst_rules_elisp'))
            except FileNotFoundError:
                # Manifest-based runfiles.
                want += [
                    '--load=' + str(run_files.resolve(pathlib.PurePosixPath(
                        'phst_rules_elisp/elisp/runfiles/runfiles.elc'))),
                    '--funcall=elisp/runfiles/install-handler',
                    '--directory=/bazel-runfile:phst_rules_elisp',
                ]
            else:
                # Directory-based runfiles.
                want.append('--directory=' + str(directory))
            want += [
                '--option',
                str(run_files.resolve(pathlib.PurePosixPath(
                    'phst_rules_elisp/elisp/private/tools/binary.cc'))),
                ' \t\n\r\f äα𝐴🐈\'\\"',
                '/:' + str(output_file),
            ]
            self.assertListEqual(got, want)

        def test_manifest(self) -> None:
            """Test the manifest."""
            got = json.loads(args.manifest.read_text(encoding='utf-8'))
            want: dict[str, Any] = {
                'root': 'RUNFILES_ROOT',
                'tags': ['local', 'mytag'],
                'loadPath': ['phst_rules_elisp'],
                'inputFiles': ['phst_rules_elisp/elisp/private/tools/binary.cc',
                               'phst_rules_elisp/elisp/private/tools/binary.h'],
                'outputFiles': [str(output_file)],
            }
            for var in (got, want):
                files: list[str] = var.get('inputFiles', [])
                for i, file in enumerate(files):
                    file = pathlib.PurePosixPath(file)
                    if not file.is_absolute():
                        files[i] = str(run_files.resolve(file))
            self.assertDictEqual(got, want)

    tests = unittest.defaultTestLoader.loadTestsFromTestCase(Test)
    if not unittest.TextTestRunner().run(tests).wasSuccessful():
        raise ValueError('incorrect arguments')


if __name__ == '__main__':
    main()
