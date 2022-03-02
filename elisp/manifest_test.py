# Copyright 2021, 2022 Google LLC
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

"""Unit tests for elisp.manifest."""

import argparse
import io
import json
import pathlib
import unittest

from phst_rules_elisp.elisp import manifest

class ManifestTest(unittest.TestCase):
    """Unit tests for the manifest.add and manifest.write functions."""

    def test_add(self) -> None:
        """Unit test for manifest.add."""
        args = ['--foo']
        with manifest.add('wrap', args) as file:
            file.write('test')
        self.assertEqual(len(args), 3)
        self.assertEqual(args[0], '--foo')
        self.assertRegex(args[1], r'^--manifest=')
        self.assertEqual(args[2], '--')

    def test_write(self) -> None:
        """Unit test for manifest.write."""
        opts = argparse.Namespace()
        opts.load_directory = [pathlib.PurePath('load-dir')]
        opts.load_file = [pathlib.PurePath('load-file')]
        opts.data_file = [pathlib.PurePath('data-file')]
        opts.rule_tag = ['tag-1', 'tag-2 \t\n\r\f äα𝐴🐈\'\0\\"']
        with io.StringIO() as file:
            manifest.write(opts,
                           [pathlib.PurePath('in-1'), pathlib.PurePath('in-2')],
                           [pathlib.PurePath('out \t\n\r\f äα𝐴🐈\'\0\\"')],
                           file)
            self.assertDictEqual(
                json.loads(file.getvalue()),
                {
                    'root': 'RUNFILES_ROOT',
                    'loadPath': ['load-dir'],
                    'inputFiles': ['in-1', 'in-2', 'load-file', 'data-file'],
                    'outputFiles': ['out \t\n\r\f äα𝐴🐈\'\0\\"'],
                    'tags': ['tag-1', 'tag-2 \t\n\r\f äα𝐴🐈\'\0\\"'],
                })


if __name__ == '__main__':
    unittest.main()
