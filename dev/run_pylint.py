# Copyright 2021-2025 Google LLC
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

"""Runs Pylint."""

import argparse
import shlex
import sys
import subprocess


def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('args', nargs='+')
    args = parser.parse_args()
    result = subprocess.run(
        args.args,
        check=False,
        encoding='utf-8', errors='backslashreplace')
    if result.returncode:
        print('$', shlex.join(result.args))
        sys.exit(result.returncode)


if __name__ == '__main__':
    main()
