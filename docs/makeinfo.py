# Copyright 2025 Philipp Stephani
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

"""Wrapper around Makeinfo."""

import platform
import shutil
import subprocess
import sys

def _main(args: list[str]):
    if platform.system() == 'Windows':
        prefix = ['C:\\MSYS64\\usr\\bin\\perl.exe', '-w',
                  'C:\\MSYS64\\usr\\bin\\makeinfo']
    else:
        prefix = [shutil.which('makeinfo')]
    subprocess.run(prefix + args, check=True)

if __name__ == '__main__':
    _main(sys.argv[1:])
