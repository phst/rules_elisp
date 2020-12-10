# Copyright 2020 Google LLC
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

import argparse
import pathlib
import shlex
import shutil
import subprocess
import tempfile
from typing import Text, Tuple


def main() -> None:
    """Configures and builds Emacs."""
    parser = argparse.ArgumentParser()
    parser.add_argument('--source', type=pathlib.Path, required=True)
    parser.add_argument('--install', type=pathlib.Path, required=True)
    parser.add_argument('--cc', type=pathlib.Path, required=True)
    parser.add_argument('--cflags', required=True)
    parser.add_argument('--ldflags', required=True)
    parser.add_argument('--module-header', type=pathlib.Path, required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory() as temp:
        temp = pathlib.Path(temp)
        build = temp / 'build'
        install = args.install.resolve()
        shutil.copytree(args.source, build)

        def run(*command: Tuple[Text]) -> None:
            try:
                subprocess.run(command, check=True, cwd=build,
                               stdin=subprocess.DEVNULL,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                               encoding='utf-8', errors='backslashescape')
            except subprocess.CalledProcessError as ex:
                print('command', ' '.join(map(shlex.quote, command)), 'failed, output follows:')
                print(ex.stdout)
                print()
                print('config.log follows:')
                print((build / 'config.log').read_text('utf-8', 'backslashescape'))
                raise

        run('./configure', '--prefix=' + str(install),
            '--without-all', '--without-ns', '--with-x-toolkit=no',
            '--with-modules',
            '--disable-build-details',
            'CC=' + str(args.cc.resolve()),
            'CFLAGS=' + args.cflags,
            'LDFLAGS=' + args.ldflags)
        run('make', 'install')
    # Delete source files that have a corresponding compiled file, as these
    # files don’t work well with Coverage (see
    # e.g. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40766).
    for compiled in install.glob('share/emacs/*/lisp/**/*.elc'):
        compiled.with_suffix('.el').unlink()
    # Sanity check to verify that the resulting binary works.
    subprocess.run([install / 'bin/emacs', '--quick', '--batch'],
                   check=True, stdin=subprocess.DEVNULL)
    # Copy emacs-module.h to the desired location.
    shutil.copy(install / 'include/emacs-module.h', args.module_header)


if __name__ == '__main__':
    main()
