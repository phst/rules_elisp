# Copyright 2020, 2021 Google LLC
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

"""This program configures and builds Emacs.

It’s an internal implementation detail of the Bazel rules for Emacs Lisp; don’t
use it outside the rules or depend on its behavior."""

import argparse
import os
import pathlib
import shlex
import shutil
import subprocess
import tempfile


def main() -> None:
    """Configures and builds Emacs."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--source', type=pathlib.Path, required=True)
    parser.add_argument('--install', type=pathlib.Path, required=True)
    parser.add_argument('--cc', type=pathlib.Path, required=True)
    parser.add_argument('--cflags', required=True)
    parser.add_argument('--ldflags', required=True)
    parser.add_argument('--module-header', type=pathlib.Path)
    args = parser.parse_args()
    windows = os.name == 'nt'
    if windows:
        bash = _find_bash(args.cc)
    temp = pathlib.Path(tempfile.mkdtemp(prefix='emacs-build-'))
    build = temp / 'build'
    install = args.install.resolve()
    shutil.copytree(args.source, build)

    def run(*command: str) -> None:
        env = None
        if windows:
            # Building Emacs requires MinGW, see nt/INSTALL.W64.  Therefore we
            # invoke commands through the MinGW shell, see
            # https://www.msys2.org/wiki/Launchers/#the-idea.
            env = dict(os.environ, MSYSTEM='MINGW64', CHERE_INVOKING='1')
            command = [str(bash), '-l',
                       '-c', ' '.join(map(shlex.quote, command))]
        try:
            subprocess.run(command, check=True, cwd=build, env=env,
                           stdin=subprocess.DEVNULL,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           encoding='utf-8', errors='backslashescape')
        except subprocess.CalledProcessError as ex:
            print('command', ' '.join(map(shlex.quote, command)),
                  'failed, output follows:')
            print(ex.stdout)
            print()
            try:
                config_log = build / 'config.log'
                content = config_log.read_text('utf-8', 'backslashescape')
                print('config.log follows:')
                print(content)
            except FileNotFoundError:
                print('config.log not found')
            print()
            print('temporary build directory is ', temp)
            raise

    run('./configure', '--prefix=' + install.as_posix(),
        '--without-all', '--without-ns', '--with-x-toolkit=no',
        '--without-libgmp',
        # Enable threads explicitly to work around
        # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30106 in older Emacs
        # versions.  Enable toolkit scrollbars to work around
        # https://debbugs.gnu.org/cgi/bugreport.cgi?bug=37042.
        '--with-modules', '--with-threads', '--with-toolkit-scroll-bars',
        '--disable-build-details',
        'CC=' + args.cc.resolve().as_posix(),
        'CFLAGS=' + args.cflags,
        'LDFLAGS=' + args.ldflags)
    run('make', 'install')
    # Build directory no longer needed, delete it.
    shutil.rmtree(temp, ignore_errors=True)
    # Delete source files that have a corresponding compiled file, as these
    # files don’t work well with Coverage (see
    # e.g. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40766).
    for compiled in install.glob('share/emacs/*/lisp/**/*.elc'):
        compiled.with_suffix('.el').unlink()
    # Sanity check to verify that the resulting binary works.
    subprocess.run([str(install / 'bin' / 'emacs'), '--quick', '--batch'],
                   check=True, stdin=subprocess.DEVNULL)
    if args.module_header:
        # Copy emacs-module.h to the desired location.
        shutil.copy(install / 'include' / 'emacs-module.h', args.module_header)


def _find_bash(c_compiler: pathlib.Path) -> pathlib.Path:
    if c_compiler.parts[-3:-1] != ('mingw64', 'bin'):
        raise ValueError(f'unsupported C compiler location {c_compiler}')
    msys = c_compiler.parents[2]
    bash = msys / 'usr' / 'bin' / 'bash.exe'
    if not bash.is_file():
        raise FileNotFoundError(f'no Bash program found in {msys}')
    return bash


if __name__ == '__main__':
    main()
