# Copyright 2020-2025 Google LLC
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
use it outside the rules or depend on its behavior.
"""

import argparse
from collections.abc import Iterable
import glob
import os
import os.path
import pathlib
import platform
import shlex
import shutil
import subprocess
import tempfile

def main() -> None:
    """Configures and builds Emacs."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--readme', type=pathlib.Path, required=True)
    parser.add_argument('--install', type=pathlib.Path, required=True)
    parser.add_argument(
        '--srcs', type=argparse.FileType('rt', encoding='utf-8'), required=True)
    parser.add_argument('--bash', type=pathlib.Path, required=True)
    parser.add_argument('--cc', type=pathlib.Path, required=True)
    parser.add_argument('--cflags', required=True)
    parser.add_argument('--ldflags', required=True)
    parser.add_argument('--module-header', type=pathlib.Path)
    args = parser.parse_args()
    source = args.readme.parent
    install = args.install.resolve()

    _build(source=source, install=install, lines=args.srcs, bash=args.bash,
           cc=args.cc, cflags=args.cflags, ldflags=args.ldflags)

    if args.module_header:
        # Copy emacs-module.h to the desired location.
        shutil.copy(install / 'include' / 'emacs-module.h', args.module_header)


def _build(*, source: pathlib.Path, install: pathlib.Path,
           lines: Iterable[str], bash: pathlib.Path,
           cc: pathlib.Path, cflags: str, ldflags: str) -> None:
    windows = platform.system() == 'Windows'
    temp = pathlib.Path(tempfile.mkdtemp(prefix='emacs-build-'))
    build = temp / 'build'
    _unpack(source=source, install=build, lines=lines)

    def run(*command: str) -> None:
        env = None
        if windows:
            # Building Emacs requires MinGW, see nt/INSTALL.W64.  Therefore we
            # invoke commands through the MinGW shell, see
            # https://www.msys2.org/wiki/Launchers/#the-idea.
            env = dict(os.environ, MSYSTEM='MINGW64', CHERE_INVOKING='1')
            command = (str(bash), '-l',
                       '-c', ' '.join(map(shlex.quote, command)))
        try:
            subprocess.run(command, check=True, cwd=build, env=env,
                           stdin=subprocess.DEVNULL,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                           encoding='utf-8', errors='backslashreplace')
        except subprocess.CalledProcessError as ex:
            print('command', ' '.join(map(shlex.quote, ex.cmd)),
                  'failed, output follows:')
            print(ex.stdout)
            print()
            try:
                config_log = build / 'config.log'
                content = config_log.read_text(encoding='utf-8',
                                               errors='backslashescape')
                print('config.log follows:')
                print(content)
            except FileNotFoundError:
                print('config.log not found')
            print()
            print('temporary build directory is ', temp)
            raise

    run('./configure', '--prefix=' + install.as_posix(),
        '--without-all', '--without-ns', '--without-x', '--with-x-toolkit=no',
        '--without-libgmp',
        # Enable toolkit scrollbars to work around
        # https://debbugs.gnu.org/37042.
        '--with-modules', '--with-toolkit-scroll-bars',
        '--disable-build-details',
        'CC=' + cc.resolve().as_posix(),
        'CFLAGS=' + cflags,
        'LDFLAGS=' + ldflags)
    # Work around https://bugs.gnu.org/76441.
    # We can remove the workaround once we drop support for Emacs 29.
    run('make', 'install', 'MAKEINFO=:')

    # Build directory no longer needed, delete it.
    shutil.rmtree(temp, ignore_errors=True)
    # Move files into hard-coded subdirectories so that run_emacs.py has less
    # work to do.
    exe_suffix = '.exe' if windows else ''
    _rename(install / 'bin' / ('emacs' + exe_suffix), install / 'emacs.exe')
    shared = _glob_unique(install / 'share' / 'emacs' / '[0-9]*')
    _rename(shared / 'etc', install / 'etc')
    _rename(
        _glob_unique(install / 'libexec' / 'emacs' / '*' / '*' / 'emacs*.pdmp'),
        install / 'emacs.pdmp')
    _rename(shared / 'lisp', install / 'lisp')


def _unpack(*, source: pathlib.Path, install: pathlib.Path,
            lines: Iterable[str]) -> None:
    for line in lines:
        src = pathlib.Path(line.rstrip('\n'))
        rel = src.relative_to(source)
        dest = install / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(src, dest)


def _glob_unique(pattern: pathlib.PurePath) -> pathlib.Path:
    # Don’t use pathlib’s globbing functions because we want to skip dotfiles.
    files = glob.glob(str(pattern))
    if not files:
        raise FileNotFoundError(f'no file matches {pattern}')
    if len(files) > 1:
        raise OSError(f'multiple files match {pattern}: {files}')
    return pathlib.Path(files[0])


def _rename(src: pathlib.Path, dest: pathlib.Path) -> pathlib.Path:
    if dest.exists(follow_symlinks=False):
        raise FileExistsError(f'destination file {dest} already exists')
    ret = src.resolve(strict=True).rename(dest)
    src.unlink(missing_ok=True)
    return ret


if __name__ == '__main__':
    main()
