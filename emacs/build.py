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

"""This program configures and builds Emacs.

It’s an internal implementation detail of the Bazel rules for Emacs Lisp; don’t
use it outside the rules or depend on its behavior.
"""

import argparse
from collections.abc import Set
import glob
import json
import os
import os.path
import pathlib
import platform
import re
import shlex
import shutil
import subprocess
import tempfile
from typing import Optional, Union

def main() -> None:
    """Configures and builds Emacs."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--archive', type=pathlib.Path)
    group.add_argument('--readme', type=pathlib.Path)
    parser.add_argument('--strip-prefix', type=pathlib.PurePosixPath)
    parser.add_argument('--install', type=pathlib.Path, required=True)
    parser.add_argument('--cc', type=pathlib.Path, required=True)
    parser.add_argument('--cflags', required=True)
    parser.add_argument('--ldflags', required=True)
    parser.add_argument('--module-header', type=pathlib.Path)
    parser.add_argument('--builtin-features', type=pathlib.Path)
    args = parser.parse_args()
    source = (args.readme.resolve().parent
              if args.readme
              else (args.archive, args.strip_prefix))
    install = args.install.resolve()

    features = _build(source=source, install=install,
                      cc=args.cc, cflags=args.cflags, ldflags=args.ldflags,
                      builtin_features=bool(args.builtin_features))

    if args.builtin_features:
        data = {'builtinFeatures': sorted(features)}
        with args.builtin_features.open('wt', encoding='utf-8') as file:
            json.dump(data, file)

    if args.module_header:
        # Copy emacs-module.h to the desired location.
        shutil.copy(install / 'include' / 'emacs-module.h', args.module_header)


# Don’t use | due to https://bugs.python.org/issue42233.
def _build(*, source: Union[pathlib.Path,
                            tuple[pathlib.Path, pathlib.PurePosixPath]],
           install: pathlib.Path,
           cc: pathlib.Path, cflags: str, ldflags: str,
           builtin_features: bool) -> Optional[Set[str]]:
    windows = platform.system() == 'Windows'
    if windows:
        bash = _find_bash(cc)
    temp = pathlib.Path(tempfile.mkdtemp(prefix='emacs-build-'))
    build = temp / 'build'

    if isinstance(source, tuple):
        archive, prefix = source
        _unpack_archive(archive, build, prefix=prefix)
    else:
        shutil.copytree(source, build)

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
    run('make', 'install')

    features = _builtin_features(build / 'lisp') if builtin_features else None

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
    lisp = _rename(shared / 'lisp', install / 'lisp')
    # Delete source files that have a corresponding compiled file, as these
    # files don’t work well with Coverage (see
    # e.g. https://debbugs.gnu.org/40766).
    for compiled in lisp.glob('**/*.elc'):
        compiled.with_suffix('.el').unlink()

    return features


def _find_bash(c_compiler: pathlib.Path) -> pathlib.Path:
    if c_compiler.parts[-3:-1] != ('mingw64', 'bin'):
        raise ValueError(f'unsupported C compiler location {c_compiler}')
    msys = c_compiler.parents[2]
    bash = msys / 'usr' / 'bin' / 'bash.exe'
    if not bash.is_file():
        raise FileNotFoundError(f'no Bash program found in {msys}')
    return bash


def _builtin_features(lisp: pathlib.Path) -> Set[str]:
    features = set()
    for source in lisp.glob('**/*.el'):
        with source.open('rt', encoding='ascii', errors='replace') as file:
            for line in file:
                match = re.match(r"\(provide '([-/\w]+)\)", line, re.ASCII)
                if match:
                    features.add(match.group(1))
    return frozenset(features)


def _glob_unique(pattern: pathlib.PurePath) -> pathlib.Path:
    # Don’t use pathlib’s globbing functions because we want to skip dotfiles.
    files = glob.glob(str(pattern))
    if not files:
        raise FileNotFoundError(f'no file matches {pattern}')
    if len(files) > 1:
        raise OSError(f'multiple files match {pattern}: {files}')
    return pathlib.Path(files[0])


def _rename(src: pathlib.Path, dest: pathlib.Path) -> pathlib.Path:
    # Once we require Python 3.12, we can use
    # dest.exists(resolve_symlink=False).
    if os.path.lexists(dest):
        raise FileExistsError(f'destination file {dest} already exists')
    ret = src.resolve(strict=True).rename(dest)
    src.unlink(missing_ok=True)
    return ret


def _unpack_archive(archive: pathlib.Path, dest: pathlib.Path, *,
                    prefix: Optional[pathlib.PurePosixPath] = None) -> None:
    # Once we require Python 3.12, we can use
    # dest.exists(resolve_symlink=False).
    if os.path.lexists(dest):
        raise FileExistsError(f'destination directory {dest} already exists')
    prefix = prefix or pathlib.PurePosixPath()
    if prefix.is_absolute():
        raise ValueError(f'absolute prefix {prefix}')
    temp = pathlib.Path(tempfile.mkdtemp('emacs-unpack-'))
    shutil.unpack_archive(archive, temp)
    _rename(temp / prefix, dest)
    shutil.rmtree(temp)


if __name__ == '__main__':
    main()
