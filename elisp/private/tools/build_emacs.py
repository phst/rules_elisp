# Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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
import datetime
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
    parser.add_argument('--release', action='store_true')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--archive', type=pathlib.Path)
    group.add_argument('--readme', type=pathlib.Path)
    parser.add_argument('--strip-prefix', type=pathlib.PurePosixPath)
    parser.add_argument('--install', type=pathlib.Path, required=True)
    parser.add_argument('--bash', type=pathlib.Path)
    parser.add_argument('--cc', type=pathlib.Path)
    parser.add_argument('--cflags')
    parser.add_argument('--ldflags')
    parser.add_argument('--module-header', type=pathlib.Path)
    parser.add_argument('--builtin-features', type=pathlib.Path)
    args = parser.parse_args()
    source = (args.readme.resolve().parent
              if args.readme
              else (args.archive, args.strip_prefix))
    install = args.install.resolve()

    if args.release:
        features = _unpack(source=source, install=install,
                           builtin_features=bool(args.builtin_features))
    else:
        features = _build(source=source, install=install, bash=args.bash,
                          cc=args.cc, cflags=args.cflags, ldflags=args.ldflags,
                          builtin_features=bool(args.builtin_features))

    if args.builtin_features:
        # We know that ‘features’ isn’t None, but the type system can’t express
        # that, so help static type checkers.
        assert features
        data = {'builtinFeatures': sorted(features)}
        with args.builtin_features.open('wt', encoding='utf-8',
                                        newline='\n') as file:
            json.dump(data, file)

    if args.module_header:
        # Copy emacs-module.h to the desired location.
        shutil.copy(install / 'include' / 'emacs-module.h', args.module_header)


# Don’t use | due to https://bugs.python.org/issue42233.
def _build(*, source: Union[pathlib.Path,
                            tuple[pathlib.Path, pathlib.PurePosixPath]],
           install: pathlib.Path, bash: pathlib.Path,
           cc: pathlib.Path, cflags: str, ldflags: str,
           builtin_features: bool) -> Optional[Set[str]]:
    windows = platform.system() == 'Windows'
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
    run('make', 'install', 'MAKEINFO=:')

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
    for compiled in lisp.rglob('*.elc'):
        compiled.with_suffix('.el').unlink()

    return features


def _unpack(*, source: tuple[pathlib.Path, pathlib.PurePosixPath],
            install: pathlib.Path,
            builtin_features: bool) -> Optional[Set[str]]:
    if not isinstance(source, tuple):
        raise NotImplementedError(
            'in release mode only archive sources are supported')
    archive, prefix = source
    try:
        # Bazel sometimes creates the output directory; remove it so that
        # _unpack_archive works correctly.
        install.rmdir()
    except FileNotFoundError:
        pass
    _unpack_archive(archive, install, prefix=prefix)
    lisp = _glob_unique(install / 'share' / 'emacs' / '[0-9]*' / 'lisp')
    features = _builtin_features(lisp) if builtin_features else None
    return features


def _builtin_features(lisp: pathlib.Path) -> Set[str]:
    features = set()
    for source in lisp.rglob('*.el'):
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
    temp = pathlib.Path(tempfile.mkdtemp(prefix='emacs-unpack-'))
    shutil.unpack_archive(archive, temp)
    shutil.move(temp / prefix, dest, copy_function=shutil.copy)
    shutil.rmtree(temp, ignore_errors=True)
    _touch(dest, datetime.datetime(2020, 1, 1, tzinfo=datetime.timezone.utc))


def _touch(root: pathlib.Path, time: datetime.datetime) -> None:
    stamp = time.timestamp()
    for parent, _dirs, files in os.walk(root):
        path = pathlib.Path(parent)
        for file in files:
            try:
                os.utime(path / file, (stamp, stamp))
            except FileNotFoundError:
                # This can happen on Windows if the filename is too long, see
                # https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
                # and
                # https://docs.python.org/3/using/windows.html#removing-the-max-path-limitation.
                # Nothing we can do about it.
                pass


if __name__ == '__main__':
    main()
