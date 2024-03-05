# Copyright 2021, 2022, 2023, 2024 Google LLC
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

"""Runs Emacs.

This is an internal helper binary for the Emacs Lisp Bazel rules.  Don’t rely on
it in any way outside the rule implementation."""

import argparse
from collections.abc import Generator, Iterable
import contextlib
import os
import os.path
import pathlib
import platform
import shutil
import subprocess
import sys
import tempfile

from elisp import runfiles

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--install', type=pathlib.PurePosixPath, required=True)
    parser.add_argument('argv', nargs='+')
    opts = parser.parse_args()
    run_files = runfiles.Runfiles()
    install = opts.install
    emacs = run_files.resolve(install / 'emacs.exe')
    etc = run_files.resolve(install / 'etc')
    libexec = run_files.resolve(install / 'libexec')
    dump = run_files.resolve(install / 'emacs.pdmp')
    lisp = run_files.resolve(install / 'lisp')
    with _shorten(dump) as dump:
        args = [str(emacs), '--dump-file=' + str(dump)] + opts.argv[1:]
        env = dict(os.environ,
                   EMACSDATA=str(etc),
                   EMACSDOC=str(etc),
                   EMACSLOADPATH=str(lisp),
                   EMACSPATH=str(libexec))
        env.update(run_files.environment())
        if _WINDOWS:
            # On Windows, Emacs doesn’t support Unicode arguments or environment
            # variables.  Check here rather than sending over garbage.
            _check_codepage('argument', args)
            _check_codepage('environment variable name', env.keys())
            _check_codepage('environment variable value', env.values())
        try:
            subprocess.run(args, env=env, check=True)
        except subprocess.CalledProcessError as ex:
            if 0 < ex.returncode < 0x100:
                # Don’t print a stacktrace if Emacs exited with a non-zero exit
                # code.
                sys.exit(ex.returncode)
            raise


# https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation
_MAX_PATH: int = 260


@contextlib.contextmanager
def _shorten(filename: pathlib.Path) -> Generator[pathlib.Path, None, None]:
    if not _WINDOWS or len(str(filename)) < _MAX_PATH:
        yield filename
    else:
        with tempfile.TemporaryDirectory() as directory:
            short = pathlib.Path(directory) / 'emacs.pdmp'
            shutil.copyfile(filename, short)
            yield short


def _check_codepage(description: str, values: Iterable[str]) -> None:
    for value in values:
        try:
            value.encode('ansi')
        except UnicodeEncodeError as ex:
            raise ValueError(
                f'can’t encode {description} “{value}” for Windows') from ex


_WINDOWS = platform.system() == 'Windows'


if __name__ == '__main__':
    main()
