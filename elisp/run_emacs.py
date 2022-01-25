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

"""Runs Emacs.

This is an internal helper binary for the Emacs Lisp Bazel rules.  Don’t rely on
it in any way outside the rule implementation."""

import argparse
import glob
import os
import os.path
import pathlib
import subprocess
import sys
from typing import Iterable, Tuple

from phst_rules_elisp.elisp import runfiles

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--runfiles_env', action='append', type=_env_var,
                        default=[])
    parser.add_argument('--install', type=pathlib.PurePosixPath, required=True)
    parser.add_argument('--dump-mode', choices=('portable', 'unexec'),
                        required=True)
    parser.add_argument('argv', nargs='+')
    opts = parser.parse_args()
    run_files = runfiles.Runfiles(dict(opts.runfiles_env))
    install = run_files.resolve(opts.install)
    exe_suffix = '.exe' if os.name == 'nt' else ''
    emacs = install / 'bin' / ('emacs' + exe_suffix)
    shared = _glob_unique(install / 'share' / 'emacs' / '[0-9]*')
    etc = shared / 'etc'
    libexec = install / 'libexec'
    # We need to set argv[0] to the original argv[0] because the binary will use
    # it to find its runfiles.  See
    # https://docs.google.com/document/d/e/2PACX-1vSDIrFnFvEYhKsCMdGdD40wZRBX3m3aZ5HhVj4CtHPmiXKDCxioTUbYsDydjKtFDAzER5eg7OjJWs3V/pub.ñ
    args = [opts.argv[0]]
    if opts.dump_mode == 'portable':
        dump = _glob_unique(libexec / 'emacs' / '*' / '*' / 'emacs.pdmp')
        args.append('--dump-file=' + str(dump))
    args.extend(opts.argv[1:])
    env = dict(os.environ,
               EMACSDATA=str(etc),
               EMACSDOC=str(etc),
               EMACSLOADPATH=str(shared / 'lisp'),
               EMACSPATH=str(libexec))
    # We don’t want the Python launcher to change the current working directory,
    # otherwise relative filenames will be all messed up.  See
    # https://github.com/bazelbuild/bazel/issues/7190.
    env.pop('RUN_UNDER_RUNFILES', None)
    env.update(run_files.environment())
    if os.name == 'nt':
        # On Windows, Emacs doesn’t support Unicode arguments or environment
        # variables.  Check here rather than sending over garbage.
        _check_codepage('argument', args)
        _check_codepage('environment variable name', env.keys())
        _check_codepage('environment variable value', env.values())
    try:
        subprocess.run(executable=str(emacs), args=args, env=env, check=True)
    except subprocess.CalledProcessError as ex:
        if 0 < ex.returncode < 0x100:
            # Don’t print a stacktrace if Emacs exited with a non-zero exit
            # code.
            sys.exit(ex.returncode)
        raise

def _glob_unique(pattern: pathlib.PurePath) -> pathlib.Path:
    # Don’t use pathlib’s globbing functions because we want to skip dotfiles.
    files = glob.glob(str(pattern))
    if not files:
        raise FileNotFoundError(f'no file matches {pattern}')
    if len(files) > 1:
        raise OSError(f'multiple files match {pattern}: {files}')
    return pathlib.Path(files[0])

def _check_codepage(description: str, values: Iterable[str]) -> None:
    for value in values:
        try:
            value.encode('ansi')
        except UnicodeEncodeError as ex:
            raise ValueError(
                f'can’t encode {description} “{value}” for Windows') from ex

def _env_var(arg: str) -> Tuple[str, str]:
    key, _, value = arg.partition('=')
    return key, value

if __name__ == '__main__':
    main()
