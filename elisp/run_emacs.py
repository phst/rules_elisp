# Copyright 2021 Google LLC
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

from bazel_tools.tools.python.runfiles import runfiles

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--install', type=pathlib.PurePosixPath, required=True)
    parser.add_argument('--dump-mode', choices=('portable', 'unexec'),
                        required=True)
    parser.add_argument('argv', nargs='+')
    opts = parser.parse_args()
    run_files = runfiles.Create()
    install = pathlib.Path(os.path.abspath(
        run_files.Rlocation(str(opts.install))))
    emacs = install / 'bin' / 'emacs'
    shared = _glob_unique(install / 'share' / 'emacs' / '[0-9]*')
    etc = shared / 'etc'
    libexec = install / 'libexec'
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
    env.update(run_files.EnvVars())
    try:
        subprocess.run(executable=emacs, args=args, env=env, check=True)
    except subprocess.CalledProcessError as ex:
        if 0 < ex.returncode < 0x80:
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

if __name__ == '__main__':
    main()
