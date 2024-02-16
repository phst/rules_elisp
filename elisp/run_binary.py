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

"""Runs an elisp_binary target.

This is an internal helper binary for the Emacs Lisp Bazel rules.  Don’t rely on
it in any way outside the rule implementation."""

import argparse
from collections.abc import Iterable, Mapping, Sequence
import os
import os.path
import pathlib
import subprocess
import sys
from typing import Optional

from elisp import load
from elisp import manifest
from elisp import runfiles

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--env', action='append', type=_env_var, default=[])
    parser.add_argument('--runfiles-env', action='append', type=_env_var,
                        default=[])
    parser.add_argument('--wrapper', type=pathlib.PurePosixPath, required=True)
    parser.add_argument('--mode', choices=('direct', 'wrap'), required=True)
    parser.add_argument('--runfiles-elc', type=pathlib.PurePosixPath,
                        required=True)
    parser.add_argument('--rule-tag', action='append', default=[])
    parser.add_argument('--load-directory', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--load-file', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--data-file', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--interactive', action='store_true', default=False)
    parser.add_argument('--input-arg', action='append', type=int, default=[])
    parser.add_argument('--output-arg', action='append', type=int, default=[])
    parser.add_argument('argv', nargs='+')
    opts = parser.parse_args()
    env: dict[str, str] = dict(os.environ)
    run_files = runfiles.Runfiles(dict(opts.runfiles_env))
    emacs = run_files.resolve(opts.wrapper)
    args: list[str] = [str(emacs)]
    with manifest.add(opts.mode, args) as manifest_file:
        args.append('--quick')
        if not opts.interactive:
            args.append('--batch')
        load.add_path(run_files, args, opts.load_directory, opts.runfiles_elc)
        for file in opts.load_file:
            abs_name = run_files.resolve(file)
            args.append('--load=' + str(abs_name))
        if manifest_file:
            runfiles_dir = _runfiles_dir(env)
            input_files = _arg_files(opts.argv, runfiles_dir, opts.input_arg)
            output_files = _arg_files(opts.argv, runfiles_dir, opts.output_arg)
            manifest.write(opts, input_files, output_files, manifest_file)
        args.extend(opts.argv[1:])
        env.update(opts.env)
        env.update(run_files.environment())
        try:
            subprocess.run(args, env=env, check=True)
        except subprocess.CalledProcessError as ex:
            if 0 < ex.returncode < 0x100:
                # Don’t print a stacktrace if Emacs exited with a non-zero exit
                # code.
                sys.exit(ex.returncode)
            raise


def _runfiles_dir(env: Mapping[str, str]) -> Optional[pathlib.Path]:
    for var in ('RUNFILES_DIR', 'TEST_SRCDIR'):
        value = env.get(var)
        if value:
            return pathlib.Path(os.path.abspath(value))
    return None


def _arg_files(argv: Sequence[str], root: pathlib.Path,
               indices: Iterable[int]) -> Sequence[pathlib.PurePath]:
    argc = len(argv)
    result = []
    for i in sorted(indices):
        if i < 0:
            i += argc
        if 0 < i < argc:
            arg = argv[i]
            # File arguments are often quoted so that Emacs doesn’t interpret
            # them as special filenames.  Unquote them first.
            if arg.startswith('/:'):
                arg = arg[2:]
            file = pathlib.Path(os.path.abspath(arg))
            # Make filenames relative if possible.
            if root:
                try:
                    file = file.relative_to(root)
                except ValueError:
                    pass
            result.append(file)
    return tuple(result)


def _env_var(arg: str) -> tuple[str, str]:
    key, _, value = arg.partition('=')
    return key, value


if __name__ == '__main__':
    main()
