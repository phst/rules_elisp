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

"""Functions for load path management.

This is an internal helper library for the Emacs Lisp Bazel rules.  Don’t rely
on it in any way outside the rule implementation."""

from collections.abc import Iterable
import pathlib

from elisp import runfiles

def add_path(run_files: runfiles.Runfiles, args: list[str],
             load_path: Iterable[pathlib.PurePosixPath]) -> None:
    """Add load path elements to the given args list."""
    runfiles_elc = pathlib.PurePosixPath(
        'phst_rules_elisp/elisp/runfiles/runfiles.elc')
    runfile_handler_installed = False
    for directory in load_path:
        try:
            args.append('--directory=' + str(run_files.resolve(directory)))
        except FileNotFoundError:
            if not runfile_handler_installed:
                file = run_files.resolve(runfiles_elc)
                args += [
                    '--load=' + str(file),
                    '--funcall=elisp/runfiles/install-handler',
                ]
                runfile_handler_installed = True
            args.append('--directory=/bazel-runfile:' + str(directory))
