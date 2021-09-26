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

"""Functions for load path management.

This is an internal helper library for the Emacs Lisp Bazel rules.  Don’t rely
on it in any way outside the rule implementation."""

import os
import os.path
import pathlib
from typing import Iterable, List

from bazel_tools.tools.python.runfiles import runfiles

def add_path(run_files: runfiles._Runfiles, args: List[str],
             load_path: Iterable[pathlib.Path]) -> None:
    """Add load path elements to the given args list."""
    runfiles_elc = 'phst_rules_elisp/elisp/runfiles/runfiles.elc'
    runfile_handler_installed = False
    for directory in load_path:
        resolved_dir = run_files.Rlocation(os.fspath(directory))
        if resolved_dir:
            args.append('--directory=' + os.path.abspath(resolved_dir))
        else:
            if not runfile_handler_installed:
                file = os.path.abspath(run_files.Rlocation(runfiles_elc))
                args += [
                    '--load=' + file,
                    '--funcall=elisp/runfiles/install-handler',
                ]
                runfile_handler_installed = True
            args.append('--directory=/bazel-runfile:' + os.fspath(directory))
