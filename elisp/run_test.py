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

"""Runs an elisp_test target.

This is an internal helper binary for the Emacs Lisp Bazel rules.  Don’t rely on
it in any way outside the rule implementation."""

import argparse
import os
import os.path
import pathlib
import subprocess
import sys
from typing import List
import urllib.parse

from phst_rules_elisp.elisp import load
from phst_rules_elisp.elisp import manifest
from phst_rules_elisp.elisp import runfiles

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--wrapper', type=pathlib.PurePosixPath, required=True)
    parser.add_argument('--mode', choices=('direct', 'wrap'), required=True)
    parser.add_argument('--rule-tag', action='append', default=[])
    parser.add_argument('--load-directory', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--load-file', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--data-file', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--skip-test', action='append', default=[])
    parser.add_argument('--skip-tag', action='append', default=[])
    parser.add_argument('argv', nargs='+')
    opts = parser.parse_args()
    orig_env = dict(os.environ)
    run_files = runfiles.Runfiles()
    emacs = run_files.resolve(opts.wrapper)
    args = [opts.argv[0]]
    with manifest.add(opts.mode, args) as manifest_file:
        args += ['--quick', '--batch', '--module-assertions']
        load.add_path(run_files, args, opts.load_directory)
        runner = run_files.resolve(
            pathlib.PurePosixPath('phst_rules_elisp/elisp/ert/runner.elc'))
        args.append('--load=' + str(runner))
        # Note that using equals signs for --test-source, --skip-test, and
        # --skip-tag doesn’t work.
        for file in opts.load_file:
            abs_name = run_files.resolve(file)
            args += ['--test-source', '/:' + _quote(str(abs_name))]
        for test in opts.skip_test:
            args += ['--skip-test', _quote(test)]
        for tag in opts.skip_tag:
            args += ['--skip-tag', _quote(tag)]
        args.append('--funcall=elisp/ert/run-batch-and-exit')
        if manifest_file:
            inputs = []  # type: List[pathlib.Path]
            outputs = []  # type: List[pathlib.Path]
            report_file = orig_env.get('XML_OUTPUT_FILE')
            if report_file:
                outputs.append(pathlib.Path(report_file))
            if orig_env.get('COVERAGE') == '1':
                coverage_manifest = orig_env.get('COVERAGE_MANIFEST')
                if coverage_manifest:
                    inputs.append(pathlib.Path(coverage_manifest))
                coverage_dir = orig_env.get('COVERAGE_DIR')
                if coverage_dir:
                    outputs.append(
                        pathlib.Path(coverage_dir) / 'emacs-lisp.dat')
            manifest.write(opts, inputs, outputs, manifest_file)
        args.extend(opts.argv[1:])
        env = dict(orig_env)
        env.update(run_files.environment())
        try:
            subprocess.run(executable=emacs, args=args, env=env, check=True)
        except subprocess.CalledProcessError as ex:
            if 0 < ex.returncode < 0x80:
                # Don’t print a stacktrace if Emacs exited with a non-zero exit
                # code.
                sys.exit(ex.returncode)
            raise

def _quote(arg: str) -> str:
    return urllib.parse.quote(arg) if _WINDOWS else arg

_WINDOWS = os.name == 'nt'

if __name__ == '__main__':
    main()
