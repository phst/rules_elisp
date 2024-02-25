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

"""Runs an elisp_test target.

This is an internal helper binary for the Emacs Lisp Bazel rules.  Don’t rely on
it in any way outside the rule implementation."""

import argparse
import logging
import os
import os.path
import pathlib
import signal
import subprocess
import sys
import time
import urllib.parse

from elisp import load
from elisp import manifest
from elisp import runfiles

def main() -> None:
    """Main function."""
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--wrapper', type=pathlib.PurePosixPath, required=True)
    parser.add_argument('--mode', choices=('direct', 'wrap'), required=True)
    parser.add_argument('--runfiles-elc', type=pathlib.PurePosixPath,
                        required=True)
    parser.add_argument('--runner-elc', type=pathlib.PurePosixPath,
                        required=True)
    parser.add_argument('--rule-tag', action='append', default=[])
    parser.add_argument('--load-directory', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--load-file', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--data-file', action='append',
                        type=pathlib.PurePosixPath, default=[])
    parser.add_argument('--skip-test', action='append', default=[])
    parser.add_argument('--skip-tag', action='append', default=[])
    parser.add_argument('--module-assertions', action='store_true',
                        default=False)
    parser.add_argument('argv', nargs='+')
    opts = parser.parse_args()
    # Be a bit more verbose for tests, since Bazel will only show output on
    # explicit request.
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s %(levelname)s %(name)s %(message)s')
    env: dict[str, str] = dict(os.environ)
    run_files = runfiles.Runfiles()
    emacs = run_files.resolve(opts.wrapper)
    args: list[str] = [str(emacs)]
    with manifest.add(opts.mode, args) as manifest_file:
        args += ['--quick', '--batch', '--no-build-details']
        if opts.module_assertions:
            args.append('--module-assertions')
        load.add_path(run_files, args, opts.load_directory, opts.runfiles_elc)
        runner = run_files.resolve(opts.runner_elc)
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
        args.extend(opts.argv[1:])
        env.update(run_files.environment())
        if manifest_file:
            inputs: list[pathlib.Path] = []
            outputs: list[pathlib.Path] = []
            report_file = env.get('XML_OUTPUT_FILE')
            if report_file:
                outputs.append(pathlib.Path(report_file))
            if env.get('COVERAGE') == '1':
                coverage_manifest = env.get('COVERAGE_MANIFEST')
                if coverage_manifest:
                    coverage_manifest = pathlib.Path(coverage_manifest)
                    _fix_coverage_manifest(coverage_manifest, run_files)
                    inputs.append(coverage_manifest)
                coverage_dir = env.get('COVERAGE_DIR')
                if coverage_dir:
                    outputs.append(
                        pathlib.Path(coverage_dir) / 'emacs-lisp.dat')
            manifest.write(opts, inputs, outputs, manifest_file)
        test_srcdir = os.getenv('TEST_SRCDIR')
        if not test_srcdir:
            raise ValueError('environment variable TEST_SRCDIR not set')
        test_workspace = os.getenv('TEST_WORKSPACE')
        if not test_workspace:
            # TEST_WORKSPACE is technically optional (see
            # https://bazel.build/reference/test-encyclopedia#initial-conditions),
            # but in practice always set.
            raise ValueError('environment variable TEST_WORKSPACE not set')
        cwd = pathlib.Path(test_srcdir, test_workspace)
        timeout_secs = None
        kwargs = {}
        if _WINDOWS:
            # On Windows, the Bazel test runner doesn’t gracefully kill the test
            # process, see https://github.com/bazelbuild/bazel/issues/12684.  We
            # work around this by creating a new process group and sending
            # CTRL + BREAK slightly before Bazel kills us.
            timeout_str = env.get('TEST_TIMEOUT') or None
            if timeout_str:
                # Lower the timeout to account for infrastructure overhead.
                timeout_secs = int(timeout_str) - 2
            flags = subprocess.CREATE_NEW_PROCESS_GROUP  # pylint: disable=line-too-long  # pytype: disable=module-attr
            kwargs['creationflags'] = flags
        # We can’t use subprocess.run on Windows because it terminates the
        # subprocess using TerminateProcess on timeout, giving it no chance to
        # clean up after itself.
        with subprocess.Popen(args, env=env, cwd=cwd, **kwargs) as process:
            try:
                process.communicate(timeout=timeout_secs)
            except subprocess.TimeoutExpired:
                # Since we pass a None timeout on Unix systems, we should get
                # here only on Windows.
                assert _WINDOWS
                _logger.warning('test timed out, sending CTRL + BREAK')
                signum = signal.CTRL_BREAK_EVENT  # pylint: disable=no-member,line-too-long  # pytype: disable=module-attr
                process.send_signal(signum)
                _logger.info('waiting for Bazel to kill this process')
                # We want timeouts to be reflected as actual timeout results in
                # Bazel, so we force a Bazel-level timeout by sleeping for a
                # long time.
                time.sleep(20)
                # If Bazel hasn’t killed us, exit anyway.
                _logger.warning('Bazel failed to kill this process')
                sys.exit(0xFF)
            returncode = process.wait()
        if returncode:
            # Don’t print a stacktrace if Emacs exited with a non-zero exit
            # code.
            sys.exit(returncode)


def _quote(arg: str) -> str:
    return urllib.parse.quote(arg) if _WINDOWS else arg


def _fix_coverage_manifest(manifest_file: pathlib.Path,
                           run_files: runfiles.Runfiles) -> None:
    """Try to look up inaccessible files in the coverage manifest as runfiles.

    We do this here so that the Emacs Lisp code doesn’t have to depend on the
    runfiles library.
    """
    files = manifest_file.read_text('iso-8859-1').splitlines()
    edited = False
    for i, file in enumerate(files):
        file = pathlib.Path(file)
        if not file.is_absolute() and not file.exists():
            try:
                file = run_files.resolve(
                    pathlib.PurePosixPath(file.as_posix()))
                file.stat()  # make sure file exists
                files[i] = file.as_posix()
                edited = True
            except FileNotFoundError:
                _logger.warning('instrumented file %s not found', file)
    if edited:
        with manifest_file.open(mode='w', encoding='iso-8859-1',
                                newline='\n') as stream:
            for file in files:
                stream.write(file + '\n')


_WINDOWS = os.name == 'nt'
_logger = logging.getLogger('elisp.run_test')


if __name__ == '__main__':
    main()
