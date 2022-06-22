#!/usr/bin/env python3

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

"""Builds this project.

Mimics a trivial version of Make."""

import argparse
import functools
import io
import os
import pathlib
import platform
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from typing import (Callable, Dict, FrozenSet, Iterable, Mapping, Optional,
                    Sequence)

_Target = Callable[['Builder'], None]
_targets: Dict[str, _Target] = {}


def target(func: _Target) -> _Target:
    """Decorator to mark a function as a build target."""
    name = func.__name__
    @functools.wraps(func)
    def wrapper(self: 'Builder') -> None:
        print(f'building target {name}')
        func(self)
    _targets[name] = wrapper
    return wrapper


class Builder:
    """Builds the project."""

    def __init__(self, *,
                 bazel: pathlib.Path,
                 action_cache: Optional[pathlib.Path] = None,
                 repository_cache: Optional[pathlib.Path] = None) -> None:
        self._kernel = platform.system()
        self._cwd = pathlib.Path(os.getcwd())
        self._env = dict(os.environ)
        self._github = self._env.get('CI') == 'true'
        self._bazel_program = bazel
        self._workspace = self._info('workspace')
        self._action_cache = action_cache
        self._repository_cache = repository_cache

    def build(self, goals: Sequence[str]) -> None:
        """Builds the specified goals."""
        if not goals:
            raise ValueError('no goals provided')
        funcs = []
        for goal in goals:
            func = _targets.get(goal)
            if not func:
                raise KeyError(f'unknown goal {goal}')
            funcs.append(func)
        for func in funcs:
            func(self)

    @target
    def all(self) -> None:
        """Builds all targets."""
        self.generate()
        self.check()

    @target
    def check(self) -> None:
        """Builds and tests the project."""
        self.buildifier()
        self.nogo()
        # Test both default toolchain and versioned toolchains.
        self.test()
        self.versions()
        self.ext()

    @target
    def generate(self) -> None:
        """Generates files to be checked in."""
        self.compdb()
        self.coverage()

    @target
    def buildifier(self) -> None:
        """Checks that all BUILD files are formatted correctly."""
        self._bazel('run',
                    ['@com_github_bazelbuild_buildtools//buildifier',
                     '--mode=check', '--lint=warn', '-r', '--',
                     str(self._workspace)])

    @target
    def nogo(self) -> None:
        """Checks that there are no unwanted Go targets in public packages.

        We don’t want any Go rules in the public packages, as our users would
        have to depend on the Go rules then as well.
        """
        print('looking for unwanted Go targets in public packages')
        for directory in ('elisp', 'emacs'):
            for dirpath, _, filenames in os.walk(self._workspace / directory):
                for file in filenames:
                    file = pathlib.Path(dirpath) / file
                    text = file.read_text(encoding='utf-8')
                    if '@io_bazel_rules_go' in text:
                        raise ValueError('unwanted Go targets in {file} found')

    @target
    def test(self) -> None:
        """Runs the Bazel tests."""
        self._test()

    @target
    def versions(self) -> None:
        """Runs the Bazel tests for all supported Emacs versions."""
        for version in sorted(_versions()):
            self._test(f'--extra_toolchains=//elisp:emacs_{version}_toolchain')

    def _test(self, *args: str) -> None:
        self._bazel('test', ['//...'], options=['--test_output=errors'],
                    postfix_options=args)

    @target
    def ext(self) -> None:
        """Run the tests in the example workspace."""
        self._bazel('test', ['//...'], options=['--test_output=errors'],
                    cwd=self._workspace / 'examples' / 'ext')

    @target
    def compdb(self) -> None:
        """Generates a compilation database for clangd."""
        self._bazel('build', ['@com_grail_bazel_compdb//:files'])
        execroot = self._info('execution_root')
        generator = (execroot / 'external' / 'com_grail_bazel_compdb' /
                     'generate.py')
        args = [sys.executable, str(generator), '--'] + self._cache_options()
        env = dict(self._env,
                   BAZEL_COMPDB_BAZEL_PATH=str(self._bazel_program),
                   # Need to compile with Clang for clangd to work.
                   CC='clang')
        self._run(args, cwd=self._workspace, env=env)

    @target
    def coverage(self) -> None:
        """Generates a coverage report."""
        output = self._bazel('coverage', ['//...'], capture_stdout=True)
        files = re.findall(r'^  (/.+/coverage\.dat)$', output, re.MULTILINE)
        if not files:
            raise FileNotFoundError('no coverage files generated')
        temp_dir = None  # type: Optional[pathlib.Path]
        if self._kernel == 'Darwin':
            # Work around https://github.com/bazelbuild/bazel/issues/14969.  Run
            # the LCov merger binary again, filtering out macOS system headers.
            temp_dir = pathlib.Path(tempfile.mkdtemp(prefix='bazel-coverage-'))
            report = temp_dir / 'report.txt'
            output = temp_dir / 'coverage.dat'
            with report.open('xt') as stream:
                for file in files:
                    stream.write(os.path.abspath(file) + '\n')
            self._bazel('run', ['@bazel_tools//tools/test:lcov_merger',
                                '--reports_file=' + str(report),
                                '--output_file=' + str(output),
                                '--filter_sources=/Applications/.+'])
            files = [str(output)]
        directory = self._workspace / 'coverage-report'
        self._run(['genhtml', '--output-directory=' + str(directory),
                   '--branch-coverage', '--'] + files,
                  cwd=self._workspace)
        if temp_dir:
            shutil.rmtree(temp_dir)
        print(f'coverage report written to {directory}')

    @target
    def install(self) -> None:
        """Installs the Info manual."""
        self._bazel('build', ['//docs:rules_elisp.info'])
        bin_dir = self._info('bazel-bin')
        info_dir = pathlib.Path('/usr/local/share/info')
        src = bin_dir / 'docs' / 'rules_elisp.info'
        dest = info_dir / 'rules_elisp.info'
        self._run(['install', '-d', '--', str(info_dir)])
        self._run(['install', '-m', '0644', '--', str(src), str(dest)])
        self._run(['install-info', '--', str(dest), str(info_dir / 'dir')])

    def _bazel(self, command: str, targets: Iterable[str], *,
               options: Iterable[str] = (), postfix_options: Iterable[str] = (),
               cwd: Optional[pathlib.Path] = None,
               capture_stdout: bool = False) -> Optional[str]:
        args = [str(self._bazel_program), command]
        args.extend(options)
        args.extend(self._cache_options())
        args.extend(postfix_options)
        args.append('--')
        args.extend(targets)
        env = dict(self._env)
        if self._github:
            if self._kernel == 'Darwin':
                # We don’t need XCode, and using the Unix toolchain tends to be
                # less flaky.  See
                # https://github.com/bazelbuild/bazel/issues/14113#issuecomment-999794586.
                # Use the Unix toolchain only on Github to make coverage
                # generation work locally; see
                # https://github.com/bazelbuild/bazel/issues/14970.
                env['BAZEL_USE_CPP_ONLY_TOOLCHAIN'] = '1'
            # Hacks so that Bazel finds the right binaries on GitHub.  See
            # https://docs.github.com/en/actions/learn-github-actions/environment-variables#default-environment-variables.
            if self._kernel == 'Windows':
                env['BAZEL_SH'] = r'C:\msys64\usr\bin\bash.exe'
        return self._run(args, cwd=cwd, env=env, capture_stdout=capture_stdout)

    def _cache_options(self) -> Sequence[str]:
        opts = []
        if self._action_cache:
            opts.append('--disk_cache=' + str(self._action_cache))
        if self._repository_cache:
            opts.append('--repository_cache=' + str(self._repository_cache))
        return opts

    def _info(self, key: str) -> pathlib.Path:
        output = self._run([str(self._bazel_program), 'info', '--', key],
                           capture_stdout=True)
        assert output is not None
        return pathlib.Path(output.rstrip('\n'))

    def _run(self, args: Sequence[str], *,
             cwd: Optional[pathlib.Path] = None,
             env: Optional[Mapping[str, str]] = None,
             capture_stdout: bool = False) -> Optional[str]:
        print(*map(shlex.quote, args))
        result = subprocess.run(
            args, check=True, cwd=cwd or self._cwd, env=env or self._env,
            stdout=subprocess.PIPE if capture_stdout else None,
            encoding='utf-8')
        return result.stdout


def _versions() -> FrozenSet[str]:
    # All potentially supported Emacs versions.
    ret = {'27.1', '27.2', '28.1'}
    uname = platform.uname()
    if uname.system in ('Linux', 'Windows'):
        # GNU/Linux and Windows support all Emacs versions.
        pass
    elif uname.system == 'Darwin':
        if uname.machine != 'x86_64':
            # Apple Silicon doesn’t support Emacs 27.1.
            ret.remove('27.1')
    else:
        raise ValueError(f'unsupported kernel {uname.system}')
    return frozenset(ret)


def main() -> None:
    """Builds the project."""
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(encoding='utf-8', line_buffering=True)
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--bazel', type=_program, default='bazel')
    parser.add_argument('--action-cache', type=_cache_directory)
    parser.add_argument('--repository-cache', type=_cache_directory)
    parser.add_argument('goals', nargs='*', choices=sorted(_targets))
    args = parser.parse_args(sys.argv[1:] or ['all'])
    builder = Builder(bazel=args.bazel,
                      action_cache=args.action_cache,
                      repository_cache=args.repository_cache)
    try:
        builder.build(args.goals)
    except subprocess.CalledProcessError as ex:
        print(*map(shlex.quote, ex.cmd), 'failed with exit code', ex.returncode)
        sys.exit(ex.returncode)


def _program(name: str) -> pathlib.Path:
    file = shutil.which(name)
    if not file:
        raise FileNotFoundError(f'program {name} not found')
    return pathlib.Path(file)


def _cache_directory(arg: str) -> pathlib.Path:
    return pathlib.Path(arg).expanduser()


if __name__ == '__main__':
    main()
