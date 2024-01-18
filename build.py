#!/usr/bin/env python3

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

"""Builds this project.

Mimics a trivial version of Make."""

import argparse
from collections.abc import Callable, Iterable, Mapping, Sequence
import enum
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
from typing import Optional

_Target = Callable[['Builder'], None]
_targets: dict[str, _Target] = {}

_Bzlmod = enum.Enum('_Bzlmod', ['NO', 'YES', 'BOTH'])
_Capture = enum.Enum('_Capture', ['NONE', 'STDOUT', 'STDERR'])


def target(func: _Target) -> _Target:
    """Decorator to mark a function as a build target."""
    name = func.__name__
    @functools.wraps(func)
    def wrapper(self: 'Builder') -> None:
        print(f'building target {name}')
        func(self)
    _targets[name] = wrapper
    return wrapper


def _parse_version(string: str) -> tuple[int, int]:
    # https://www.gnu.org/prep/standards/html_node/_002d_002dversion.html
    match = re.match(r'.+ (\d+)\.(\d+)[^ ]*$', string, re.ASCII | re.MULTILINE)
    if not match:
        raise ValueError(f'invalid version string {string}')
    return int(match[1]), int(match[2])


class Builder:
    """Builds the project."""

    def __init__(self, *,
                 bazel: pathlib.Path,
                 action_cache: Optional[pathlib.Path] = None,
                 repository_cache: Optional[pathlib.Path] = None) -> None:
        self._bazel_program = bazel
        self._action_cache = action_cache
        self._repository_cache = repository_cache
        self._kernel = platform.system()
        self._cwd = pathlib.Path(os.getcwd())
        self._env = dict(os.environ)
        self._github = self._env.get('CI') == 'true'
        self._output_base = self._init_output_base()
        self._workspace = self._info('workspace')
        version = _parse_version(
            self._run([str(bazel), '--version'], capture=_Capture.STDOUT))
        # Older Bazel versions don’t support Bzlmod properly.
        self._bzlmod = _Bzlmod.BOTH if version >= (6, 3) else _Bzlmod.NO
        self._ignore_lockfile = version < (7, 0)

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
        self.license()
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
                     '--mode=check', '--lint=warn',
                     '--warnings=+native-py,+out-of-order-load', '-r', '--',
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
                        raise ValueError(f'unwanted Go targets in {file} found')

    @target
    def license(self) -> None:
        """Checks that all source files have a license header."""
        self._bazel('run',
                    ['@com_github_google_addlicense//:addlicense',
                     '--check',
                     '--ignore=**/coverage-report/**',
                     '--',
                     str(self._workspace)])

    @target
    def test(self) -> None:
        """Runs the Bazel tests."""
        self._test()

    @target
    def versions(self) -> None:
        """Runs the Bazel tests for all supported Emacs versions."""
        for version in sorted(_VERSIONS):
            self._test(f'--extra_toolchains=//elisp:emacs_{version}_toolchain')

    def _test(self, *args: str, cwd: Optional[pathlib.Path] = None) -> None:
        bzlmods = {
            _Bzlmod.NO: [False],
            _Bzlmod.YES: [True],
            _Bzlmod.BOTH: [False, True],
        }
        for bzlmod in bzlmods[self._bzlmod]:
            prefix = '' if bzlmod else 'no'
            options = [
                '--test_output=errors',
                f'--{prefix}enable_bzlmod',
            ]
            if bzlmod and self._github and not self._ignore_lockfile:
                options.append('--lockfile_mode=error')
            options.extend(args)
            self._bazel('test', ['//...'], options=options, cwd=cwd)

    @target
    def ext(self) -> None:
        """Run the tests in the example workspace."""
        self._test(cwd=self._workspace / 'examples' / 'ext')

    @target
    def compdb(self) -> None:
        """Generates a compilation database for clangd."""
        options = (
            '--enable_bzlmod',
            '--lockfile_mode=off',
            '--output_groups=-check_python',
        )
        args = ['@hedron_compile_commands//:refresh_all']
        args.extend(self._bazel_options())
        args.extend(options)
        self._bazel('run', args, options=options)

    @target
    def coverage(self) -> None:
        """Generates a coverage report."""
        directory = self._workspace / 'coverage-report'
        self._bazel('run',
                    ['@phst_bazelcov//:bazelcov',
                     f'--bazel={self._bazel_program}', f'--output={directory}'],
                    options=['--enable_bzlmod',
                             '--lockfile_mode=off'])
        print(f'coverage report written to {directory}')

    @target
    def lock(self) -> None:
        """Manually update MODULE.bazel.lock."""
        cwds = (
            self._workspace,
            self._workspace / 'examples' / 'ext',
        )
        for cwd in cwds:
            self._bazel(
                'build', ['//...'],
                options=['--nobuild',
                         '--enable_bzlmod',
                         '--lockfile_mode=update'],
                cwd=cwd)

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
               startup_options: Iterable[str] = (),
               options: Iterable[str] = (),
               cwd: Optional[pathlib.Path] = None,
               capture: _Capture = _Capture.NONE) -> Optional[str]:
        args = [str(self._bazel_program)]
        if self._output_base:
            args.append('--output_base=' + str(self._output_base))
        args.extend(startup_options)
        args.append(command)
        args.extend(self._bazel_options())
        args.extend(options)
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
            # Note that due to https://github.com/bazelbuild/bazel/issues/15919
            # we have to install MSYS2 in C:\Tools.
            if self._kernel == 'Windows':
                env['BAZEL_SH'] = r'C:\Tools\msys64\usr\bin\bash.exe'
        return self._run(args, cwd=cwd, env=env, capture=capture)

    def _bazel_options(self) -> Sequence[str]:
        opts = []
        if self._github:
            opts += [
                '--verbose_failures',
                '--experimental_convenience_symlinks=ignore',
            ]
        if self._action_cache:
            opts.append('--disk_cache=' + str(self._action_cache))
        if self._repository_cache:
            opts.append('--repository_cache=' + str(self._repository_cache))
        return opts

    def _info(self, key: str) -> pathlib.Path:
        output = self._bazel('info', [key], capture=_Capture.STDOUT)
        assert output is not None
        return pathlib.Path(output.rstrip('\n'))

    def _init_output_base(self) -> Optional[pathlib.Path]:
        if not self._github or platform.system() != 'Windows':
            return None
        # Work around https://github.com/protocolbuffers/protobuf/issues/12947.
        # See https://bazel.build/configure/windows#long-path-issues.
        base = pathlib.Path(
            tempfile.mkdtemp(prefix='ob-', dir=os.getenv('RUNNER_TEMP')))
        self._run(['SUBST', 'O:', str(base)])
        return pathlib.Path('O:\\')

    def _run(self, args: Sequence[str], *,
             cwd: Optional[pathlib.Path] = None,
             env: Optional[Mapping[str, str]] = None,
             capture: _Capture = _Capture.NONE) -> Optional[str]:
        print(*map(shlex.quote, args))
        result = subprocess.run(
            args, check=True, cwd=cwd or self._cwd, env=env or self._env,
            stdout=subprocess.PIPE if capture == _Capture.STDOUT else None,
            stderr=subprocess.PIPE if capture == _Capture.STDERR else None,
            encoding='utf-8')
        return result.stdout if capture == _Capture.STDOUT else result.stderr


# All potentially supported Emacs versions.
_VERSIONS = frozenset({'28.1', '28.2', '29.1'})


def main() -> None:
    """Builds the project."""
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(encoding='utf-8', line_buffering=True)
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--bazel', type=_program, default='bazel')
    parser.add_argument('--action-cache', type=pathlib.PurePosixPath)
    parser.add_argument('--repository-cache', type=pathlib.PurePosixPath)
    parser.add_argument('goals', nargs='*', default=['all'])
    args = parser.parse_args()
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


if __name__ == '__main__':
    main()
