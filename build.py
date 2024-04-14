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
from collections.abc import Callable, Iterable, Sequence
import functools
import io
import os
import pathlib
import shlex
import shutil
import subprocess
import sys
from typing import Optional

_Target = Callable[['Builder'], None]
_targets: dict[str, _Target] = {}


def target(func: _Target) -> _Target:
    """Decorator to mark a function as a build target."""
    name = func.__name__
    @functools.wraps(func)
    def wrapper(self: 'Builder') -> None:
        print(f'building target {name}')
        func(self)
    _targets[name] = wrapper
    return wrapper


def _run(args: Sequence[str | pathlib.Path], *,
         cwd: Optional[pathlib.Path] = None) -> None:
    if cwd:
        print('cd', shlex.quote(str(cwd)), '&&', end=' ')
    print(_quote(args))
    subprocess.run(args, check=True, cwd=cwd)


class Builder:
    """Builds the project."""

    def __init__(self, *,
                 profiles: Optional[pathlib.Path]) -> None:
        bazel = shutil.which('bazelisk') or shutil.which('bazel')
        if not bazel:
            raise FileNotFoundError('neither Bazelisk nor Bazel found')
        self._bazel = pathlib.Path(bazel)
        self._profiles = profiles
        self._github = os.getenv('CI') == 'true'
        self._workspace = pathlib.Path(
            os.getenv('BUILD_WORKSPACE_DIRECTORY')
            or pathlib.Path(__file__).parent
        ).absolute()

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
    def buildifier(self) -> None:
        """Checks that all BUILD files are formatted correctly."""
        _run([self._bazel, 'run', '--',
              '@buildifier_prebuilt//:buildifier',
              '--mode=check', '--lint=warn',
              '--warnings=+native-py,+out-of-order-load', '-r', '--',
              self._workspace])

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
        _run([self._bazel, 'run', '--',
              '@com_github_google_addlicense//:addlicense',
              '--check',
              '--ignore=**/coverage-report/**',
              '--',
              self._workspace])

    @target
    def test(self) -> None:
        """Runs the Bazel tests."""
        self._test(profile='test')

    @target
    def versions(self) -> None:
        """Runs the Bazel tests for all supported Emacs versions."""
        for version in sorted(_VERSIONS):
            self._test(f'--extra_toolchains=//elisp:emacs_{version}_toolchain',
                       profile=version)

    def _test(self, *args: str, profile: str,
              cwd: Optional[pathlib.Path] = None) -> None:
        for bzlmod in (True, False):
            prefix = '' if bzlmod else 'no'
            options = [f'--{prefix}enable_bzlmod']
            if bzlmod and self._profiles:
                self._profiles.mkdir(exist_ok=True)
                profile_file = self._profiles / (profile + '.json.gz')
                options += [
                    '--generate_json_trace_profile',
                    '--experimental_announce_profile_path',
                    '--profile=' + str(profile_file),
                ]
            options.extend(args)
            _run([self._bazel, 'test'] + options + ['--', '//...'], cwd=cwd)

    @target
    def ext(self) -> None:
        """Run the tests in the example workspace."""
        self._test(cwd=self._workspace / 'examples' / 'ext', profile='ext')

    @target
    def lock(self) -> None:
        """Manually update MODULE.bazel.lock."""
        cwds = (
            self._workspace,
            self._workspace / 'examples' / 'ext',
        )
        for cwd in cwds:
            _run([self._bazel, 'mod', 'deps', '--lockfile_mode=update'],
                 cwd=cwd)


# All potentially supported Emacs versions.
_VERSIONS = frozenset({'28.1', '28.2', '29.1', '29.2', '29.3'})


def main() -> None:
    """Builds the project."""
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(encoding='utf-8', line_buffering=True)
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('--profiles', type=_path)
    parser.add_argument('goals', nargs='*', default=['check'])
    args = parser.parse_args()
    builder = Builder(profiles=args.profiles)
    try:
        builder.build(args.goals)
    except subprocess.CalledProcessError as ex:
        print(_quote(ex.cmd), 'failed with exit code', ex.returncode)
        sys.exit(ex.returncode)


def _path(value: str) -> pathlib.Path:
    if not value:
        raise ValueError('missing file name')
    return pathlib.Path(value).absolute()


def _quote(args: Iterable[str | pathlib.Path]) -> str:
    return shlex.join(map(str, args))


if __name__ == '__main__':
    main()
