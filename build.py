#!/usr/bin/env python3

# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

    def __init__(self) -> None:
        bazel = shutil.which('bazelisk') or shutil.which('bazel')
        if not bazel:
            raise FileNotFoundError('neither Bazelisk nor Bazel found')
        self._bazel = pathlib.Path(bazel)
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
        # Test both default toolchain and versioned toolchains.
        self.test()
        self.versions()
        self.ext()

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
        options = []
        options.extend(args)
        _run([self._bazel, 'test'] + options + ['--', '//...'], cwd=cwd)

    @target
    def ext(self) -> None:
        """Run the tests in the example workspace."""
        self._test(cwd=self._workspace / 'examples' / 'ext')


# All potentially supported Emacs versions.
_VERSIONS = frozenset({'28.2', '29.4'})


def main() -> None:
    """Builds the project."""
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(encoding='utf-8', line_buffering=True)
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument('goals', nargs='*', default=['check'])
    args = parser.parse_args()
    builder = Builder()
    try:
        builder.build(args.goals)
    except subprocess.CalledProcessError as ex:
        print(_quote(ex.cmd), 'failed with exit code', ex.returncode)
        sys.exit(ex.returncode)


def _quote(args: Iterable[str | pathlib.Path]) -> str:
    return shlex.join(map(str, args))


if __name__ == '__main__':
    main()
