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

from collections.abc import Iterable
import io
import pathlib
import shlex
import shutil
import subprocess
import sys
from typing import Optional


def _test(bazel: pathlib.Path, *opts: str,
          cwd: Optional[pathlib.Path] = None) -> None:
    args = [bazel, 'test'] + list(opts) + ['--', '//...']
    if cwd:
        print('cd', shlex.quote(str(cwd)), '&&', end=' ')
    print(_quote(args))
    subprocess.run(args, check=True, cwd=cwd)


# All potentially supported Emacs versions.
_VERSIONS = frozenset({'28.2', '29.4'})


def main() -> None:
    """Builds the project."""
    if isinstance(sys.stdout, io.TextIOWrapper):
        sys.stdout.reconfigure(encoding='utf-8', line_buffering=True)
    bazel = shutil.which('bazelisk') or shutil.which('bazel')
    if not bazel:
        raise FileNotFoundError('neither Bazelisk nor Bazel found')
    bazel = pathlib.Path(bazel)
    try:
        # Test both default toolchain and versioned toolchains.
        _test(bazel)
        for version in sorted(_VERSIONS):
            _test(bazel,
                  f'--extra_toolchains=//elisp:emacs_{version}_toolchain')
        _test(bazel, cwd=pathlib.Path('examples', 'ext'))
    except subprocess.CalledProcessError as ex:
        print(_quote(ex.cmd), 'failed with exit code', ex.returncode)
        sys.exit(ex.returncode)


def _quote(args: Iterable[str | pathlib.Path]) -> str:
    return shlex.join(map(str, args))


if __name__ == '__main__':
    main()
