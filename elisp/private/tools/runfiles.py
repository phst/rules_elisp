# Copyright 2021-2023, 2025 Google LLC
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

"""Contains a class to access Bazel runfiles."""

from collections.abc import Mapping
import os
import pathlib
from typing import Optional

from python.runfiles import runfiles  # type: ignore[import-not-found]

class Runfiles:
    """Represents a set of Bazel runfiles."""

    def __init__(self, env: Optional[Mapping[str, str]] = None):
        impl = runfiles.Create(dict(env) if env else None)
        if not impl:
            raise ValueError(
                f'No runfiles implementation for environment {env} available')
        self._impl = impl

    def resolve(self, name: pathlib.PurePosixPath) -> pathlib.Path:
        """Resolves a runfile name to an absolute filename.

        Raises:
          FileNotFoundError if the runfile wasn’t found in the manifest
        """
        if not name:
            raise ValueError('Missing runfile name')
        if name.is_absolute():
            raise ValueError(f'Runfile name “{name}” is absolute')
        result = self._impl.Rlocation(str(name))
        if not result:
            raise FileNotFoundError(f'Runfile “{name}” not found')
        return pathlib.Path(os.path.abspath(result))

    def environment(self) -> Mapping[str, str]:
        """Returns an environment variable map."""
        return self._impl.EnvVars()
