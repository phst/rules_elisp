# Copyright 2025 Philipp Stephani
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Defines the `mypy` aspect to run Mypy on the Python source files.

See https://github.com/theoremlp/rules_mypy/blob/main/readme.md#bzlmod-setup.
"""

load("@pip_types//:types.bzl", "types")
load("@rules_mypy//mypy:mypy.bzl", _mypy = "mypy")

visibility("private")

mypy = _mypy(
    types = types,
    color = False,
)
