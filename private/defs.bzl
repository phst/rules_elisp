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

"""Internal-only rules and functions.

These definitions are internal and subject to change without notice."""

visibility([
    # keep sorted
    "//",
    "//dev",
    "//docs",
    "//elisp",
    "//elisp/common",
    "//elisp/extensions",
    "//elisp/private",
    "//elisp/private/tools",
    "//elisp/proto",
    "//elisp/runfiles",
    "//elisp/toolchains",
    "//emacs",
    "//examples",
    "//gazelle",
    "//tests",
    "//tests/pkg",
])

# FIXME: This restriction is arbitrary; elisp_binary rules should accept any
# number of input files if necessary.
MAX_MANUAL_ADDITIONAL_INPUTS = 10
