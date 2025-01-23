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

"""Defines the global constant `PACKAGE_FEATURES`."""

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

# Features for all packages.  These may not contain select expressions.
# FIXME: Once we drop support for Bazel 7.0, move these features to the
# REPO.bazel files, and remove them from BUILD files.
PACKAGE_FEATURES = [
    "no_copts_tokenization",
    "layering_check",
    "parse_headers",
    "external_include_paths",
    # On Windows, Bazel generates incorrectly-escaped parameter files.  See
    # https://github.com/bazelbuild/bazel/issues/21029.
    "-compiler_param_file",
    "-macos_default_link_flags",
]
