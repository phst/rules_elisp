# Copyright 2020, 2021, 2023-2025 Google LLC
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

load("@phst_rules_elisp//elisp:elisp_library.bzl", "elisp_library")
load("@phst_rules_elisp//elisp:elisp_test.bzl", "elisp_test")

package(
    default_applicable_licenses = ["@phst_rules_elisp//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = [
        "layering_check",
        "parse_headers",
        # On Windows, Bazel generates incorrectly-escaped parameter files.  See
        # https://github.com/bazelbuild/bazel/issues/21029.
        "-compiler_param_file",
        "-macos_default_link_flags",
    ],
)

licenses(["notice"])

elisp_library(
    name = "lib_4",
    srcs = ["lib-4.el"],
    visibility = ["@phst_rules_elisp//examples:__pkg__"],
    deps = ["@dash"],
)

elisp_test(
    name = "lib_4_test",
    size = "small",
    srcs = ["lib-4-test.el"],
    deps = [":lib_4"],
)
