# Copyright 2020, 2021, 2023, 2024 Google LLC
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

# Tests for Emacs Lisp Bazel rules.  These tests are outside the //elisp package
# so that they can use external repositories loaded in our WORKSPACE file only,
# e.g. the Go rules.

load("//elisp:defs.bzl", "elisp_library", "elisp_test")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = ["-macos_default_link_flags"],
)

licenses(["notice"])

# This library verifies that libraries can have source files from unrelated
# packages.
elisp_library(
    name = "lib",
    srcs = ["//tests:test-lib.el"],
)

elisp_test(
    name = "lib_test",
    size = "small",
    srcs = ["test.el"],
    deps = [":lib"],
)
