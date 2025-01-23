# Copyright 2020, 2021, 2023, 2024, 2025 Google LLC
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

# Tests for Emacs Lisp Bazel rules.  These tests are outside the ‘elisp’ package
# so that they can use external repositories loaded as development-only
# dependencies in our MODULE.bazel file, e.g. the Go rules.

load("//elisp:defs.bzl", "elisp_library", "elisp_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
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
