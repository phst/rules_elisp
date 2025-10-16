# Copyright 2020-2025 Google LLC
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

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//private:package_features.bzl", "PACKAGE_FEATURES")
load(":elisp_test_suite.bzl", "elisp_test_suite")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

bzl_library(
    name = "elisp_test_suite",
    srcs = ["elisp_test_suite.bzl"],
    deps = [
        ":elisp_info_test",
        "//elisp:elisp_library",
        "@bazel_skylib//lib:partial",
        "@bazel_skylib//lib:unittest",
    ],
)

# keep
bzl_library(
    name = "elisp_info_test",
    srcs = ["elisp_info_test.bzl"],
    deps = [
        "//elisp/common:elisp_info",
        "@bazel_skylib//lib:unittest",
    ],
)

elisp_test_suite(name = "elisp_test")

elisp_binary(
    name = "empty",
    src = "empty.el",
    visibility = [
        "//tests/integration:__pkg__",
        "//tests/tools:__pkg__",
    ],
)
