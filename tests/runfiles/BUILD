# Copyright 2020, 2021, 2024, 2025 Google LLC
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

load("@bazel_features//:features.bzl", "bazel_features")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp:elisp_test.bzl", "elisp_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

TEST_DATA = [
    "test.txt",
    "test-lib.elc",
    "test-manifest",
    "test-mapping",
] + (["testäα𝐴🐈'.txt"] if bazel_features.docs.utf8_enabled else [])

elisp_test(
    name = "runfiles_test",
    size = "small",
    srcs = ["runfiles-test.el"],
    args = ["%s=$(rlocationpath %s)" % (f, f) for f in TEST_DATA],
    data = TEST_DATA,
    deps = ["//elisp/runfiles"],
)

# keep
elisp_library(
    name = "test_lib",
    srcs = ["test-lib.el"],
    outs = ["test-lib.elc"],
)
