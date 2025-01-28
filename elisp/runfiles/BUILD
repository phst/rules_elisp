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

load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp:elisp_test.bzl", "elisp_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

elisp_library(
    name = "runfiles",
    srcs = ["runfiles.el"],
    outs = ["runfiles.elc"],
    visibility = ["//visibility:public"],
)

elisp_test(
    name = "runfiles_test",
    size = "small",
    srcs = ["runfiles-test.el"],
    data = [
        "test.txt",
        "test-lib.elc",
        "test-manifest",
        "test-mapping",
    ],
    deps = [":runfiles"],
)

elisp_library(
    name = "test_lib",
    testonly = True,
    srcs = ["test-lib.el"],
    outs = ["test-lib.elc"],
)
