# Copyright 2020, 2021, 2024-2026 Google LLC
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

load("@bazel_skylib//rules:select_file.bzl", "select_file")
load("@bazel_skylib//rules:write_file.bzl", "write_file")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp:elisp_test.bzl", "elisp_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_package_metadata = ["//:license"],
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
]

UNICODE = [
    # This filename contains only letters and can be checked in.
    "testäα𝐴.txt",
    # This filename contains non-letter characters and can’t be checked in; see
    # https://go.dev/ref/mod#zip-path-size-constraints.
    "testäα𝐴🐈'.txt",
]

write_file(
    name = "write_file",
    out = "testäα𝐴🐈'.txt",
    content = ["test file with non-letter characters in its name"],
)

elisp_test(
    name = "runfiles_test",
    size = "small",
    srcs = ["runfiles-test.el"],
    args = ["--%s=$(rlocationpath %s)" % (f, f) for f in TEST_DATA] + ["--unicode-%d=$(rlocationpath %s)" % (i, f) for i, f in enumerate(UNICODE, 1)],
    data = TEST_DATA + UNICODE,
    deps = ["//elisp/runfiles"],
)

# keep
elisp_library(
    name = "test_lib",
    srcs = ["test-lib.el"],
)

select_file(
    name = "test-lib.elc",
    srcs = ":test_lib",
    subpath = "/" + package_name() + "/test-lib.elc",
)
