# Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

load("@bazel_skylib//lib:shell.bzl", "shell")
load("@rules_python//python:py_test.bzl", "py_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

py_test(
    name = "binary_test",
    size = "medium",
    timeout = "short",
    srcs = ["binary_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote("--launcher=$(rlocationpath //elisp/private/tools:launcher)"),
        shell.quote("--binary-cc=$(rlocationpath //elisp/private/tools:binary.cc)"),
    ],
    data = [
        "//elisp/private/tools:binary.cc",
        "//elisp/private/tools:launcher",
    ],
    deps = [
        "//elisp/private/tools:runfiles",
        "@abseil-py//absl/flags",
        "@abseil-py//absl/testing:absltest",
    ],
)

py_test(
    name = "manifest_test",
    size = "small",
    srcs = ["manifest_test.py"],
    deps = [
        "//elisp/private/tools:manifest",
        "@abseil-py//absl/testing:absltest",
    ],
)
