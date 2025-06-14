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
load("@protobuf//bazel:proto_library.bzl", "proto_library")
load("@rules_python//python:py_test.bzl", "py_test")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//elisp:elisp_test.bzl", "elisp_test")
load("//elisp/proto:elisp_proto_library.bzl", "elisp_proto_library")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

elisp_test(
    name = "proto_test",
    size = "small",
    srcs = ["proto-test.el"],
    flaky = True,  # https://debbugs.gnu.org/46722
    deps = [
        ":test_elisp_proto",
        "//elisp/proto",
        "//elisp/proto:any_elisp_proto",
        "//elisp/proto:compiler_plugin_elisp_proto",
        "//elisp/proto:descriptor_elisp_proto",
        "//elisp/proto:duration_elisp_proto",
        "//elisp/proto:timestamp_elisp_proto",
        "//elisp/proto:type_elisp_proto",  # keep
        "//elisp/proto:wrappers_elisp_proto",
    ],
)

py_test(
    name = "module_test",
    srcs = ["module_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--cat=$(rlocationpath :cat)"),
    ],
    data = [":cat"],
    deps = [
        "//elisp/private/tools:runfiles",
        "@abseil-py//absl/flags",
        "@abseil-py//absl/testing:absltest",
    ],
)

elisp_binary(
    name = "cat",
    src = "cat.el",
    input_args = [2],
    output_args = [2],
    deps = ["//elisp/proto:module"],
)

proto_library(
    name = "test_proto",
    srcs = ["test.proto"],
)

elisp_proto_library(
    name = "test_elisp_proto",
    deps = [":test_proto"],
)

# gazelle:go_generate_proto false
# gazelle:resolve_regexp elisp ^google/protobuf/(\w+)\.proto$ //elisp/proto:${1}_elisp_proto
# gazelle:resolve_regexp elisp ^google/protobuf/(\w+)/(\w+)\.proto$ //elisp/proto:${1}_${2}_elisp_proto
