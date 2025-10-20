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

load("@protobuf//bazel:proto_library.bzl", "proto_library")
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

proto_library(
    name = "test_proto",
    srcs = ["test.proto"],
)

elisp_proto_library(
    name = "test_elisp_proto",
    deps = [":test_proto"],
)

# TODO: Remove the next pragma once
# https://github.com/EngFlow/gazelle_cc/pull/83 is released.
# gazelle:lang go,proto,starlark,elisp

# gazelle:cc_generate_proto false
# gazelle:go_generate_proto false
