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
load(
    "//elisp:defs.bzl",
    "elisp_binary",
    "elisp_cc_module",
    "elisp_library",
    "elisp_manual",
    "elisp_proto_library",
    "elisp_test",
)
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

elisp_library(
    name = "lib_1",
    srcs = ["lib-1.el"],
    data = ["data.txt"],
    deps = [
        ":lib_2",
        "//elisp/runfiles",
        "@phst_rules_elisp_example//:lib_4",
    ],
)

elisp_test(
    name = "lib_1_test",
    size = "small",
    srcs = ["lib-1-test.el"],
    data = ["lib-1-resources/test.txt"],
    skip_tests = ["lib-1-broken-test"],
    deps = [":lib_1"],
)

elisp_library(
    name = "lib_2",
    srcs = [
        "lib-2-a.el",
        "lib-2-b.el",
        "lib-2-pkg.el",
        ":module",
    ],
    fatal_warnings = False,
    load_path = ["."],
)

elisp_test(
    name = "lib_2_test",
    size = "small",
    srcs = ["lib-2-test.el"],
    skip_tags = ["skip"],
    deps = [":lib_2"],
)

elisp_library(
    name = "lib_3",
    srcs = ["subdir/lib-3.el"],
    load_path = ["/examples/subdir"],
)

elisp_test(
    name = "lib_3_test",
    size = "small",
    srcs = ["subdir/lib-3-test.el"],
    deps = [":lib_3"],
)

elisp_binary(
    name = "bin",
    src = "bin.el",
    deps = [":lib_1"],
)

elisp_cc_module(
    name = "module",
    srcs = ["module.c"],
    features = ["-default_link_libs"],
)

elisp_manual(
    name = "doc",
    src = "doc.org",
    out = "doc.texi",
    additional_inputs = ["config.org"],
)

py_test(
    name = "bin_test",
    size = "medium",
    timeout = "short",
    srcs = ["bin_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--bin=$(rlocationpath :bin)"),
    ],
    data = ["bin"],
    deps = [
        "//elisp/private/tools:runfiles",
        "@abseil-py//absl/flags",
        "@abseil-py//absl/testing:absltest",
    ],
)

# You can run this binary interactively: bazel run //examples:interactive
elisp_binary(
    name = "interactive",
    src = "interactive.el",
    interactive = True,
    deps = [
        ":example_elisp_proto",
        "//elisp/proto",
        "//elisp/runfiles",
    ],
)

proto_library(
    name = "example_proto",
    srcs = ["example.proto"],
)

elisp_proto_library(
    name = "example_elisp_proto",
    deps = [":example_proto"],
)

elisp_test(
    name = "proto_test",
    size = "small",
    srcs = ["proto-test.el"],
    deps = [
        ":example_elisp_proto",
        "//elisp/proto",
    ],
)

# gazelle:lang go,proto,starlark
# gazelle:go_generate_proto false
