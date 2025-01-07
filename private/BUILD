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

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@bazel_skylib//lib:selects.bzl", "selects")
load("@bazel_skylib//rules:diff_test.bzl", "diff_test")
load("@bazel_skylib//rules:expand_template.bzl", "expand_template")
load(":defs.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

bzl_library(
    name = "defs",
    srcs = ["defs.bzl"],
    visibility = [
        "//docs:__pkg__",
        "//elisp:__pkg__",
        "//emacs:__pkg__",
    ],
    deps = [
        ":generated",
        "@bazel_skylib//lib:paths",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
        "@rules_python//python:py_info_bzl",
    ],
)

bzl_library(
    name = "repositories",
    srcs = ["repositories.bzl"],
)

bzl_library(
    name = "extensions",
    srcs = ["extensions.bzl"],
    deps = ["@bazel_skylib//lib:modules"],
)

bzl_library(
    name = "generated",
    srcs = ["generated.bzl"],
)

expand_template(
    name = "expand",
    out = "generated.expanded.bzl",
    substitutions = {
        "[[chr]]": ", ".join(['"\\%o"' % i for i in range(0x100)]),
        "[[ord]]": ", ".join(['"\\%o": %d' % (i, i) for i in range(0x100)]),
    },
    template = "generated.template.bzl",
)

diff_test(
    name = "generated_test",
    timeout = "short",
    failure_message = """Please run

    bazel build //private:generated.expanded.bzl

and copy the generated file to //private:generated.bzl.""",
    file1 = "generated.bzl",
    file2 = "generated.expanded.bzl",
)

selects.config_setting_group(
    name = "gcc_or_clang",
    match_any = [
        ":gcc",
        "@rules_cc//cc/compiler:clang",
    ],
    visibility = ["//visibility:private"],
)

selects.config_setting_group(
    name = "gcc",
    match_any = [
        "@rules_cc//cc/compiler:gcc",
        ":gcc_old",
    ],
    visibility = ["//visibility:private"],
)

# Obsolete value for the C++ compiler flag, to be removed once
# https://github.com/bazelbuild/bazel/pull/16297 and
# https://github.com/bazelbuild/bazel/pull/20350 are in all supported Bazel
# versions.
config_setting(
    name = "gcc_old",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "compiler"},
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
        "//emacs:__pkg__",
    ],
)
