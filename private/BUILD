# Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
)

licenses(["notice"])

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    visibility = [
        "//docs:__pkg__",
        "//elisp:__pkg__",
        "//emacs:__pkg__",
    ],
    deps = [
        ":deps_defs_bzl",
        "//elisp:builtin_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "repositories_bzl",
    srcs = ["repositories.bzl"],
    visibility = [
        "//docs:__pkg__",
        "//elisp:__pkg__",
    ],
)

bzl_library(
    name = "extensions_bzl",
    srcs = ["extensions.bzl"],
    deps = [":repositories_bzl"],
)

bzl_library(
    name = "deps_defs_bzl",
    srcs = ["@phst_rules_elisp_deps//:defs.bzl"],
)

selects.config_setting_group(
    name = "gcc_or_clang",
    match_any = [
        ":gcc",
        ":clang",
    ],
    visibility = ["//visibility:private"],
)

selects.config_setting_group(
    name = "gcc",
    match_any = [
        ":gcc_new",
        ":gcc_old",
    ],
    visibility = ["//visibility:private"],
)

config_setting(
    name = "gcc_new",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "gcc"},
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
    ],
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
    ],
)

config_setting(
    name = "clang",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "clang"},
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
    ],
)

config_setting(
    name = "msvc-cl",
    flag_values = {"@bazel_tools//tools/cpp:compiler": "msvc-cl"},
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
    ],
)
