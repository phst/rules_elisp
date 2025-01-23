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
load("@bazel_skylib//rules:common_settings.bzl", "bool_flag")
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
        "//elisp/private:__pkg__",
        "//elisp/proto:__pkg__",
        "//elisp/toolchains:__pkg__",
        "//emacs:__pkg__",
    ],
    deps = ["@bazel_skylib//lib:paths"],
)

bzl_library(
    name = "extensions",
    srcs = ["extensions.bzl"],
    deps = ["@bazel_skylib//lib:modules"],
)

selects.config_setting_group(
    name = "gcc_or_clang",
    match_any = [
        "@rules_cc//cc/compiler:gcc",
        "@rules_cc//cc/compiler:clang",
    ],
    visibility = ["//visibility:private"],
)

bool_flag(
    name = "treat_warnings_as_errors",
    build_setting_default = False,
)

config_setting(
    name = "treat_warnings_as_errors_enabled",
    flag_values = {":treat_warnings_as_errors": "true"},
)

# gazelle:ignore generated.*.bzl

bzl_library(
    name = "repositories",
    srcs = ["repositories.bzl"],
)
