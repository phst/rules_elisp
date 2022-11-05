# Copyright 2020, 2021, 2022 Google LLC
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
load("@bazel_skylib//rules:common_settings.bzl", "string_flag")
load("@bazel_tools//tools/python:toolchain.bzl", "py_runtime_pair")
load(":defs.bzl", "python_runtime")

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    visibility = [
        "//docs:__pkg__",
        "//elisp:__pkg__",
        "//emacs:__pkg__",
    ],
    deps = [
        "//elisp:builtin_bzl",
        "@bazel_skylib//lib:paths",
        "@bazel_skylib//rules:common_settings",
    ],
)

string_flag(
    name = "python_interpreter",
    build_setting_default = "",
)

python_runtime(
    name = "python_runtime",
    fallback_toolchain = "@bazel_tools//tools/python:_autodetecting_py_runtime_pair",
    interpreter = ":python_interpreter",
)

py_runtime_pair(
    name = "python_runtime_pair",
    py3_runtime = ":python_runtime",
)

toolchain(
    name = "python_toolchain",
    toolchain = ":python_runtime_pair",
    toolchain_type = "@bazel_tools//tools/python:toolchain_type",
)
