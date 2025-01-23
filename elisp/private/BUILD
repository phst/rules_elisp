# Copyright 2025 Philipp Stephani
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("//private:defs.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

bzl_library(
    name = "bootstrap",
    srcs = ["bootstrap.bzl"],
    visibility = ["//visibility:private"],
    deps = ["//private:defs"],
)

bzl_library(
    name = "compile",
    srcs = ["compile.bzl"],
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
    ],
    deps = [
        ":load_path",
        "//elisp/common:elisp_info",
        "//private:defs",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "binary",
    srcs = ["binary.bzl"],
    visibility = ["//elisp:__pkg__"],
    deps = [
        ":cc_launcher",
        ":compile",
        "//private:defs",
        "@bazel_skylib//lib:collections",
    ],
)

bzl_library(
    name = "cc_launcher",
    srcs = ["cc_launcher.bzl"],
    visibility = ["//elisp/toolchains:__pkg__"],
    deps = [
        "//private:defs",
        "//private:generated",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "load_path",
    srcs = ["load_path.bzl"],
    visibility = ["//elisp:__pkg__"],
    deps = [
        "//private:defs",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "module_config",
    srcs = ["module_config.bzl"],
    visibility = ["//visibility:private"],
    deps = ["//private:defs"],
)

bzl_library(
    name = "executable_only",
    srcs = ["executable_only.bzl"],
    visibility = ["//elisp:__subpackages__"],
)
