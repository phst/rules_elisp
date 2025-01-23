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

exports_files(
    [
        # keep sorted
        "elisp_emacs_binary.bzl",
        "elisp_toolchain.bzl",
    ],
    visibility = ["//docs:__pkg__"],
)

bzl_library(
    name = "elisp_emacs_binary",
    srcs = ["elisp_emacs_binary.bzl"],
    visibility = ["//visibility:public"],
    deps = [
        "//elisp/private:cc_launcher",
        "//private:defs",
        "@rules_cc//cc:action_names_bzl",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "elisp_toolchain",
    srcs = ["elisp_toolchain.bzl"],
    visibility = ["//visibility:public"],
)
