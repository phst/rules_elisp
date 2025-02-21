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
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

alias(
    name = "emacs",
    actual = ":emacs_29.4",
    visibility = ["//visibility:public"],
)

alias(
    name = "module_header",
    actual = select({
        "@platforms//os:windows": "@gnu_emacs_windows_29.4//:module_header",
        "//conditions:default": "@gnu_emacs_29.4//:module_header",
    }),
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_28.2",
    actual = select({
        "@platforms//os:windows": "@gnu_emacs_windows_28.2//:emacs",
        "//conditions:default": "@gnu_emacs_28.2//:emacs",
    }),
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_29.4",
    actual = select({
        "@platforms//os:windows": "@gnu_emacs_windows_29.4//:emacs",
        "//conditions:default": "@gnu_emacs_29.4//:emacs",
    }),
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_30.1",
    actual = select({
        "@platforms//os:windows": "@gnu_emacs_windows_30.1//:emacs",
        "//conditions:default": "@gnu_emacs_30.1//:emacs",
    }),
    visibility = ["//visibility:public"],
)

bzl_library(
    name = "defs",
    srcs = ["defs.bzl"],
    visibility = ["//visibility:public"],
    deps = ["//elisp/toolchains:elisp_emacs_binary"],
)
