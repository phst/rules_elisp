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
load("@bazel_skylib//lib:selects.bzl", "selects")
load("//elisp:util.bzl", "cc_defaults")

alias(
    name = "emacs",
    actual = ":emacs_27.2",
    visibility = ["//visibility:public"],
)

py_test(
    name = "emacs_test",
    size = "medium",
    timeout = "short",
    srcs = ["emacs_test.py"],
    data = [":emacs"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = ["//elisp:runfiles"],
)

alias(
    name = "module_header",
    actual = "@gnu_emacs_27.2//:module_header",
    visibility = ["//visibility:public"],
)

alias(
    name = "builtin_features",
    actual = "@gnu_emacs_27.2//:builtin_features",
    visibility = ["//gazelle:__pkg__"],
)

alias(
    name = "emacs_26.1",
    actual = "@gnu_emacs_26.1//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_26.2",
    actual = "@gnu_emacs_26.2//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_26.3",
    actual = "@gnu_emacs_26.3//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_27.1",
    actual = "@gnu_emacs_27.1//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_27.2",
    actual = "@gnu_emacs_27.2//:emacs",
    visibility = ["//visibility:public"],
)

py_binary(
    name = "build",
    srcs = ["build.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    visibility = ["//visibility:public"],
)

exports_files(srcs = ["launcher.template"])

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    deps = [
        "//elisp:builtin_bzl",
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

exports_files(
    ["defs.bzl"],
    visibility = ["//docs:__pkg__"],
)

# All versions of Emacs support these platforms.
selects.config_setting_group(
    name = "always_supported",
    match_any = [
        ":linux",
        ":freebsd",
        ":openbsd",
    ],
)

selects.config_setting_group(
    name = "windows",
    match_any = [
        ":windows_x86_32",
        ":windows_x86_64",
    ],
)

selects.config_setting_group(
    name = "macos_x86",
    match_any = [
        ":macos_x86_32",
        ":macos_x86_64",
    ],
)

selects.config_setting_group(
    name = "macos_arm",
    match_any = [
        ":macos_arm64",
        ":macos_aarch64",
    ],
)

config_setting(
    name = "linux",
    constraint_values = ["@platforms//os:linux"],
)

config_setting(
    name = "freebsd",
    constraint_values = ["@platforms//os:freebsd"],
)

config_setting(
    name = "openbsd",
    constraint_values = ["@platforms//os:openbsd"],
)

config_setting(
    name = "windows_x86_32",
    constraint_values = [
        "@platforms//os:windows",
        "@platforms//cpu:x86_32",
    ],
)

config_setting(
    name = "windows_x86_64",
    constraint_values = [
        "@platforms//os:windows",
        "@platforms//cpu:x86_64",
    ],
)

config_setting(
    name = "macos_x86_32",
    constraint_values = [
        "@platforms//os:macos",
        "@platforms//cpu:x86_32",
    ],
)

config_setting(
    name = "macos_x86_64",
    constraint_values = [
        "@platforms//os:macos",
        "@platforms//cpu:x86_64",
    ],
)

config_setting(
    name = "macos_aarch64",
    constraint_values = [
        "@platforms//os:macos",
        "@platforms//cpu:aarch64",
    ],
)

config_setting(
    name = "macos_arm64",
    constraint_values = [
        "@platforms//os:macos",
        "@platforms//cpu:arm64",
    ],
)

# https://docs.bazel.build/versions/4.0.0/platforms.html#more-expressive-constraints
constraint_setting(name = "incompatible_setting")

constraint_value(
    name = "incompatible",
    constraint_setting = ":incompatible_setting",
    visibility = [
        "@gnu_emacs_26.1//:__pkg__",
        "@gnu_emacs_26.2//:__pkg__",
        "@gnu_emacs_26.3//:__pkg__",
        "@gnu_emacs_27.1//:__pkg__",
        "@gnu_emacs_27.2//:__pkg__",
    ],
)

cc_defaults(
    name = "defaults",
    copts = [],
    defines = [
        "NO_SHLWAPI_ISOS",  # Work around https://bugs.gnu.org/48303
    ],
    linkopts = select({
        "//constraints:macos": [
            # Override the toolchain’s “-undefined dynamic” option so that the
            # configure script doesn’t incorrectly detect absent functions as
            # present.
            "-undefined",
            "error",
        ],
        "//conditions:default": [],
    }),
    visibility = [
        "@gnu_emacs_26.1//:__pkg__",
        "@gnu_emacs_26.2//:__pkg__",
        "@gnu_emacs_26.3//:__pkg__",
        "@gnu_emacs_27.1//:__pkg__",
        "@gnu_emacs_27.2//:__pkg__",
    ],
)
