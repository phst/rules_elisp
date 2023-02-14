# Copyright 2020, 2021, 2022, 2023 Google LLC
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
load("//private:defs.bzl", "cc_defaults")
load(":defs.bzl", "emacs_binary")

alias(
    name = "emacs",
    actual = ":emacs_28.2",
    visibility = ["//visibility:public"],
)

py_test(
    name = "emacs_test",
    size = "medium",
    timeout = "short",
    srcs = ["emacs_test.py"],
    args = [
        # FIXME: Use rlocationpath once we drop support for Bazel older than
        # version 5.4.0.
        "--emacs=$(rootpath :emacs)",
    ],
    data = [":emacs"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = ["//elisp:runfiles"],
)

cc_library(
    name = "module_header",
    srcs = ["emacs-module.h"],
    includes = ["."],
    linkstatic = True,
    visibility = ["//visibility:public"],
)

filegroup(
    name = "builtin_features",
    srcs = ["builtin_features.json"],
    visibility = ["//gazelle:__pkg__"],
)

emacs_binary(
    name = "emacs_27.1",
    srcs = ["@gnu_emacs_27.1//:srcs"],
    readme = "@gnu_emacs_27.1//:readme",
    target_compatible_with = select({
        ":always_supported": [],
        ":macos_arm": [":incompatible"],
        "//conditions:default": [":incompatible"],
    }),
    visibility = ["//visibility:public"],
)

emacs_binary(
    name = "emacs_27.2",
    srcs = ["@gnu_emacs_27.2//:srcs"],
    readme = "@gnu_emacs_27.2//:readme",
    target_compatible_with = select({
        ":always_supported": [],
        ":macos_arm": [],
        "//conditions:default": [":incompatible"],
    }),
    visibility = ["//visibility:public"],
)

emacs_binary(
    name = "emacs_28.1",
    srcs = ["@gnu_emacs_28.1//:srcs"],
    readme = "@gnu_emacs_28.1//:readme",
    target_compatible_with = select({
        ":always_supported": [],
        ":macos_arm": [],
        "//conditions:default": [":incompatible"],
    }),
    visibility = ["//visibility:public"],
)

emacs_binary(
    name = "emacs_28.2",
    srcs = ["@gnu_emacs_28.2//:srcs"],
    builtin_features = "builtin_features.json",
    module_header = "emacs-module.h",
    readme = "@gnu_emacs_28.2//:readme",
    target_compatible_with = select({
        ":always_supported": [],
        ":macos_arm": [],
        "//conditions:default": [":incompatible"],
    }),
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
        "//private:defs_bzl",
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
        # keep sorted
        ":freebsd",
        ":linux",
        ":macos_x86",
        ":openbsd",
        ":windows",
    ],
)

selects.config_setting_group(
    name = "windows",
    match_any = [
        # keep sorted
        ":windows_x86_32",
        ":windows_x86_64",
    ],
)

selects.config_setting_group(
    name = "macos_x86",
    match_any = [
        # keep sorted
        ":macos_x86_32",
        ":macos_x86_64",
    ],
)

selects.config_setting_group(
    name = "macos_arm",
    match_any = [
        # keep sorted
        ":macos_aarch64",
        ":macos_arm64",
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
        # keep sorted
        "@platforms//cpu:x86_64",
        "@platforms//os:windows",
    ],
)

config_setting(
    name = "macos_x86_32",
    constraint_values = [
        # keep sorted
        "@platforms//cpu:x86_32",
        "@platforms//os:macos",
    ],
)

config_setting(
    name = "macos_x86_64",
    constraint_values = [
        # keep sorted
        "@platforms//cpu:x86_64",
        "@platforms//os:macos",
    ],
)

config_setting(
    name = "macos_aarch64",
    constraint_values = [
        # keep sorted
        "@platforms//cpu:aarch64",
        "@platforms//os:macos",
    ],
)

config_setting(
    name = "macos_arm64",
    constraint_values = [
        # keep sorted
        "@platforms//cpu:arm64",
        "@platforms//os:macos",
    ],
)

# https://bazel.build/extending/platforms#expressive-constraints
constraint_setting(name = "incompatible_setting")

constraint_value(
    name = "incompatible",
    constraint_setting = ":incompatible_setting",
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
)
