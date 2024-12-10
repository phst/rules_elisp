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
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@rules_python//python:defs.bzl", "py_binary", "py_test")
load("//private:defs.bzl", "PACKAGE_FEATURES", "cc_defaults")

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

py_test(
    name = "emacs_test",
    size = "medium",
    timeout = "short",
    srcs = ["emacs_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--emacs=$(rlocationpath :emacs)"),
    ],
    data = [":emacs"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [
        "//elisp:runfiles",
        "@io_abseil_py//absl/flags",
        "@io_abseil_py//absl/testing:absltest",
    ],
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
    name = "builtin_features",
    actual = select({
        "@platforms//os:windows": "@gnu_emacs_windows_29.4//:builtin_features",
        "//conditions:default": "@gnu_emacs_29.4//:builtin_features",
    }),
    visibility = ["//gazelle:__pkg__"],
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

py_binary(
    name = "build",
    srcs = ["build.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    visibility = [
        # FIXME: Make private once we drop support for Bazel 7.0.
        "//visibility:public",
    ],
)

bzl_library(
    name = "defs",
    srcs = ["defs.bzl"],
    deps = [
        "//private:defs",
        "@bazel_skylib//lib:paths",
        "@rules_cc//cc:action_names_bzl",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

exports_files(
    ["defs.bzl"],
    visibility = ["//docs:__pkg__"],
)

alias(
    name = "cc_toolchain",
    actual = select({
        "@platforms//os:windows": ":windows_cc_toolchain",
        "//conditions:default": "@rules_cc//cc:current_cc_toolchain",
    }),
    visibility = [
        # FIXME: Make private once we drop support for Bazel 7.0.
        "@gnu_emacs_28.2//:__pkg__",
        "@gnu_emacs_29.4//:__pkg__",
        "@gnu_emacs_windows_28.2//:__pkg__",
        "@gnu_emacs_windows_29.4//:__pkg__",
    ],
)

alias(
    name = "windows_cc_toolchain",
    actual = "@local_config_cc//:cc-compiler-x64_windows_mingw",
    target_compatible_with = [
        "@platforms//os:windows",
        "@platforms//cpu:x86_64",
    ],
)

cc_defaults(
    name = "defaults",
    copts = [],
    defines = [],
    features = [
        "-treat_warnings_as_errors",
        "-layering_check",
        "-parse_headers",
        "-module_maps",
        # Never instrument Emacs itself for coverage collection.  It doesn’t
        # hurt, but leads to needless reinstall actions when switching between
        # “bazel test” and “bazel coverage”.
        "-coverage",
        # Don’t link against the C++ standard library, as Emacs is pure C code.
        "-default_link_libs",
    ],
    linkopts = [],
    visibility = [
        # FIXME: Make private once we drop support for Bazel 7.0.
        "@gnu_emacs_28.2//:__pkg__",
        "@gnu_emacs_29.4//:__pkg__",
        "@gnu_emacs_windows_28.2//:__pkg__",
        "@gnu_emacs_windows_29.4//:__pkg__",
    ],
)
