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
load("//private:defs.bzl", "FEATURES", "PACKAGE_FEATURES", "cc_defaults")
load(":defs.bzl", "emacs_binary")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

alias(
    name = "emacs",
    actual = ":emacs_29.2",
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

cc_library(
    name = "module_header",
    srcs = ["emacs-module.h"],
    features = FEATURES + ["-default_link_libs"],
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
    name = "emacs_28.1",
    srcs = ["@phst_rules_elisp_deps//emacs-28.1:srcs"],
    readme = "@phst_rules_elisp_deps//emacs-28.1:readme",
    visibility = ["//visibility:public"],
)

emacs_binary(
    name = "emacs_28.2",
    srcs = ["@phst_rules_elisp_deps//emacs-28.2:srcs"],
    readme = "@phst_rules_elisp_deps//emacs-28.2:readme",
    visibility = ["//visibility:public"],
)

emacs_binary(
    name = "emacs_29.1",
    srcs = ["@phst_rules_elisp_deps//emacs-29.1:srcs"],
    readme = "@phst_rules_elisp_deps//emacs-29.1:readme",
    visibility = ["//visibility:public"],
)

emacs_binary(
    name = "emacs_29.2",
    srcs = ["@phst_rules_elisp_deps//emacs-29.2:srcs"],
    builtin_features = "builtin_features.json",
    module_header = "emacs-module.h",
    readme = "@phst_rules_elisp_deps//emacs-29.2:readme",
    visibility = ["//visibility:public"],
)

py_binary(
    name = "build",
    srcs = ["build.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
)

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

alias(
    name = "cc_toolchain",
    actual = select({
        "@platforms//os:windows": ":windows_cc_toolchain",
        "//conditions:default": "@bazel_tools//tools/cpp:current_cc_toolchain",
    }),
)

cc_toolchain_suite(
    name = "windows_cc_toolchain",
    target_compatible_with = ["@platforms//os:windows"],
    toolchains = {
        "x64_windows": "@local_config_cc//:cc-compiler-x64_windows_mingw",
        "x64_windows|mingw-gcc": "@local_config_cc//:cc-compiler-x64_windows_mingw",
    },
)

cc_defaults(
    name = "defaults",
    copts = [],
    defines = [],
    features = [
        "-treat_warnings_as_errors",
        # Never instrument Emacs itself for coverage collection.  It doesn’t
        # hurt, but leads to needless reinstall actions when switching between
        # “bazel test” and “bazel coverage”.
        "-coverage",
        # Don’t link against the C++ standard library, as Emacs is pure C code.
        "-default_link_libs",
    ],
    linkopts = select({
        "@platforms//os:macos": [
            # Override the toolchain’s “-undefined dynamic_lookup” option so
            # that the configure script doesn’t incorrectly detect absent
            # functions as present.
            "-undefined",
            "error",
        ],
        "//conditions:default": [],
    }),
)
