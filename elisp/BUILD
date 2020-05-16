# Copyright 2020 Google LLC
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
load("@bazel_skylib//rules:build_test.bzl", "build_test")
load(":defs.bzl", "elisp_library", "elisp_toolchain")
load(":defs_test.bzl", "elisp_test_suite")
load(":util.bzl", "COPTS")

toolchain_type(
    name = "toolchain_type",
    visibility = ["//visibility:public"],
)

# Restrict the hermetic toolchain to GNU/Linux for now because Bazel produces a
# broken binary on macOS.  Revisit once we have Emacs 27 with the portable
# dumper.
toolchain(
    name = "hermetic_toolchain",
    exec_compatible_with = ["@platforms//os:linux"],
    target_compatible_with = ["@platforms//os:linux"],
    toolchain = ":hermetic",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "local_toolchain",
    toolchain = ":local",
    toolchain_type = ":toolchain_type",
)

elisp_toolchain(
    name = "hermetic",
    emacs = "//emacs",
)

elisp_toolchain(
    name = "local",
    emacs = ":local_emacs",
    use_default_shell_env = True,
)

exports_files([
    "compile.el",
    "binary.template",
    "test.template",
])

sh_binary(
    name = "local_emacs",
    srcs = ["local_emacs.sh"],
)

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    deps = [
        ":util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "repositories_bzl",
    srcs = ["repositories.bzl"],
)

bzl_library(
    name = "util_bzl",
    srcs = ["util.bzl"],
    visibility = [
        "//documentation:__pkg__",
        "//emacs:__pkg__",
    ],
    deps = [
        ":builtin_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "builtin_bzl",
    srcs = ["@bazel_tools//tools:bzl_srcs"],
    visibility = [
        "//documentation:__pkg__",
        "//emacs:__pkg__",
    ],
)

elisp_test_suite(name = "elisp_test")

cc_library(
    name = "exec",
    srcs = ["exec.cc"],
    hdrs = ["exec.h"],
    copts = COPTS,
    visibility = ["//visibility:public"],
    deps = [
        ":file",
        ":status",
        ":str",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/base",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:optional",
        "@com_google_absl//absl/types:span",
        "@com_google_absl//absl/utility",
        "@nlohmann_json//:json",
    ],
)

cc_test(
    name = "exec_test",
    srcs = ["exec_test.cc"],
    copts = COPTS,
    data = [
        "exec.h",
        "//tests/wrap",
    ],
    deps = [
        ":exec",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "file",
    srcs = ["file.cc"],
    hdrs = ["file.h"],
    copts = COPTS,
    deps = [
        ":status",
        ":str",
        "@com_google_absl//absl/base",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/memory",
        "@com_google_absl//absl/meta:type_traits",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/utility",
    ],
)

cc_test(
    name = "file_test",
    srcs = ["file_test.cc"],
    copts = COPTS,
    data = ["test.txt"],
    deps = [
        ":file",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:optional",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "str",
    hdrs = ["str.h"],
    copts = COPTS,
)

cc_library(
    name = "status",
    srcs = ["status.cc"],
    hdrs = ["status.h"],
    copts = COPTS,
    deps = [
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:variant",
    ],
)

exports_files(
    [
        "defs.bzl",
        "repositories.bzl",
    ],
    visibility = ["//documentation:__pkg__"],
)

# Make sure compile.el can itself be byte-compiled.  We don’t use the compiled
# output for bootstrapping reasons.
elisp_library(
    name = "compile",
    srcs = ["compile.el"],
)

build_test(
    name = "compile_test",
    targets = [":compile"],
)
