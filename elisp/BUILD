# Copyright 2020, 2021 Google LLC
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
load(":defs.bzl", "elisp_binary", "elisp_library", "elisp_toolchain")
load(":util.bzl", "COPTS")

toolchain_type(
    name = "toolchain_type",
    visibility = ["//visibility:public"],
)

toolchain(
    name = "hermetic_toolchain",
    toolchain = ":hermetic",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_26.1_toolchain",
    toolchain = "emacs_26.1",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_26.2_toolchain",
    toolchain = "emacs_26.2",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_26.3_toolchain",
    toolchain = "emacs_26.3",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_27.1_toolchain",
    toolchain = "emacs_27.1",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_27.2_toolchain",
    toolchain = "emacs_27.2",
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

elisp_toolchain(
    name = "emacs_26.1",
    emacs = "//emacs:emacs_26.1",
)

elisp_toolchain(
    name = "emacs_26.2",
    emacs = "//emacs:emacs_26.2",
)

elisp_toolchain(
    name = "emacs_26.3",
    emacs = "//emacs:emacs_26.3",
)

elisp_toolchain(
    name = "emacs_27.1",
    emacs = "//emacs:emacs_27.1",
)

elisp_toolchain(
    name = "emacs_27.2",
    emacs = "//emacs:emacs_27.2",
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
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
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
        "//docs:__pkg__",
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
        "//docs:__pkg__",
        "//emacs:__pkg__",
    ],
)

cc_library(
    name = "emacs",
    srcs = ["emacs.cc"],
    hdrs = ["emacs.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":file",
        ":options",
        ":process",
        ":status",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "binary",
    srcs = ["binary.cc"],
    hdrs = ["binary.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":algorithm",
        ":file",
        ":load",
        ":manifest",
        ":options",
        ":process",
        ":status",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "binary_test",
    size = "small",
    srcs = ["binary_test.cc"],
    copts = COPTS,
    data = [
        "binary.cc",
        "binary.h",
        "//tests/wrap",
    ],
    deps = [
        ":binary",
        ":options",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "test",
    testonly = 1,
    srcs = ["test.cc"],
    hdrs = ["test.h"],
    data = ["//elisp/ert:runner.elc"],
    visibility = ["//visibility:public"],
    deps = [
        ":algorithm",
        ":file",
        ":load",
        ":manifest",
        ":options",
        ":process",
        ":status",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "options",
    hdrs = ["options.h"],
    copts = COPTS,
    deps = ["@com_google_absl//absl/container:flat_hash_set"],
)

cc_library(
    name = "manifest",
    srcs = ["manifest.cc"],
    hdrs = ["manifest.h"],
    deps = [
        ":algorithm",
        ":file",
        ":options",
        ":status",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:optional",
        "@nlohmann_json//:json",
    ],
)

cc_library(
    name = "load",
    srcs = ["load.cc"],
    hdrs = ["load.h"],
    data = ["//elisp/runfiles:runfiles.elc"],
    deps = [
        ":process",
        ":status",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/utility",
    ],
)

cc_library(
    name = "process",
    srcs = ["process.cc"],
    hdrs = ["process.h"],
    deps = [
        ":file",
        ":options",
        ":status",
        ":str",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
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
        "@com_google_absl//absl/meta:type_traits",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/utility",
    ],
)

cc_test(
    name = "file_test",
    size = "small",
    srcs = ["file_test.cc"],
    copts = COPTS,
    data = ["test.txt"],
    deps = [
        ":file",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
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
    name = "algorithm",
    hdrs = ["algorithm.h"],
    copts = COPTS,
    deps = ["@com_google_absl//absl/algorithm:container"],
)

cc_library(
    name = "status",
    srcs = ["status.cc"],
    hdrs = ["status.h"],
    copts = COPTS,
    deps = [
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
    ],
)

exports_files(
    [
        "defs.bzl",
        "repositories.bzl",
    ],
    visibility = ["//docs:__pkg__"],
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

elisp_binary(
    name = "export_org",
    src = "export-org.el",
    input_args = [1],
    output_args = [2],
    visibility = ["//visibility:public"],
)
