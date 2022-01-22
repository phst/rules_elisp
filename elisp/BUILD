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
load("@bazel_skylib//rules:build_test.bzl", "build_test")
load(":defs.bzl", "elisp_binary", "elisp_library", "elisp_toolchain")
load(":util.bzl", "cc_defaults")

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

# Shared C++ compilation options.
COPTS = [
    "-fno-exceptions",
    "-Werror",
    "-Wall",
    "-Wextra",
    "-Wconversion",
    "-Wsign-conversion",
    "-pedantic-errors",
    # GCC appears to treat some moves as redundant that are in fact necessary.
    "-Wno-redundant-move",
    "-D_GNU_SOURCE",
] + select({
    "//constraints:linux": [],
    "//constraints:macos": [],
    "//constraints:windows": [
        "-D_UNICODE",
        "-DUNICODE",
        "-DSTRICT",
        "-DNOMINMAX",
        "-DWIN32_LEAN_AND_MEAN",
    ],
})

LAUNCHER_FEATURES = select({
    "//constraints:windows": [
        # keep sorted
        "fully_static_link",
        "static_linking_mode",
    ],
    "//conditions:default": [],
})

LAUNCHER_COPTS = COPTS + select({
    "//constraints:windows": [
        "-mwin32",
        "-municode",
        "-mconsole",
    ],
    "//conditions:default": [],
})

LINKOPTS = select({
    "//constraints:windows": [
        "-limagehlp",  # https://stackoverflow.com/a/26513179
    ],
    "//conditions:default": [],
})

LAUNCHER_LINKOPTS = LINKOPTS + select({
    "//constraints:windows": [
        "-mwin32",
        "-municode",
        "-mconsole",
    ],
    "//conditions:default": [],
})

cc_library(
    name = "emacs",
    srcs = ["emacs.cc"],
    hdrs = ["emacs.h"],
    copts = COPTS,
    data = [":run_emacs"],
    linkopts = LINKOPTS,
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
        ":process",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
    ],
)

py_binary(
    name = "run_emacs",
    srcs = ["run_emacs.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [":runfiles"],
)

cc_library(
    name = "binary",
    srcs = ["binary.cc"],
    hdrs = ["binary.h"],
    copts = COPTS,
    data = [":run_binary"],
    linkopts = LINKOPTS,
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
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
    features = LAUNCHER_FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    deps = [
        ":binary",
        ":process",
        "@com_google_googletest//:gtest_main",
    ],
)

py_binary(
    name = "run_binary",
    srcs = ["run_binary.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [
        ":load",
        ":manifest",
        ":runfiles",
    ],
)

cc_library(
    name = "test",
    testonly = 1,
    srcs = ["test.cc"],
    hdrs = ["test.h"],
    copts = COPTS,
    data = [":run_test"],
    linkopts = LINKOPTS,
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

py_binary(
    name = "run_test",
    testonly = 1,
    srcs = ["run_test.py"],
    data = ["//elisp/ert:runner.elc"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [
        ":load",
        ":manifest",
        ":runfiles",
    ],
)

py_library(
    name = "manifest",
    srcs = ["manifest.py"],
    srcs_version = "PY3",
)

py_test(
    name = "manifest_test",
    size = "small",
    srcs = ["manifest_test.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [":manifest"],
)

py_library(
    name = "load",
    srcs = ["load.py"],
    data = ["//elisp/runfiles:runfiles.elc"],
    srcs_version = "PY3",
    deps = [":runfiles"],
)

py_test(
    name = "load_test",
    size = "small",
    srcs = ["load_test.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [
        ":load",
        ":runfiles",
    ],
)

py_library(
    name = "runfiles",
    srcs = ["runfiles.py"],
    srcs_version = "PY3",
    visibility = [
        "//emacs:__pkg__",
        "//examples:__pkg__",
        "//tests/wrap:__pkg__",
    ],
    deps = ["@bazel_tools//tools/python/runfiles"],
)

cc_library(
    name = "process",
    srcs = ["process.cc"],
    hdrs = ["process.h"],
    copts = COPTS,
    linkopts = LINKOPTS,
    deps = [
        ":platform",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "platform",
    hdrs = ["platform.h"],
    copts = COPTS,
    linkopts = LINKOPTS,
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

cc_defaults(
    name = "launcher_defaults",
    copts = LAUNCHER_COPTS,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    visibility = ["//visibility:public"],
)

exports_files(
    [
        "binary.cc",
        "binary.h",
    ],
    visibility = ["//tests/wrap:__pkg__"],
)
