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
load("@bazel_skylib//rules:build_test.bzl", "build_test")
load("@rules_python//python:defs.bzl", "py_binary", "py_library", "py_test")
load("//private:defs.bzl", "COPTS", "CXXOPTS", "DEFINES", "FEATURES", "LAUNCHER_COPTS", "LAUNCHER_DEFINES", "LAUNCHER_FEATURES", "LAUNCHER_LINKOPTS", "LINKOPTS", "bootstrap", "cc_defaults", "executable_only")
load(":defs.bzl", "elisp_binary", "elisp_toolchain")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
)

licenses(["notice"])

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
    name = "emacs_28.1_toolchain",
    toolchain = "emacs_28.1",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_28.2_toolchain",
    toolchain = "emacs_28.2",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_29.1_toolchain",
    toolchain = "emacs_29.1",
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
    name = "emacs_28.1",
    emacs = "//emacs:emacs_28.1",
)

elisp_toolchain(
    name = "emacs_28.2",
    emacs = "//emacs:emacs_28.2",
)

elisp_toolchain(
    name = "emacs_29.1",
    emacs = "//emacs:emacs_29.1",
)

exports_files([
    "binary_main.cc",
    "test_main.cc",
])

sh_binary(
    name = "local_emacs",
    srcs = ["local_emacs.sh"],
)

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    deps = [
        "//private:defs_bzl",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "repositories_bzl",
    srcs = ["repositories.bzl"],
    visibility = ["//docs:__pkg__"],
    deps = ["//private:repositories_bzl"],
)

bzl_library(
    name = "extensions_bzl",
    srcs = ["extensions.bzl"],
    deps = [
        ":repositories_bzl",
        "//private:repositories_bzl",
    ],
)

bzl_library(
    name = "builtin_bzl",
    srcs = ["@bazel_tools//tools:bzl_srcs"],
    visibility = [
        "//docs:__pkg__",
        "//emacs:__pkg__",
        "//private:__pkg__",
    ],
)

cc_library(
    name = "emacs",
    srcs = ["emacs.cc"],
    hdrs = ["emacs.h"],
    copts = COPTS + CXXOPTS,
    data = [":run_emacs_stripped"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        """PHST_RULES_ELISP_RUN_EMACS='R"*($(rlocationpath :run_emacs_stripped))*"'""",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/log",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
    ],
)

py_binary(
    name = "run_emacs",
    srcs = ["run_emacs.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [":runfiles"],
)

executable_only(
    name = "run_emacs_stripped",
    src = ":run_emacs",
)

cc_library(
    name = "binary",
    srcs = ["binary.cc"],
    hdrs = ["binary.h"],
    copts = COPTS + CXXOPTS,
    data = [":run_binary_stripped"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        """PHST_RULES_ELISP_RUN_BINARY='R"*($(rlocationpath :run_binary_stripped))*"'""",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/log",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
    ],
)

cc_test(
    name = "binary_test",
    size = "small",
    srcs = ["binary_test.cc"],
    args = ["$(rlocationpath //tests/wrap)"],
    copts = COPTS + CXXOPTS,
    data = [
        "binary.cc",
        "binary.h",
        "//tests/wrap",
    ],
    features = LAUNCHER_FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES,
    deps = [
        ":binary",
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/log:check",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/synchronization",
        "@com_google_googletest//:gtest",
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

executable_only(
    name = "run_binary_stripped",
    src = ":run_binary",
)

cc_library(
    name = "test",
    testonly = 1,
    srcs = ["test.cc"],
    hdrs = ["test.h"],
    copts = COPTS + CXXOPTS,
    data = [":run_test_stripped"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        """PHST_RULES_ELISP_RUN_TEST='R"*($(rlocationpath :run_test_stripped))*"'""",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/log",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
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

executable_only(
    name = "run_test_stripped",
    testonly = True,
    src = ":run_test",
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
    deps = [
        ":manifest",
        "@io_abseil_py//absl/testing:absltest",
    ],
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
        "@io_abseil_py//absl/testing:absltest",
    ],
)

py_library(
    name = "runfiles",
    srcs = ["runfiles.py"],
    srcs_version = "PY3",
    visibility = [
        "//emacs:__pkg__",
        "//examples:__pkg__",
        "//tests:__pkg__",
        "//tests/wrap:__pkg__",
    ],
    deps = ["@rules_python//python/runfiles"],
)

cc_library(
    name = "process",
    srcs = ["process.cc"],
    hdrs = ["process.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    deps = [
        ":platform",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/algorithm:container",
        "@com_google_absl//absl/base:core_headers",
        "@com_google_absl//absl/base:nullability",
        "@com_google_absl//absl/cleanup",
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/log",
        "@com_google_absl//absl/log:check",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "platform",
    hdrs = ["platform.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
)

exports_files(
    [
        # keep sorted
        "defs.bzl",
        "extensions.bzl",
        "repositories.bzl",
    ],
    visibility = [
        "//dev:__pkg__",
        "//docs:__pkg__",
    ],
)

# We can’t use “elisp_library” here for bootstrapping reasons.
bootstrap(
    name = "compile",
    src = "compile.el",
    out = "compile.elc",
    visibility = ["//visibility:public"],
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
    defines = LAUNCHER_DEFINES,
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
