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
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@bazel_skylib//rules:build_test.bzl", "build_test")
load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")
load("@rules_python//python:py_binary.bzl", "py_binary")
load("@rules_python//python:py_library.bzl", "py_library")
load("@rules_python//python:py_test.bzl", "py_test")
load(
    "//private:defs.bzl",
    "COPTS",
    "CXXOPTS",
    "DEFINES",
    "FEATURES",
    "LAUNCHER_COPTS",
    "LAUNCHER_DEFINES",
    "LAUNCHER_DEPS",
    "LAUNCHER_FEATURES",
    "LAUNCHER_LINKOPTS",
    "LINKOPTS",
    "MAX_MANUAL_ADDITIONAL_INPUTS",
    "PACKAGE_FEATURES",
    "bootstrap",
    "cc_defaults",
    "executable_only",
    "module_config",
)
load(":defs.bzl", "elisp_binary", "elisp_toolchain")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
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
    name = "emacs_28.2_toolchain",
    toolchain = "emacs_28.2",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_29.4_toolchain",
    toolchain = "emacs_29.4",
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
    emacs = "@local_emacs//:emacs",
    use_default_shell_env = True,
)

elisp_toolchain(
    name = "emacs_28.2",
    emacs = "//emacs:emacs_28.2",
)

elisp_toolchain(
    name = "emacs_29.4",
    emacs = "//emacs:emacs_29.4",
)

exports_files(
    ["launcher.cc"],
    visibility = ["//emacs:__pkg__"],
)

bzl_library(
    name = "defs",
    srcs = ["defs.bzl"],
    visibility = ["//docs:__pkg__"],
    deps = [
        "//private:defs",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:paths",
        "@protobuf//bazel/common:proto_common_bzl",
        "@protobuf//bazel/common:proto_info_bzl",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "extensions",
    srcs = ["extensions.bzl"],
    visibility = ["//docs:__pkg__"],
    deps = ["@bazel_skylib//lib:modules"],
)

cc_binary(
    name = "launcher",
    testonly = True,
    srcs = ["launcher.cc"],
    copts = LAUNCHER_COPTS,
    data = [":wrap_stripped"],
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_HEADER="elisp/binary.h"'),
        shell.quote("RULES_ELISP_LAUNCHER_ARGS=" + ", ".join([
            'RULES_ELISP_NATIVE_LITERAL(R"*(--wrapper=$(rlocationpath :wrap_stripped))*")',
            'RULES_ELISP_NATIVE_LITERAL("--mode=wrap")',
            'RULES_ELISP_NATIVE_LITERAL("--rule-tag=local")',
            'RULES_ELISP_NATIVE_LITERAL("--rule-tag=mytag")',
            'RULES_ELISP_NATIVE_LITERAL("--load-directory=phst_rules_elisp")',
            'RULES_ELISP_NATIVE_LITERAL("--data-file=phst_rules_elisp/elisp/binary.h")',
            'RULES_ELISP_NATIVE_LITERAL("--input-arg=2")',
            'RULES_ELISP_NATIVE_LITERAL("--output-arg=-1")',
        ])),
    ],
    deps = LAUNCHER_DEPS + [":binary"],
)

build_test(
    name = "launcher_test",
    targets = [":launcher"],
)

cc_library(
    name = "emacs",
    srcs = ["emacs.cc"],
    hdrs = ["emacs.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    visibility = ["//emacs:__pkg__"],
    deps = [
        ":platform",
        ":process",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/types:span",
        "@bazel_tools//tools/cpp/runfiles",
    ],
)

cc_library(
    name = "binary",
    srcs = ["binary.cc"],
    hdrs = ["binary.h"],
    copts = COPTS + CXXOPTS,
    data = [
        ":run_binary_stripped",
        "//elisp/runfiles:runfiles.elc",
    ],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_RUN_BINARY=R"*($(rlocationpath :run_binary_stripped))*"'),
        shell.quote('RULES_ELISP_BINARY_ARGS=RULES_ELISP_NATIVE_LITERAL(R"*(--runfiles-elc=$(rlocationpath //elisp/runfiles:runfiles.elc))*")'),
    ],
    deps = [
        ":platform",
        ":process",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/types:span",
        "@bazel_tools//tools/cpp/runfiles",
    ],
)

py_test(
    name = "binary_test",
    size = "medium",
    timeout = "short",
    srcs = ["binary_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote("--launcher=$(rlocationpath :launcher)"),
        shell.quote("--binary-cc=$(rlocationpath :binary.cc)"),
    ],
    data = [
        "binary.cc",
        ":launcher",
    ],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = [
        ":runfiles",
        "@abseil-py//absl/flags",
        "@abseil-py//absl/testing:absltest",
    ],
)

executable_only(
    name = "wrap_stripped",
    testonly = True,
    src = ":wrap",
)

py_binary(
    name = "wrap",
    testonly = True,
    srcs = ["wrap.py"],
    data = [
        ":binary.cc",
        ":binary.h",
        "//elisp/runfiles:runfiles.elc",
    ],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = ["//elisp:runfiles"],
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
    testonly = True,
    srcs = ["test.cc"],
    hdrs = ["binary.h"],
    copts = COPTS + CXXOPTS,
    data = [
        ":run_test_stripped",
        "//elisp/ert:runner.elc",
        "//elisp/runfiles:runfiles.elc",
    ],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_RUN_TEST=R"*($(rlocationpath :run_test_stripped))*"'),
        shell.quote("RULES_ELISP_TEST_ARGS=" + ", ".join([
            'RULES_ELISP_NATIVE_LITERAL(R"*(--runfiles-elc=$(rlocationpath //elisp/runfiles:runfiles.elc))*")',
            'RULES_ELISP_NATIVE_LITERAL(R"*(--runner-elc=$(rlocationpath //elisp/ert:runner.elc))*")',
        ])),
    ],
    deps = [
        ":platform",
        ":process",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/types:span",
        "@bazel_tools//tools/cpp/runfiles",
    ],
)

py_binary(
    name = "run_test",
    testonly = True,
    srcs = ["run_test.py"],
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
        "@abseil-py//absl/testing:absltest",
    ],
)

py_library(
    name = "load",
    srcs = ["load.py"],
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
        "@abseil-py//absl/testing:absltest",
    ],
)

py_library(
    name = "runfiles",
    srcs = ["runfiles.py"],
    srcs_version = "PY3",
    visibility = [
        "//elisp/proto:__pkg__",
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
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/base:core_headers",
        "@abseil-cpp//absl/base:nullability",
        "@abseil-cpp//absl/cleanup",
        "@abseil-cpp//absl/container:flat_hash_map",
        "@abseil-cpp//absl/hash",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/log:die_if_null",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/types:span",
        "@bazel_tools//tools/cpp/runfiles",
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
    visibility = ["//emacs:__pkg__"],
)

exports_files(
    [
        # keep sorted
        "defs.bzl",
        "extensions.bzl",
    ],
    visibility = ["//docs:__pkg__"],
)

# We can’t use “elisp_library” here for bootstrapping reasons.
bootstrap(
    name = "compile",
    src = "compile.el",
    out = "compile.elc",
)

build_test(
    name = "compile_test",
    targets = [":compile"],
)

elisp_binary(
    name = "export_org",
    src = "export-org.el",
    input_args = range(
        2,
        3 + MAX_MANUAL_ADDITIONAL_INPUTS,
    ),
    output_args = [1],
)

cc_defaults(
    name = "launcher_defaults",
    copts = LAUNCHER_COPTS,
    defines = LAUNCHER_DEFINES,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    visibility = ["//emacs:__pkg__"],
)

module_config(
    name = "module_config",
    srcs = select({
        "@platforms//os:linux": [":module.lds"],
        "@platforms//os:windows": [":module.def"],
        "@platforms//os:macos": [],
    }),
    linkopts = select({
        # Export only symbols that make up the module interface.
        "@platforms//os:linux": [
            "-Wl,--version-script=$(execpath :module.lds)",
            "-Wl,--no-undefined",
        ],
        "@platforms//os:windows": [
            "/DEF:$(execpath :module.def)",
        ],
        "@platforms//os:macos": [
            "-Wl,-exported_symbol,_emacs_module_init",
            "-Wl,-exported_symbol,_plugin_is_GPL_compatible",
        ],
    }),
    suffix = select({
        "@platforms//os:linux": ".so",
        "@platforms//os:windows": ".dll",
        "@platforms//os:macos": ".dylib",
    }),
)
