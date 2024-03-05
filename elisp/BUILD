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
load("@bazel_skylib//rules:build_test.bzl", "build_test")
load("@rules_python//python:defs.bzl", "py_binary", "py_library", "py_test")
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
    "PACKAGE_FEATURES",
    "UNDEFINED_ERROR",
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
    name = "emacs_29.2_toolchain",
    toolchain = "emacs_29.2",
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

elisp_toolchain(
    name = "emacs_29.2",
    emacs = "//emacs:emacs_29.2",
)

exports_files(
    ["launcher.cc"],
    visibility = [
        # FIXME: Make internal once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
)

sh_binary(
    name = "local_emacs",
    srcs = ["local_emacs.sh"],
)

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    deps = [
        "//private:defs_bzl",
        "@bazel_features//:features",
        "@bazel_skylib//lib:collections",
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
        shell.quote("RULES_ELISP_LAUNCHER_ARGS=" + ", ".join([
            'RULES_ELISP_NATIVE_LITERAL(R"*(--wrapper=$(rlocationpath :wrap_stripped))*")',
            'RULES_ELISP_NATIVE_LITERAL("--mode=wrap")',
            'RULES_ELISP_NATIVE_LITERAL("--rule-tag=local")',
            'RULES_ELISP_NATIVE_LITERAL("--rule-tag=mytag")',
            'RULES_ELISP_NATIVE_LITERAL("--load-directory=phst_rules_elisp")',
            'RULES_ELISP_NATIVE_LITERAL("--data-file=phst_rules_elisp/elisp/main.h")',
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
    hdrs = ["main.h"],
    copts = COPTS + CXXOPTS,
    data = [":run_emacs_stripped"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_RUN_EMACS=R"*($(rlocationpath :run_emacs_stripped))*"'),
    ],
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:nullability",
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
    hdrs = ["main.h"],
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
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:nullability",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
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
        "@io_abseil_py//absl/flags",
        "@io_abseil_py//absl/testing:absltest",
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
        ":main.h",
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
    hdrs = ["main.h"],
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
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
    deps = [
        ":platform",
        ":process",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_absl//absl/base:nullability",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/types:span",
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
        "@io_abseil_py//absl/testing:absltest",
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
        "@com_google_absl//absl/hash",
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
    visibility = [
        # FIXME: Make internal once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
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
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
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
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
)

cc_defaults(
    name = "launcher_defaults",
    copts = LAUNCHER_COPTS,
    defines = LAUNCHER_DEFINES,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
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
            # See https://github.com/bazelbuild/bazel/issues/12313 why we need
            # to add additional quoting.
            shell.quote("-Wl,--version-script=$(execpath :module.lds)"),
            "-Wl,--no-undefined",
        ],
        "@platforms//os:windows": [
            "/DEF:$(execpath :module.def)",
        ],
        "@platforms//os:macos": [
            "-Wl,-exported_symbol,_emacs_module_init",
            "-Wl,-exported_symbol,_plugin_is_GPL_compatible",
        ],
    }) + UNDEFINED_ERROR,
    suffix = select({
        "@platforms//os:linux": ".so",
        "@platforms//os:windows": ".dll",
        "@platforms//os:macos": ".dylib",
    }),
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
)
