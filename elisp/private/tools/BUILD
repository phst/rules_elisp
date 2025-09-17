# Copyright 2020-2025 Google LLC
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

load("@bazel_skylib//lib:shell.bzl", "shell")
load("@bazel_skylib//rules:select_file.bzl", "select_file")
load("@rules_cc//cc:cc_binary.bzl", "cc_binary")
load("@rules_cc//cc:cc_library.bzl", "cc_library")
load("@rules_python//python:py_binary.bzl", "py_binary")
load("@rules_python//python:py_library.bzl", "py_library")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp/private:bootstrap.bzl", "bootstrap")
load("//elisp/private:cc_config.bzl", "COPTS", "CXXOPTS", "DEFINES", "FEATURES", "LINKOPTS")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_COPTS", "LAUNCHER_DEFINES", "LAUNCHER_DEPS", "LAUNCHER_FEATURES", "LAUNCHER_LINKOPTS")
load("//elisp/private:executable_only.bzl", "executable_only")
load("//elisp/private:manual.bzl", "MAX_MANUAL_ADDITIONAL_INPUTS")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

py_binary(
    name = "build_emacs",
    srcs = ["build_emacs.py"],
    visibility = ["//elisp/toolchains:__pkg__"],
)

cc_binary(
    name = "launcher",
    testonly = True,
    srcs = ["launcher.cc"],
    copts = LAUNCHER_COPTS,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_HEADER="elisp/private/tools/binary.h"'),
        shell.quote("RULES_ELISP_LAUNCHER_ARGS=" + ", ".join([
            'RULES_ELISP_NATIVE_LITERAL("--wrapper=/bin/false")',
            'RULES_ELISP_NATIVE_LITERAL("--mode=wrap")',
        ])),
    ],
    visibility = ["//tests/tools:__pkg__"],
    deps = LAUNCHER_DEPS + [":binary"],
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
    visibility = ["//elisp/toolchains:__pkg__"],
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@rules_cc//cc/runfiles",  # keep
        ":platform",
        ":process",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/types:span",
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
    visibility = [
        "//elisp:__pkg__",
        "//tests/wrap:__pkg__",
    ],
    deps = [
        ":platform",
        ":process",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/types:span",
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",
        "@rules_cc//cc/runfiles",
    ],
)

py_binary(
    name = "run_binary",
    srcs = ["run_binary.py"],
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
    # keep
    srcs = ["test.cc"],
    hdrs = ["binary.h"],
    copts = COPTS + CXXOPTS,
    data = [
        ":run_test_stripped",
        "//elisp/private/tools:run-test.elc",
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
            'RULES_ELISP_NATIVE_LITERAL(R"*(--run-test-elc=$(rlocationpath //elisp/private/tools:run-test.elc))*")',
        ])),
    ],
    visibility = ["//elisp:__pkg__"],
    # keep
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@rules_cc//cc/runfiles",  # keep
        ":platform",
        ":process",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/types:span",
    ],
)

py_binary(
    name = "run_test",
    testonly = True,
    srcs = ["run_test.py"],
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

# keep
elisp_library(
    name = "run_test_el",
    testonly = True,
    srcs = ["run-test.el"],
    visibility = ["//tests/tools:__pkg__"],
)

select_file(
    name = "run-test.elc",
    testonly = True,
    srcs = ":run_test_el",
    subpath = "/" + package_name() + "/run-test.elc",
)

py_library(
    name = "manifest",
    srcs = ["manifest.py"],
    visibility = ["//tests/tools:__pkg__"],
)

py_library(
    name = "load",
    srcs = ["load.py"],
    visibility = ["//tests/tools:__pkg__"],
    deps = [":runfiles"],
)

py_library(
    name = "runfiles",
    srcs = ["runfiles.py"],
    visibility = [
        "//elisp/proto:__pkg__",
        "//emacs:__pkg__",
        "//examples:__pkg__",
        "//tests/tools:__pkg__",
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
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@rules_cc//cc/runfiles",  # keep
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
        "//elisp:__pkg__",
        "//elisp/toolchains:__pkg__",
        "//tests/wrap:__pkg__",
    ],
)

# We can’t use “elisp_library” here for bootstrapping reasons.
bootstrap(
    name = "compile",
    src = "compile.el",
    out = "compile.elc",
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
        "//tests/tools:__pkg__",
    ],
)

elisp_binary(
    name = "gen_proto",
    src = "gen-proto.el",
    visibility = ["//elisp/proto:__pkg__"],
    deps = [
        "//elisp/proto",  # keep
    ],
)

elisp_binary(
    name = "gen_proto_bundle",
    src = "gen-proto-bundle.el",
    output_args = [1],
    visibility = ["//elisp/proto:__pkg__"],
)

elisp_binary(
    name = "export_org",
    src = "export-org.el",
    input_args = range(
        2,
        3 + MAX_MANUAL_ADDITIONAL_INPUTS,
    ),
    output_args = [1],
    visibility = ["//elisp:__pkg__"],
)

exports_files(
    ["launcher.cc"],
    visibility = [
        "//elisp:__pkg__",
        "//elisp/toolchains:__pkg__",
        "//tests/wrap:__pkg__",
    ],
)

exports_files(
    [
        # keep sorted
        "binary.cc",
        "binary.h",
    ],
    visibility = [
        "//tests/tools:__pkg__",
        "//tests/wrap:__pkg__",
    ],
)

exports_files(
    [
        # keep sorted
        "module.def",
        "module.lds",
    ],
    visibility = ["//elisp:__pkg__"],
)
