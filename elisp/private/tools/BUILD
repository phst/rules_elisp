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
load("@protobuf//bazel:cc_proto_library.bzl", "cc_proto_library")
load("@protobuf//bazel:proto_library.bzl", "proto_library")
load("@rules_cc//cc:cc_binary.bzl", "cc_binary")
load("@rules_cc//cc:cc_library.bzl", "cc_library")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp/private:bootstrap.bzl", "bootstrap")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_COPTS", "LAUNCHER_DEFINES", "LAUNCHER_DEPS", "LAUNCHER_FEATURES", "LAUNCHER_LINKOPTS")
load("//elisp/private:manual.bzl", "MAX_MANUAL_ADDITIONAL_INPUTS")
load("//private:cc_config.bzl", "COPTS", "CXXOPTS", "DEFINES", "FEATURES", "LINKOPTS")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

cc_binary(
    name = "build_emacs",
    srcs = ["build_emacs.cc"],
    copts = LAUNCHER_COPTS,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES,
    visibility = ["//elisp/toolchains:__pkg__"],
    deps = [
        ":copy",
        ":platform",
        ":system",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/base:log_severity",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/log:globals",
        "@abseil-cpp//absl/log:initialize",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/types:span",
    ],
)

cc_binary(
    name = "copy_tree",
    srcs = ["copy_tree.cc"],
    copts = LAUNCHER_COPTS,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES,
    visibility = ["//elisp/toolchains:__pkg__"],
    deps = [
        ":copy",
        ":platform",
        ":system",
        "@abseil-cpp//absl/base:log_severity",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/log:globals",
        "@abseil-cpp//absl/log:initialize",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
    ],
)

cc_binary(
    name = "builtin_features",
    srcs = ["builtin_features.cc"],
    copts = LAUNCHER_COPTS,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES,
    visibility = ["//elisp/toolchains:__pkg__"],
    deps = [
        ":builtin_features_cc_proto",
        ":platform",
        ":system",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/base:log_severity",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/log:globals",
        "@abseil-cpp//absl/log:initialize",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
        "@protobuf//src/google/protobuf/io",
        "@protobuf//src/google/protobuf/json",
    ],
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
        shell.quote("RULES_ELISP_BINARY=1"),
        shell.quote('RULES_ELISP_WRAPPER=RULES_ELISP_NATIVE_LITERAL("/bin/false")'),
        shell.quote("RULES_ELISP_MODE=rules_elisp::ToolchainMode::kWrap"),
        shell.quote("RULES_ELISP_TAGS="),
        shell.quote("RULES_ELISP_LOAD_PATH="),
        shell.quote("RULES_ELISP_LOAD_FILES="),
        shell.quote("RULES_ELISP_DATA_FILES="),
        shell.quote("RULES_ELISP_INTERACTIVE=false"),
        shell.quote("RULES_ELISP_INPUT_ARGS="),
        shell.quote("RULES_ELISP_OUTPUT_ARGS="),
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
        ":runfiles",
        ":strings",
        ":system",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/types:span",
    ],
)

cc_library(
    name = "binary",
    srcs = ["binary.cc"],
    hdrs = ["binary.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    visibility = [
        "//elisp:__pkg__",
        "//tests/integration/wrap:__pkg__",
    ],
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@rules_cc//cc/runfiles",  # keep
        ":load",
        ":manifest",
        ":numeric",
        ":platform",
        ":runfiles",
        ":system",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/base:core_headers",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/types:span",
    ],
)

# This is called “tst” and not “test” to keep Gazelle from thinking it’s a test
# instead of a library.  Same with run-tst.el.
cc_library(
    name = "tst",
    testonly = True,
    srcs = ["tst.cc"],
    hdrs = ["tst.h"],
    copts = COPTS + CXXOPTS,
    data = [":run-tst.elc"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_RUN_TST_ELC=R"*($(rlocationpath :run-tst.elc))*"'),
    ],
    visibility = ["//elisp:__pkg__"],
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@rules_cc//cc/runfiles",  # keep
        ":load",
        ":manifest",
        ":numeric",
        ":platform",
        ":runfiles",
        ":strings",
        ":system",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/time",
        "@abseil-cpp//absl/types:span",
    ],
)

# keep
elisp_library(
    name = "run_tst",
    testonly = True,
    srcs = ["run-tst.el"],
    visibility = ["//tests/tools:__pkg__"],
)

select_file(
    name = "run-tst.elc",
    testonly = True,
    srcs = ":run_tst",
    subpath = "/" + package_name() + "/run-tst.elc",
)

cc_library(
    name = "load",
    srcs = ["load.cc"],
    hdrs = ["load.h"],
    copts = COPTS + CXXOPTS,
    data = ["//elisp/runfiles:runfiles.elc"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_RUNFILES_ELC=R"*($(rlocationpath //elisp/runfiles:runfiles.elc))*"'),
    ],
    visibility = ["//tests/tools:__pkg__"],
    deps = [
        ":platform",
        ":runfiles",
        ":strings",
        ":system",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/types:span",
    ],
)

cc_library(
    name = "manifest",
    srcs = ["manifest.cc"],
    hdrs = ["manifest.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    visibility = ["//tests/tools:__pkg__"],
    deps = [
        ":manifest_cc_proto",
        ":platform",
        ":system",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/types:span",
        "@protobuf//:protobuf_lite",  # keep
        "@protobuf//src/google/protobuf/json",
    ],
)

cc_library(
    name = "copy",
    srcs = ["copy.cc"],
    hdrs = ["copy.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    deps = [
        ":platform",
        ":system",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
    ],
)

cc_library(
    name = "runfiles",
    srcs = ["runfiles.cc"],
    hdrs = ["runfiles.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    visibility = ["//tests/tools:__pkg__"],
    deps = [
        ":platform",
        ":strings",
        ":system",
        "@abseil-cpp//absl/base:nullability",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/log:die_if_null",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/types:span",
        "@rules_cc//cc/runfiles",  # keep
    ],
)

cc_library(
    name = "system",
    srcs = ["system.cc"],
    hdrs = ["system.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS + select({
        "//private:msvc_or_clang_cl": ["/DEFAULTLIB:Shell32.lib"],
        "//conditions:default": [],
    }),
    linkstatic = True,
    local_defines = DEFINES,
    visibility = ["//tests/tools:__pkg__"],
    deps = [
        ":numeric",
        ":platform",
        ":strings",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/base:core_headers",
        "@abseil-cpp//absl/base:nullability",
        "@abseil-cpp//absl/cleanup",
        "@abseil-cpp//absl/container:flat_hash_map",
        "@abseil-cpp//absl/container:hash_container_defaults",
        "@abseil-cpp//absl/log",
        "@abseil-cpp//absl/log:absl_check",
        "@abseil-cpp//absl/log:check",
        "@abseil-cpp//absl/log:die_if_null",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/strings:str_format",
        "@abseil-cpp//absl/time",
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
        "//tests/integration/wrap:__pkg__",
        "//tests/tools:__pkg__",
    ],
)

cc_library(
    name = "numeric",
    hdrs = ["numeric.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    visibility = ["//tests/tools:__pkg__"],
)

cc_library(
    name = "strings",
    srcs = ["strings.cc"],
    hdrs = ["strings.h"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    visibility = ["//tests/tools:__pkg__"],
    deps = [
        ":numeric",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/strings:str_format",
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
        "//tests/integration/wrap:__pkg__",
    ],
)

exports_files(
    [
        # keep sorted
        "binary.cc",
        "binary.h",
    ],
    visibility = [
        "//tests/integration:__pkg__",
        "//tests/integration/wrap:__pkg__",
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

cc_proto_library(
    name = "builtin_features_cc_proto",
    deps = [":builtin_features_proto"],
)

cc_proto_library(
    name = "manifest_cc_proto",
    deps = [":manifest_proto"],
)

# gazelle:elisp_generate_proto false
# gazelle:go_generate_proto false

proto_library(
    name = "builtin_features_proto",
    srcs = ["builtin_features.proto"],
)

proto_library(
    name = "manifest_proto",
    srcs = ["manifest.proto"],
)
