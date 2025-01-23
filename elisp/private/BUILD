# Copyright 2025 Philipp Stephani
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@bazel_skylib//lib:selects.bzl", "selects")
load("@bazel_skylib//rules:diff_test.bzl", "diff_test")
load("@bazel_skylib//rules:expand_template.bzl", "expand_template")
load("//private:package_features.bzl", "PACKAGE_FEATURES")
load(":cc_defaults.bzl", "cc_defaults")
load(
    ":cc_launcher_config.bzl",
    "LAUNCHER_COPTS",
    "LAUNCHER_DEFINES",
    "LAUNCHER_FEATURES",
    "LAUNCHER_LINKOPTS",
)

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

bzl_library(
    name = "bootstrap",
    srcs = ["bootstrap.bzl"],
    deps = [":run_emacs"],
)

bzl_library(
    name = "compile",
    srcs = ["compile.bzl"],
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
    ],
    deps = [
        ":cc_launcher_config",
        ":load_path",
        ":run_emacs",
        "//elisp/common:elisp_info",
        "//private:defs",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "binary",
    srcs = ["binary.bzl"],
    visibility = ["//elisp:__pkg__"],
    deps = [
        ":cc_launcher",
        ":compile",
        "//private:defs",
        "@bazel_skylib//lib:collections",
    ],
)

bzl_library(
    name = "cc_launcher",
    srcs = ["cc_launcher.bzl"],
    visibility = ["//elisp/toolchains:__pkg__"],
    deps = [
        ":generated",
        "//private:defs",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "cc_launcher_config",
    srcs = ["cc_launcher_config.bzl"],
    visibility = [
        "//elisp:__pkg__",
        "//elisp/toolchains:__pkg__",
    ],
    deps = ["//private:defs"],
)

bzl_library(
    name = "cc_defaults",
    srcs = ["cc_defaults.bzl"],
    deps = ["//private:defs"],
)

bzl_library(
    name = "load_path",
    srcs = ["load_path.bzl"],
    visibility = ["//elisp:__pkg__"],
    deps = [
        "//private:defs",
        "@bazel_skylib//lib:paths",
    ],
)

expand_template(
    name = "expand",
    out = "generated.expanded.bzl",
    substitutions = {
        "[[chr]]": ", ".join(['"\\%o"' % i for i in range(0x100)]),
        "[[ord]]": ", ".join(['"\\%o": %d' % (i, i) for i in range(0x100)]),
    },
    template = "generated.template.bzl",
)

diff_test(
    name = "generated_test",
    timeout = "short",
    failure_message = """Please run

    bazel build //elisp/private:generated.expanded.bzl

and copy the generated file to //elisp/private:generated.bzl.""",
    file1 = "generated.bzl",
    file2 = "generated.expanded.bzl",
)

bzl_library(
    name = "module_config",
    srcs = ["module_config.bzl"],
    deps = [
        ":module_config_info",
        "//private:defs",
    ],
)

# gazelle:exclude generated.*.bzl

cc_defaults(
    name = "launcher_defaults",
    copts = LAUNCHER_COPTS,
    defines = LAUNCHER_DEFINES,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    visibility = [
        "//elisp:__pkg__",
        "//elisp/toolchains:__pkg__",
    ],
)

cc_defaults(
    name = "emacs_defaults",
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
    visibility = ["//elisp/toolchains:__pkg__"],
)

selects.config_setting_group(
    name = "gcc_or_clang",
    match_any = [
        "@rules_cc//cc/compiler:gcc",
        "@rules_cc//cc/compiler:clang",
    ],
    visibility = ["//visibility:private"],
)

bzl_library(
    name = "deps",
    srcs = ["deps.bzl"],
    deps = ["@bazel_skylib//lib:modules"],
)

# keep
bzl_library(
    name = "module_config_info",
    srcs = ["module_config_info.bzl"],
    visibility = ["//elisp:__pkg__"],
)

alias(
    name = "emacs_cc_toolchain",
    actual = select({
        "@platforms//os:windows": ":windows_cc_toolchain",
        "//conditions:default": "@rules_cc//cc:current_cc_toolchain",
    }),
    visibility = ["//elisp/toolchains:__pkg__"],
)

alias(
    name = "windows_cc_toolchain",
    actual = "@local_config_cc//:cc-compiler-x64_windows_mingw",
    target_compatible_with = [
        "@platforms//os:windows",
        "@platforms//cpu:x86_64",
    ],
)

bzl_library(
    name = "executable_only",
    srcs = ["executable_only.bzl"],
)

bzl_library(
    name = "generated",
    srcs = ["generated.bzl"],
)

bzl_library(
    name = "run_emacs",
    srcs = ["run_emacs.bzl"],
)
