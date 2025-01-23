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
load("//private:defs.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

bzl_library(
    name = "bootstrap",
    srcs = ["bootstrap.bzl"],
    deps = ["//private:defs"],
)

bzl_library(
    name = "compile",
    srcs = ["compile.bzl"],
    visibility = [
        "//elisp:__pkg__",
        "//elisp/proto:__pkg__",
    ],
    deps = [
        ":load_path",
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
    deps = ["//private:defs"],
)

bzl_library(
    name = "executable_only",
    srcs = ["executable_only.bzl"],
)

bzl_library(
    name = "generated",
    srcs = ["generated.bzl"],
)

selects.config_setting_group(
    name = "gcc_or_clang",
    match_any = [
        "@rules_cc//cc/compiler:gcc",
        "@rules_cc//cc/compiler:clang",
    ],
    visibility = ["//visibility:private"],
)

# gazelle:ignore generated.*.bzl
