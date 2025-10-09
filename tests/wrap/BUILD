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
load("@bazel_skylib//rules:build_test.bzl", "build_test")
load("@rules_cc//cc:cc_binary.bzl", "cc_binary")
load("@rules_go//go:def.bzl", "go_binary", "go_library")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_COPTS", "LAUNCHER_DEFINES", "LAUNCHER_DEPS", "LAUNCHER_FEATURES", "LAUNCHER_LINKOPTS")  # buildifier: disable=bzl-visibility
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

# keep
cc_binary(
    name = "launcher",
    testonly = True,
    srcs = ["//elisp/private/tools:launcher.cc"],
    copts = LAUNCHER_COPTS,
    data = [":wrap"],
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote("RULES_ELISP_BINARY=1"),
        shell.quote('RULES_ELISP_WRAPPER=RULES_ELISP_NATIVE_LITERAL(R"*($(rlocationpath :wrap))*")'),
        shell.quote("RULES_ELISP_MODE=rules_elisp::ToolchainMode::kWrap"),
        shell.quote('RULES_ELISP_TAGS=RULES_ELISP_NATIVE_LITERAL("local"), RULES_ELISP_NATIVE_LITERAL("mytag")'),
        shell.quote('RULES_ELISP_LOAD_PATH=RULES_ELISP_NATIVE_LITERAL("phst_rules_elisp")'),
        shell.quote("RULES_ELISP_LOAD_FILES="),
        shell.quote('RULES_ELISP_DATA_FILES=RULES_ELISP_NATIVE_LITERAL("phst_rules_elisp/elisp/private/tools/binary.h")'),
        shell.quote("RULES_ELISP_INPUT_ARGS=2"),
        shell.quote("RULES_ELISP_OUTPUT_ARGS=-1"),
        shell.quote("RULES_ELISP_INTERACTIVE=false"),
    ],
    visibility = ["//tests:__pkg__"],
    deps = LAUNCHER_DEPS + ["//elisp/private/tools:binary"],
)

build_test(
    name = "launcher_test",
    targets = [":launcher"],
)

go_library(
    name = "wrap_lib",
    srcs = ["main.go"],
    importpath = "github.com/phst/rules_elisp/tests/wrap",
)

go_binary(
    name = "wrap",
    embed = [":wrap_lib"],
)
