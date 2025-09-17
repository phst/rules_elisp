load("@rules_python//python:py_binary.bzl", "py_binary")

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
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_COPTS", "LAUNCHER_DEFINES", "LAUNCHER_DEPS", "LAUNCHER_FEATURES", "LAUNCHER_LINKOPTS")  # buildifier: disable=bzl-visibility
load("//elisp/private:executable_only.bzl", "executable_only")  # buildifier: disable=bzl-visibility
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
    data = [":wrap_stripped"],
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    linkstatic = True,
    local_defines = LAUNCHER_DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_HEADER="elisp/private/tools/binary.h"'),
        shell.quote("RULES_ELISP_LAUNCHER_ARGS=" + ", ".join([
            'RULES_ELISP_NATIVE_LITERAL(R"*(--wrapper=$(rlocationpath :wrap_stripped))*")',
            'RULES_ELISP_NATIVE_LITERAL("--mode=wrap")',
            'RULES_ELISP_NATIVE_LITERAL("--rule-tag=local")',
            'RULES_ELISP_NATIVE_LITERAL("--rule-tag=mytag")',
            'RULES_ELISP_NATIVE_LITERAL("--load-directory=phst_rules_elisp")',
            'RULES_ELISP_NATIVE_LITERAL("--data-file=phst_rules_elisp/elisp/private/tools/binary.h")',
            'RULES_ELISP_NATIVE_LITERAL("--input-arg=2")',
            'RULES_ELISP_NATIVE_LITERAL("--output-arg=-1")',
        ])),
    ],
    visibility = ["//tests/tools:__pkg__"],
    deps = LAUNCHER_DEPS + ["//elisp/private/tools:binary"],
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
        "//elisp/private/tools:binary.cc",
        "//elisp/private/tools:binary.h",
        "//elisp/runfiles:runfiles.elc",
    ],
    deps = ["//elisp/private/tools:runfiles"],
)

build_test(
    name = "launcher_test",
    targets = [":launcher"],
)
