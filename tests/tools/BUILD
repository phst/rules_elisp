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
load("@rules_cc//cc:cc_test.bzl", "cc_test")
load("@rules_python//python:py_test.bzl", "py_test")
load("//private:cc_config.bzl", "COPTS", "CXXOPTS", "DEFINES", "FEATURES", "LINKOPTS")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

py_test(
    name = "manifest_test",
    size = "small",
    srcs = ["manifest_test.py"],
    python_version = "3.12",
    deps = [
        "//elisp/private/tools:manifest",
        "@abseil-py//absl/testing:absltest",
    ],
)

cc_test(
    name = "manifest_cc_test",
    size = "small",
    srcs = ["manifest_test.cc"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    deps = [
        "//elisp/private/tools:manifest_cc",
        "//elisp/private/tools:platform",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/status:status_matchers",
        "@abseil-cpp//absl/status:statusor",
        "@googletest//:gtest",
        "@googletest//:gtest_main",  # keep
    ],
)

py_test(
    name = "load_test",
    size = "small",
    srcs = ["load_test.py"],
    python_version = "3.12",
    deps = [
        "//elisp/private/tools:load",
        "//elisp/private/tools:runfiles",
        "@abseil-py//absl/testing:absltest",
    ],
)

cc_test(
    name = "load_cc_test",
    size = "small",
    srcs = ["load_test.cc"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@googletest//:gtest_main",  # keep
        "//elisp/private/tools:load_cc",
        "//elisp/private/tools:platform",
        "//elisp/private/tools:runfiles_cc",
        "//elisp/private/tools:system",
        "@abseil-cpp//absl/algorithm:container",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:status_matchers",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings:str_format",
        "@googletest//:gtest",
    ],
)

cc_test(
    name = "numeric_test",
    size = "small",
    srcs = ["numeric_test.cc"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    deps = [
        "//elisp/private/tools:numeric",
        "@googletest//:gtest",
        "@googletest//:gtest_main",  # keep
    ],
)

cc_test(
    name = "strings_test",
    size = "small",
    srcs = ["strings_test.cc"],
    copts = COPTS + CXXOPTS,
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES,
    deps = [
        "//elisp/private/tools:strings",
        "@abseil-cpp//absl/base:core_headers",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:status_matchers",
        "@googletest//:gtest",
        "@googletest//:gtest_main",  # keep
    ],
)

cc_test(
    name = "runfiles_cc_test",
    size = "medium",
    timeout = "short",
    srcs = ["runfiles_test.cc"],
    copts = COPTS + CXXOPTS,
    data = ["//tests:empty"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_PROGRAM=R"*($(rlocationpath //tests:empty))*"'),
    ],
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@googletest//:gtest_main",  # keep
        "@rules_cc//cc/runfiles",  # keep
        "//elisp/private/tools:platform",
        "//elisp/private/tools:runfiles_cc",
        "//elisp/private/tools:system",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:status_matchers",
        "@abseil-cpp//absl/status:statusor",
        "@googletest//:gtest",
    ],
)

cc_test(
    name = "system_test",
    size = "medium",
    timeout = "short",
    srcs = ["system_test.cc"],
    copts = COPTS + CXXOPTS,
    data = ["BUILD"],
    features = FEATURES,
    linkopts = LINKOPTS,
    linkstatic = True,
    local_defines = DEFINES + [
        # See https://github.com/bazelbuild/bazel/issues/10859 why we need to
        # add additional quoting.
        shell.quote('RULES_ELISP_DATA=R"*($(rlocationpath :BUILD))*"'),
    ],
    deps = [
        # FIXME: Remove the next dependency once we drop support for Bazel
        # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
        "@bazel_tools//tools/cpp/runfiles",  # keep
        "@googletest//:gtest_main",  # keep
        "@rules_cc//cc/runfiles",  # keep
        "//elisp/private/tools:numeric",
        "//elisp/private/tools:platform",
        "//elisp/private/tools:runfiles_cc",
        "//elisp/private/tools:strings",
        "//elisp/private/tools:system",
        "@abseil-cpp//absl/base:nullability",
        "@abseil-cpp//absl/status",
        "@abseil-cpp//absl/status:status_matchers",
        "@abseil-cpp//absl/status:statusor",
        "@abseil-cpp//absl/strings",
        "@abseil-cpp//absl/time",
        "@googletest//:gtest",
    ],
)

build_test(
    name = "launcher_test",
    targets = ["//elisp/private/tools:launcher"],
)

build_test(
    name = "run_test_el_test",
    targets = ["//elisp/private/tools:run_test_el"],
)

build_test(
    name = "compile_test",
    targets = ["//elisp/private/tools:compile"],
)
