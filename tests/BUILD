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

# Tests for Emacs Lisp Bazel rules.  These tests are outside the ‘elisp’ package
# so that they can use external repositories loaded as development-only
# dependencies in our MODULE.bazel file, e.g. the Go rules.

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@bazel_skylib//lib:shell.bzl", "shell")
load("@bazel_skylib//rules:run_binary.bzl", "run_binary")
load("@rules_go//go:def.bzl", "go_test")
load("@rules_python//python:py_test.bzl", "py_test")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp:elisp_test.bzl", "elisp_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")
load(":elisp_test_suite.bzl", "elisp_test_suite")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

py_test(
    name = "emacs_test",
    size = "medium",
    timeout = "short",
    srcs = ["emacs_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--emacs=$(rlocationpath //emacs)"),
    ],
    data = ["//emacs"],
    deps = [
        "//elisp/private/tools:runfiles",
        "@abseil-py//absl/flags",
        "@abseil-py//absl/testing:absltest",
    ],
)

bzl_library(
    name = "elisp_test_suite",
    srcs = ["elisp_test_suite.bzl"],
    deps = [
        ":elisp_info_test",
        "//elisp:elisp_library",
    ],
)

# keep
bzl_library(
    name = "elisp_info_test",
    srcs = ["elisp_info_test.bzl"],
    deps = [
        "//elisp/common:elisp_info",
        "@bazel_skylib//lib:unittest",
    ],
)

elisp_test_suite(name = "elisp_test")

go_test(
    name = "tests_test",
    size = "medium",
    timeout = "short",
    srcs = ["ert_test.go"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--binary=$(rlocationpath :test)"),
        shell.quote("--test-el=$(rlocationpath :test.el)"),
        shell.quote("--junit-xsd=$(rlocationpath @junit_xsd//:JUnit.xsd)"),
    ],
    data = [
        ":test",
        ":test.el",
        "@junit_xsd//:JUnit.xsd",
    ],
    embedsrcs = [
        "coverage.dat",
        "version.txt",
    ],
    deps = [
        "@com_github_google_go_cmp//cmp",
        "@com_github_google_go_cmp//cmp/cmpopts",
        "@rules_go//go/runfiles",
    ],
)

elisp_test(
    name = "test",
    size = "small",
    srcs = ["test.el"],
    data = [
        # We need to explicitly include the source file as data dependency,
        # otherwise it won’t be present and we can’t test coverage.  For real
        # tests, Bazel does the right thing.
        "test-lib.el",
    ] + select({
        # We also need to explicitly include the marker file that Bazel would
        # generate to indicate that a file should be instrumented.  See the
        # commentary in //elisp:elisp_test.bzl for details.  Again, for real
        # tests, Bazel itself takes care of this.  We also skip this if we’re
        # already collecting coverage information, because then the marker file
        # is already present, and including it here would trigger a symlink
        # target clash.
        ":coverage": [],
        "//conditions:default": ["test-lib.el.instrument"],
    }),
    # Test a few Unicode characters.
    skip_tags = ["skip-from-attribute \t\n\r\014 äα𝐴🐈'\\\""],
    skip_tests = ["filter-via-skip-tests-attribute-äα𝐴🐈"],
    tags = ["manual"],
    deps = [":test_lib"],
)

elisp_library(
    name = "test_lib",
    srcs = ["test-lib.el"],
)

exports_files(
    ["test-lib.el"],
    visibility = ["//tests/pkg:__pkg__"],
)

config_setting(
    name = "coverage",
    values = {"collect_code_coverage": "true"},
)

elisp_binary(
    name = "empty",
    src = "empty.el",
)

py_test(
    name = "empty_test",
    size = "medium",
    timeout = "short",
    srcs = ["empty_test.py"],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--binary=$(rlocationpath :empty)"),
    ],
    data = [":empty"],
    deps = [
        "//elisp/private/tools:runfiles",
        "@abseil-py//absl/flags",
        "@abseil-py//absl/testing:absltest",
    ],
)

run_binary(
    name = "version",
    outs = ["version.txt"],
    args = ["/:$(execpath version.txt)"],
    tool = ":print_version",
)

elisp_binary(
    name = "print_version",
    src = "print-version.el",
)
