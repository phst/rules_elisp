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

# Integration tests for Emacs Lisp Bazel rules.  These tests are outside the
# ‘elisp’ package so that they can use external repositories loaded as
# development-only dependencies in our MODULE.bazel file, e.g. the Go rules.

load("@bazel_skylib//lib:shell.bzl", "shell")
load("@bazel_skylib//rules:run_binary.bzl", "run_binary")
load("@rules_go//go:def.bzl", "go_test")
load("@xmllint//:file.bzl", XMLLINT = "FILE")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp:elisp_test.bzl", "elisp_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_testonly = True,
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

TEST_DATA = [
    # keep sorted
    "//elisp/private/tools:binary.cc",
    "//elisp/private/tools:binary.h",
    "//elisp/runfiles:runfiles.elc",
    "//emacs",
    "//tests:empty",
    "//tests/integration:signal",
    "//tests/integration:test",
    "//tests/integration:test.el",
    "//tests/integration/wrap:launcher",
    "@junit_xsd//:JUnit.xsd",
]

go_test(
    name = "integration_test",
    size = "medium",
    timeout = "short",
    srcs = [
        "emacs_test.go",
        "ert_test.go",
        "unix_test.go",
        "windows_test.go",
    ],
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--xmllint=" + XMLLINT),
    ] + [
        shell.quote("--{t}=$(rlocationpath {t})".format(t = t))
        for t in TEST_DATA
    ],
    data = TEST_DATA,
    embedsrcs = [
        "coverage.dat",
        "version.txt",
    ],
    deps = [
        "//private/testutil",
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

exports_files(
    ["test-lib.el"],
    visibility = ["//tests/integration/pkg:__pkg__"],
)

config_setting(
    name = "coverage",
    values = {"collect_code_coverage": "true"},
)

run_binary(
    name = "version",
    outs = ["version.txt"],
    args = ["$(execpath version.txt)"],
    tool = ":print_version",
)

elisp_binary(
    name = "print_version",
    src = "print-version.el",
)

elisp_binary(
    name = "signal",
    src = "signal.el",
)

elisp_library(
    name = "test_lib",
    srcs = ["test-lib.el"],
)
