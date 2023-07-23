# Copyright 2020, 2021, 2022, 2023 Google LLC
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

# Tests for Emacs Lisp Bazel rules.  These tests are outside the //elisp
# package so that they can use external workspaces loaded in our WORKSPACE file
# only, e.g. the Go rules.

load("//elisp:defs.bzl", "elisp_library", "elisp_test")
load(":private/defs.bzl", "elisp_test_suite")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@io_bazel_rules_go//go:def.bzl", "go_test")
load("@rules_pkg//pkg:tar.bzl", "pkg_tar")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
)

licenses(["notice"])

elisp_test_suite(name = "elisp_test")

go_test(
    name = "go_default_test",
    size = "medium",
    timeout = "short",
    srcs = ["ert_test.go"],
    embedsrcs = [
        "test_package.tar",
        "JUnit.xsd",
        "coverage.dat",
    ],
    rundir = ".",
    deps = [
        "@com_github_google_go_cmp//cmp:go_default_library",
        "@com_github_google_go_cmp//cmp/cmpopts:go_default_library",
        "@io_bazel_rules_go//go/runfiles",
    ],
)

elisp_test(
    name = "test_test",
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
        # commentary in //elisp:defs.bzl for details.  Again, for real tests,
        # Bazel itself takes care of this.  We also skip this if we’re already
        # collecting coverage information, because then the marker file is
        # already present, and including it here would trigger a symlink target
        # clash.
        ":coverage": [],
        "//conditions:default": ["test-lib.el.instrument"],
    }),
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

pkg_tar(
    name = "test_package",
    testonly = True,
    srcs = [":test_test"],
    include_runfiles = True,
    strip_prefix = "/",
)

copy_file(
    name = "copy_junit_xsd",
    testonly = True,
    src = "@junit_xsd//:JUnit.xsd",
    out = "JUnit.xsd",
)
