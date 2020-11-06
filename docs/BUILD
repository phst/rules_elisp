# Copyright 2020 Google LLC
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

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@io_bazel_rules_go//go:def.bzl", "go_test")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")

stardoc(
    name = "elisp_defs_doc",
    out = "elisp_defs_doc.md",
    input = "//elisp:defs.bzl",
    deps = [
        "//elisp:util_bzl",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "emacs_defs_doc",
    out = "emacs_defs_doc.md",
    input = "//emacs:defs.bzl",
    deps = [
        "//elisp:builtin_bzl",
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories_doc",
    out = "repositories_doc.md",
    input = "//elisp:repositories.bzl",
    deps = ["//elisp:builtin_bzl"],
)

go_test(
    name = "go_default_test",
    srcs = ["consistency_test.go"],
    args = ["$(locations :markdown)"],
    data = [":markdown"],
    rundir = ".",
    deps = [
        "@com_github_google_go_cmp//cmp:go_default_library",
        "@org_golang_x_text//encoding:go_default_library",
        "@org_golang_x_text//encoding/charmap:go_default_library",
        "@org_golang_x_text//transform:go_default_library",
    ],
)

filegroup(
    name = "markdown",
    srcs = glob(
        ["*.md"],
        exclude = ["index.md"],
    ) + [
        "elisp_defs_doc.md",
        "emacs_defs_doc.md",
        "repositories_doc.md",
    ],
)
