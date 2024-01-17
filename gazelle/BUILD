# Copyright 2021, 2022, 2023, 2024 Google LLC
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

load("@bazel_gazelle//:def.bzl", "gazelle_binary")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@io_bazel_rules_go//go:def.bzl", "go_library", "go_test")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = [
        "-macos_default_link_flags",
        "-macos_minimum_os",
    ],
)

licenses(["notice"])

gazelle_binary(
    name = "gazelle",
    languages = [":go_default_library"],
)

go_library(
    name = "go_default_library",
    srcs = [
        "builtin.go",
        "feature.go",
        "generate.go",
        "imports.go",
        "language.go",
        "loadpath.go",
        "resolve.go",
        "source.go",
    ],
    embedsrcs = ["builtin_features.json"],
    importpath = "github.com/phst/rules_elisp/gazelle",
    visibility = ["//visibility:private"],
    deps = [
        "@bazel_gazelle//config:go_default_library",
        "@bazel_gazelle//label:go_default_library",
        "@bazel_gazelle//language:go_default_library",
        "@bazel_gazelle//pathtools:go_default_library",
        "@bazel_gazelle//repo:go_default_library",
        "@bazel_gazelle//resolve:go_default_library",
        "@bazel_gazelle//rule:go_default_library",
    ],
)

go_test(
    name = "go_default_test",
    timeout = "short",
    srcs = [
        "gazelle_test.go",
        "generate_test.go",
        "imports_test.go",
        "resolve_test.go",
    ],
    embedsrcs = ["gazelle.exe"],
    rundir = ".",
    deps = [
        ":go_default_library",
        "@bazel_gazelle//label:go_default_library",
        "@bazel_gazelle//language:go_default_library",
        "@bazel_gazelle//repo:go_default_library",
        "@bazel_gazelle//resolve:go_default_library",
        "@bazel_gazelle//rule:go_default_library",
        "@bazel_gazelle//testtools:go_default_library",
        "@com_github_bazelbuild_buildtools//build:go_default_library",
        "@com_github_google_go_cmp//cmp:go_default_library",
        "@io_bazel_rules_go//go/runfiles",
    ],
)

copy_file(
    name = "copy_builtin_features",
    src = "//emacs:builtin_features",
    out = "builtin_features.json",
)

copy_file(
    name = "copy_gazelle",
    testonly = True,
    src = ":gazelle",
    out = "gazelle.exe",
)
