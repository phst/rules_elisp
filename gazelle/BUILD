# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@gazelle//:def.bzl", "gazelle", "gazelle_binary")
load("@rules_go//go:def.bzl", "go_library", "go_test")
load("//private:defs.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

gazelle(
    name = "gazelle",
    gazelle = ":binary",
)

gazelle_binary(
    name = "binary",
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
        "@gazelle//config:go_default_library",
        "@gazelle//label:go_default_library",
        "@gazelle//language:go_default_library",
        "@gazelle//pathtools:go_default_library",
        "@gazelle//repo:go_default_library",
        "@gazelle//resolve:go_default_library",
        "@gazelle//rule:go_default_library",
    ],
)

go_test(
    name = "gazelle_test",
    timeout = "short",
    srcs = [
        "gazelle_test.go",
        "generate_test.go",
        "imports_test.go",
        "resolve_test.go",
    ],
    embedsrcs = ["binary.exe"],
    rundir = ".",
    deps = [
        ":go_default_library",
        "@com_github_bazelbuild_buildtools//build:go_default_library",
        "@com_github_google_go_cmp//cmp:go_default_library",
        "@gazelle//label:go_default_library",
        "@gazelle//language:go_default_library",
        "@gazelle//repo:go_default_library",
        "@gazelle//resolve:go_default_library",
        "@gazelle//rule:go_default_library",
        "@gazelle//testtools:go_default_library",
        "@rules_go//go/runfiles",
    ],
)

copy_file(
    name = "copy_builtin_features",
    src = "//emacs:builtin_features",
    out = "builtin_features.json",
)

copy_file(
    name = "copy_binary",
    testonly = True,
    src = ":binary",
    out = "binary.exe",
)
