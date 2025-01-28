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

load("@bazel_skylib//lib:shell.bzl", "shell")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@gazelle//:def.bzl", "gazelle", "gazelle_binary")
load("@rules_go//go:def.bzl", "go_library", "go_test")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

gazelle(
    name = "wrapper",
    gazelle = ":binary",
)

gazelle_binary(
    name = "binary",
    languages = [":gazelle"],
)

go_library(
    name = "gazelle",
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
    visibility = ["//dev:__pkg__"],
    deps = [
        "@gazelle//config",
        "@gazelle//label",
        "@gazelle//language",
        "@gazelle//pathtools",
        "@gazelle//repo",
        "@gazelle//resolve",
        "@gazelle//rule",
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
    args = [
        # See https://github.com/bazelbuild/bazel/issues/12313 why we need to
        # add additional quoting.
        shell.quote("--gazelle=$(rlocationpath :binary)"),
    ],
    data = [":binary"],
    deps = [
        ":gazelle",
        "@com_github_bazelbuild_buildtools//build",
        "@com_github_google_go_cmp//cmp",
        "@gazelle//config",
        "@gazelle//label",
        "@gazelle//language",
        "@gazelle//repo",
        "@gazelle//resolve",
        "@gazelle//rule",
        "@gazelle//testtools",
        "@rules_go//go/runfiles",
    ],
)

copy_file(
    name = "copy_builtin_features",
    src = "//emacs:builtin_features",
    out = "builtin_features.json",
)
