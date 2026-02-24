# Copyright 2020-2026 Google LLC
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
load("@buildifier//:rules.bzl", "buildifier_test")
load("@gazelle//:def.bzl", "DEFAULT_LANGUAGES", "gazelle", "gazelle_binary", "gazelle_test")
load("@phst_license_test//:def.bzl", "license_test")
load("@rules_go//go:def.bzl", "TOOLS_NOGO", "nogo")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

buildifier_test(
    name = "buildifier_test",
    size = "medium",
    timeout = "short",
    lint_mode = "warn",
    lint_warnings = ["all"],
    no_sandbox = True,
    workspace = "//:MODULE.bazel",
)

license_test(
    name = "license_test",
    size = "medium",
    timeout = "short",
    ignore = [
        "coverage-report/**",
        "index.html",
    ],
    marker = "//:MODULE.bazel",
)

nogo(
    name = "nogo",
    config = "nogo.json",
    visibility = ["//visibility:public"],
    deps = TOOLS_NOGO,
)

gazelle(
    name = "gazelle",
    gazelle = ":gazelle_binary",
)

gazelle_test(
    name = "gazelle_test",
    size = "medium",
    timeout = "short",
    gazelle = ":gazelle_binary",
    workspace = "//:MODULE.bazel",
)

gazelle_binary(
    name = "gazelle_binary",
    languages = DEFAULT_LANGUAGES + [
        "//gazelle/elisp",
        "@bazel_skylib_gazelle_plugin//bzl",
        "@gazelle_cc//language/cc",
    ],
)

bzl_library(
    name = "local_binary",
    srcs = ["local_binary.bzl"],
)
