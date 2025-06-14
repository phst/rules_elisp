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

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@buildifier//:rules.bzl", "buildifier_test")
load("@gazelle//:def.bzl", "DEFAULT_LANGUAGES", "gazelle", "gazelle_binary", "gazelle_test")
load("@phst_license_test//:def.bzl", "license_test")
load("@pip//:requirements.bzl", "requirement")
load("@rules_go//go:def.bzl", "TOOLS_NOGO", "nogo")
load("@rules_python//python:pip.bzl", "compile_pip_requirements")
load("@rules_python//python:py_binary.bzl", "py_binary")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

buildifier_test(
    name = "buildifier_test",
    timeout = "short",
    lint_mode = "warn",
    lint_warnings = ["all"],
    no_sandbox = True,
    workspace = "//:MODULE.bazel",
)

license_test(
    name = "license_test",
    timeout = "short",
    ignore = [
        "coverage-report/**",
        "index.html",
    ],
    marker = "//:MODULE.bazel",
)

compile_pip_requirements(
    name = "requirements",
    timeout = "moderate",
    env = {"USERPROFILE": r"C:\Temp"},
    python_version = "3.12",
    requirements_in = "requirements.txt",
    requirements_txt = ":requirements_lock",
    tags = [
        # Don’t try to run Pylint or Pytype.  This target doesn’t contain any of
        # our source files.
        "no-pylint",
        "no-pytype",
    ],
)

alias(
    name = "requirements_lock",
    actual = select({
        "@platforms//os:linux": "requirements-linux.lock",
        "@platforms//os:macos": "requirements-macos.lock",
        "@platforms//os:windows": "requirements-windows.lock",
    }),
)

py_binary(
    name = "check_python",
    srcs = ["check_python.py"],
    main = "check_python.py",
    python_version = "3.12",
    visibility = ["//private:__pkg__"],
    deps = [requirement("pylint")] + select({
        # Pytype doesn’t work on Windows, so don’t build it when running
        # “bazel build //...”.
        "@platforms//os:linux": [requirement("pytype")],
        "@platforms//os:macos": [requirement("pytype")],
        "//conditions:default": [],
    }),
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
    timeout = "short",
    gazelle = ":gazelle_binary",
    target_compatible_with = select({
        # FIXME: Make this work on Windows.
        "@platforms//os:windows": ["@platforms//:incompatible"],
        "//conditions:default": [],
    }),
    workspace = "//:MODULE.bazel",
)

gazelle_binary(
    name = "gazelle_binary",
    languages = DEFAULT_LANGUAGES + [
        "//gazelle",
        "@bazel_skylib_gazelle_plugin//bzl",
        "@gazelle//language/bazel/visibility",
        "@gazelle_cc//language/cc",
    ],
)

bzl_library(
    name = "pylint",
    srcs = ["pylint.bzl"],
    deps = [
        ":check_python_bzl",
        "@rules_python//python:py_info_bzl",
    ],
)

bzl_library(
    name = "pytype",
    srcs = ["pytype.bzl"],
    deps = [
        ":check_python_bzl",
        "@rules_python//python:py_info_bzl",
    ],
)

# keep
bzl_library(
    name = "check_python_bzl",
    srcs = ["check_python.bzl"],
)
