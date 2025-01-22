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

load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@buildifier//:rules.bzl", "buildifier_test")
load("@gazelle//:def.bzl", "gazelle")
load("@phst_license_test//:def.bzl", "license_test")
load("@pip//:requirements.bzl", "requirement")
load("@rules_go//go:def.bzl", "TOOLS_NOGO", "go_test", "nogo")
load("@rules_python//python:defs.bzl", "py_binary")
load("@rules_python//python:pip.bzl", "compile_pip_requirements")
load("//private:defs.bzl", "PACKAGE_FEATURES")

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
    lint_warnings = [
        "+native-cc",
        "+native-proto",
        "+native-py",
    ],
    no_sandbox = True,
    workspace = "//:MODULE.bazel",
)

license_test(
    name = "license_test",
    timeout = "short",
    ignore = ["coverage-report/**"],
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
        "no-python-check",
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

go_test(
    name = "go_default_test",
    size = "small",
    srcs = ["version_test.go"],
    embedsrcs = ["MODULE.bazel"],
    deps = ["@com_github_bazelbuild_buildtools//build"],
)

copy_file(
    name = "copy_module_bazel",
    src = "//:MODULE.bazel",
    out = "MODULE.bazel",
)

nogo(
    name = "nogo",
    config = "nogo.json",
    visibility = ["//visibility:public"],
    deps = TOOLS_NOGO,
)

gazelle(name = "gazelle")
