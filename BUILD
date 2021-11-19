# Copyright 2020, 2021 Google LLC
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

load("@bazel_gazelle//:def.bzl", "gazelle")
load("@pip_deps//:requirements.bzl", "requirement")
load("@rules_python//python:pip.bzl", "compile_pip_requirements")

compile_pip_requirements(
    name = "requirements",
    extra_args = [
        "--allow-unsafe",  # for setuptools
    ],
    requirements_in = "requirements.in",
    requirements_txt = ":requirements_txt",
)

alias(
    name = "requirements_txt",
    actual = select({
        ":linux": "linux-requirements.txt",
        ":macos": "macos-requirements.txt",
        ":windows": "windows-requirements.txt",
    }),
)

config_setting(
    name = "linux",
    constraint_values = ["@platforms//os:linux"],
)

config_setting(
    name = "macos",
    constraint_values = ["@platforms//os:macos"],
)

config_setting(
    name = "windows",
    constraint_values = ["@platforms//os:windows"],
)

py_binary(
    name = "run_pylint",
    srcs = ["run_pylint.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    visibility = ["//:__pkg__"],
    deps = [requirement("pylint")],
)

py_binary(
    name = "run_pytype",
    srcs = ["run_pytype.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    # Pytype doesn’t work on Windows, so don’t build it when running
    # “bazel build //...”.
    tags = ["manual"],
    visibility = ["//:__pkg__"],
    deps = [requirement("pytype")],
)

# gazelle:prefix github.com/phst/rules_elisp
gazelle(name = "gazelle")
