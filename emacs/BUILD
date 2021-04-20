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

alias(
    name = "emacs",
    actual = ":emacs_27.2",
    visibility = ["//visibility:public"],
)

sh_test(
    name = "emacs_test",
    srcs = ["emacs_test.sh"],
    data = [":emacs"],
)

alias(
    name = "module_header",
    actual = "@gnu_emacs_27.2//:module_header",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_26.1",
    actual = "@gnu_emacs_26.1//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_26.2",
    actual = "@gnu_emacs_26.2//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_26.3",
    actual = "@gnu_emacs_26.3//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_27.1",
    actual = "@gnu_emacs_27.1//:emacs",
    visibility = ["//visibility:public"],
)

alias(
    name = "emacs_27.2",
    actual = "@gnu_emacs_27.2//:emacs",
    visibility = ["//visibility:public"],
)

py_binary(
    name = "build",
    srcs = ["build.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    visibility = ["//visibility:public"],
)

exports_files(srcs = ["driver.template"])

bzl_library(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    deps = [
        "//elisp:builtin_bzl",
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

exports_files(
    ["defs.bzl"],
    visibility = ["//docs:__pkg__"],
)
