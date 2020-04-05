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
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")
load(":defs.bzl", "elisp_toolchain")
load(":defs_test.bzl", "elisp_test_suite")

toolchain_type(
    name = "toolchain_type",
    visibility = ["//visibility:public"],
)

toolchain(
    name = "hermetic_toolchain",
    toolchain = ":hermetic",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "local_toolchain",
    toolchain = ":local",
    toolchain_type = ":toolchain_type",
)

elisp_toolchain(
    name = "hermetic",
    emacs = "//emacs",
)

elisp_toolchain(
    name = "local",
    emacs = ":local_emacs",
    use_default_shell_env = True,
)

filegroup(
    name = "compile",
    srcs = ["compile.el"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "binary_template",
    srcs = ["binary.template"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "test_template",
    srcs = ["test.template"],
    visibility = ["//visibility:public"],
)

sh_binary(
    name = "local_emacs",
    srcs = ["local_emacs.sh"],
)

stardoc(
    name = "defs_doc",
    out = "defs_doc.md",
    input = "defs.bzl",
    deps = [
        ":util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories_doc",
    out = "repositories_doc.md",
    input = "repositories.bzl",
    deps = [":builtin_bzl"],
)

bzl_library(
    name = "util_bzl",
    srcs = ["util.bzl"],
    visibility = ["//emacs:__pkg__"],
    deps = [
        ":builtin_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

bzl_library(
    name = "builtin_bzl",
    srcs = ["@bazel_tools//tools:bzl_srcs"],
    visibility = ["//emacs:__pkg__"],
)

elisp_test_suite(name = "elisp_test")
