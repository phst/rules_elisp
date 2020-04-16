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

stardoc(
    name = "elisp_defs_doc",
    out = "elisp_defs_doc.md",
    input = "//elisp:defs_bzl",
    deps = [
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "emacs_defs_doc",
    out = "emacs_defs_doc.md",
    input = "//emacs:defs_bzl",
    deps = [
        "//elisp:builtin_bzl",
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories_doc",
    out = "repositories_doc.md",
    input = "//elisp:repositories_bzl",
    deps = ["//elisp:builtin_bzl"],
)
