# Copyright 2020, 2021, 2022 Google LLC
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
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")
load("@pip_deps//:requirements.bzl", "requirement")
load("//:private/defs.bzl", "merged_manual")
load("//elisp:defs.bzl", "elisp_binary", "elisp_manual")

elisp_manual(
    name = "manual",
    src = "merged.org",
    out = "manual.texi",
)

# This rule assumes that Texinfo is installed locally.
genrule(
    name = "info",
    srcs = ["manual.texi"],
    outs = ["rules_elisp.info"],
    cmd = "makeinfo --no-split --output=$@ -- $<",
)

DOCS = [
    "elisp",
    "emacs",
    "repositories",
]

merged_manual(
    name = "merged_manual",
    out = "merged.org",
    includes = DOCS,
    main = "manual.org",
)

elisp_binary(
    name = "merge",
    src = "merge.el",
    input_args = list(range(
        2,
        len(DOCS) + 3,
    )),
    output_args = [1],
)

stardoc(
    name = "elisp",
    out = "elisp.bin",
    format = "proto",
    input = "//elisp:defs.bzl",
    deps = [
        "//:private_defs_bzl",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "emacs",
    out = "emacs.bin",
    format = "proto",
    input = "//emacs:defs.bzl",
    deps = [
        "//:private_defs_bzl",
        "//elisp:builtin_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories",
    out = "repositories.bin",
    format = "proto",
    input = "//elisp:repositories.bzl",
    deps = ["//elisp:builtin_bzl"],
)

py_binary(
    name = "generate",
    srcs = ["generate.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    tags = ["no-pytype"],  # FIXME
    deps = [
        ":stardoc_output_py_proto",
        requirement("commonmark"),
    ],
)

py_proto_library(
    name = "stardoc_output_py_proto",
    srcs = [":stardoc_output.proto"],
    srcs_version = "PY3",
    tags = ["no-python-check"],
)

copy_file(
    name = "copy_stardoc_output_proto",
    src = "@io_bazel_stardoc//stardoc/proto:stardoc_output.proto",
    out = "stardoc_output.proto",
)
