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

load("@pip_deps//:requirements.bzl", "requirement")
load("@protobuf//bazel:py_proto_library.bzl", "py_proto_library")
load("@rules_python//python:py_binary.bzl", "py_binary")
load("@stardoc//stardoc:stardoc.bzl", "stardoc")
load("//elisp:defs.bzl", "elisp_binary", "elisp_manual")
load("//private:defs.bzl", "PACKAGE_FEATURES", "merged_manual")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

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
    "extensions",
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
    out = "elisp.binpb",
    format = "proto",
    input = "//elisp:defs.bzl",
    deps = ["//elisp:defs"],
)

stardoc(
    name = "emacs",
    out = "emacs.binpb",
    format = "proto",
    input = "//emacs:defs.bzl",
    deps = ["//emacs:defs"],
)

stardoc(
    name = "extensions",
    out = "extensions.binpb",
    format = "proto",
    input = "//elisp:extensions.bzl",
    deps = ["//elisp:extensions"],
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
    tags = ["no-python-check"],
    deps = ["@stardoc//stardoc/proto:stardoc_output_proto"],
)
