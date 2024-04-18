# Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")
load("@pip_deps//:requirements.bzl", "requirement")
load("@rules_python//python:defs.bzl", "py_binary")
load("@rules_python//python:proto.bzl", "py_proto_library")
load("//elisp:defs.bzl", "elisp_binary", "elisp_manual")
load("//private:defs.bzl", "DOCUMENT_EXTENSIONS", "PACKAGE_FEATURES", "merged_manual")

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
    "repositories",
] + (["extensions"] if DOCUMENT_EXTENSIONS else [])

merged_manual(
    name = "merged_manual",
    out = "merged.org",
    exclude_tag = "" if DOCUMENT_EXTENSIONS else "extensions",
    includes = DOCS,
    main = "manual.org",
)

elisp_binary(
    name = "merge",
    src = "merge.el",
    input_args = list(range(
        3,
        len(DOCS) + 4,
    )),
    output_args = [2],
)

stardoc(
    name = "elisp",
    out = "elisp.binpb",
    format = "proto",
    input = "//elisp:defs.bzl",
    deps = [
        "//private:defs",
        "@bazel_features//:features",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:paths",
        "@rules_proto//proto:defs",
    ],
)

stardoc(
    name = "emacs",
    out = "emacs.binpb",
    format = "proto",
    input = "//emacs:defs.bzl",
    deps = [
        "//elisp:builtin",
        "//private:defs",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories",
    out = "repositories.binpb",
    format = "proto",
    input = "//elisp:repositories.bzl",
    deps = [
        "//elisp:builtin",
        "//private:repositories",
    ],
)

stardoc(
    name = "extensions",
    out = "extensions.binpb",
    format = "proto",
    input = "//elisp:extensions.bzl",
    tags = [
        # Due to https://github.com/bazelbuild/stardoc/issues/192, building this
        # target fails on Bazel 6 and below, so don’t include it in target
        # patterns.
        "manual",
    ],
    deps = [
        "//elisp:builtin",
        "//elisp:repositories",
        "//private:repositories",
    ],
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
    deps = ["@io_bazel_stardoc//stardoc/proto:stardoc_output_proto"],
)
