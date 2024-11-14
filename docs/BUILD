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

load("@com_google_protobuf//bazel:py_proto_library.bzl", "py_proto_library")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")
load("@pip_deps//:requirements.bzl", "requirement")
load("@rules_python//python:defs.bzl", "py_binary")
load("//elisp:defs.bzl", "elisp_binary", "elisp_manual")
load("//private:defs.bzl", "PACKAGE_FEATURES", "STARDOC_TAGS", "merged_manual")

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
    tags = STARDOC_TAGS,
)

# This rule assumes that Texinfo is installed locally.
genrule(
    name = "info",
    srcs = ["manual.texi"],
    outs = ["rules_elisp.info"],
    cmd = "makeinfo --no-split --output=$@ -- $<",
    tags = STARDOC_TAGS,
)

DOCS = [
    "elisp",
    "emacs",
    "repositories",
    "extensions",
]

merged_manual(
    name = "merged_manual",
    out = "merged.org",
    includes = DOCS,
    main = "manual.org",
    tags = STARDOC_TAGS,
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
    tags = STARDOC_TAGS,
    deps = [
        "//private:defs",
        "@bazel_features//:features",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:paths",
        "@com_google_protobuf//bazel/common:proto_common_bzl",
        "@com_google_protobuf//bazel/common:proto_info_bzl",
    ],
)

stardoc(
    name = "emacs",
    out = "emacs.binpb",
    format = "proto",
    input = "//emacs:defs.bzl",
    tags = STARDOC_TAGS,
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
    tags = STARDOC_TAGS,
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
    tags = STARDOC_TAGS,
    deps = [
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
