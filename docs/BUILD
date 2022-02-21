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
load("@bazel_skylib//rules:run_binary.bzl", "run_binary")
load("@com_google_protobuf//:protobuf.bzl", "py_proto_library")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")
load("@pip_deps//:requirements.bzl", "requirement")
load("//elisp:defs.bzl", "elisp_binary")

DOCS = {
    "elisp": "//elisp:defs.bzl",
    "emacs": "//emacs:defs.bzl",
    "repositories": "//elisp:repositories.bzl",
}

run_binary(
    name = "run_merge",
    srcs = ["manual.org"] + [f + ".org" for f in DOCS],
    outs = ["merged.org"],
    args = [
        "$(location :merged.org)",
        "$(location :manual.org)",
    ] + ["$(location :{}.org)".format(f) for f in DOCS],
    tool = ":merge",
)

elisp_binary(
    name = "merge",
    src = "merge.el",
    input_args = list(range(
        2,
        len(DOCS) + 2,
    )),
    output_args = [1],
)

stardoc(
    name = "elisp_doc",
    out = "elisp_doc.bin",
    format = "proto",
    input = DOCS["elisp"],
    deps = [
        "//elisp:util_bzl",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "emacs_doc",
    out = "emacs_doc.bin",
    format = "proto",
    input = DOCS["emacs"],
    deps = [
        "//elisp:builtin_bzl",
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories_doc",
    out = "repositories_doc.bin",
    format = "proto",
    input = DOCS["repositories"],
    deps = ["//elisp:builtin_bzl"],
)

[
    run_binary(
        name = "gen_" + doc,
        srcs = [doc + "_doc.bin"],
        outs = [doc + ".org"],
        args = [
            "--",
            label,
            "$(location :{}_doc.bin)".format(doc),
            "$(location :{}.org)".format(doc),
        ],
        tool = ":org",
    )
    for doc, label in DOCS.items()
]

py_binary(
    name = "org",
    srcs = ["org.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    tags = ["no-pytype"],  # FIXME
    deps = [
        ":stardoc_output_py_proto",
        requirement("marko"),
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
