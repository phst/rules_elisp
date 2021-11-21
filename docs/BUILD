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

load("@bazel_skylib//rules:diff_test.bzl", "diff_test")
load("@bazel_skylib//rules:run_binary.bzl", "run_binary")
load("@io_bazel_stardoc//stardoc:stardoc.bzl", "stardoc")

stardoc(
    name = "elisp_doc",
    out = "elisp_doc.md",
    input = "//elisp:defs.bzl",
    deps = [
        "//elisp:util_bzl",
        "@bazel_skylib//lib:collections",
        "@bazel_skylib//lib:dicts",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "emacs_doc",
    out = "emacs_doc.md",
    input = "//emacs:defs.bzl",
    deps = [
        "//elisp:builtin_bzl",
        "//elisp:util_bzl",
        "@bazel_skylib//lib:paths",
    ],
)

stardoc(
    name = "repositories_doc",
    out = "repositories_doc.md",
    input = "//elisp:repositories.bzl",
    deps = ["//elisp:builtin_bzl"],
)

DOCS = [
    "elisp",
    "emacs",
    "repositories",
]

[
    # Bazel (including Stardoc) interprets all files as Latin-1,
    # cf. https://docs.bazel.build/versions/4.1.0/build-ref.html#BUILD_files.
    # However, our files all use UTF-8, leading to double encoding.  Reverse
    # that effect here.
    run_binary(
        name = "reencode_" + doc,
        srcs = [doc + "_doc.md"],
        outs = [doc + ".md.generated"],
        args = [
            "--",
            "$(location :{}_doc.md)".format(doc),
            "$(location :{}.md.generated)".format(doc),
        ],
        tool = ":reencode",
    )
    for doc in DOCS
]

[
    diff_test(
        name = doc + "_test",
        file1 = doc + ".md",
        file2 = doc + ".md.generated",
    )
    for doc in DOCS
]

py_binary(
    name = "reencode",
    srcs = ["reencode.py"],
    python_version = "PY3",
    srcs_version = "PY3",
)
