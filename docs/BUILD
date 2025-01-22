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

load("@pip//:requirements.bzl", "requirement")
load("@protobuf//bazel:py_proto_library.bzl", "py_proto_library")
load("@rules_python//python:py_binary.bzl", "py_binary")
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

# This rule assumes that Perl and Texinfo are installed locally.
genrule(
    name = "info",
    srcs = ["manual.texi"],
    outs = ["rules_elisp.info"],
    cmd = "$(execpath @local_texinfo//:perl) -w $(execpath @local_texinfo//:makeinfo) --no-split --output=$@ -- $<",
    tools = [
        "@local_texinfo//:makeinfo",
        "@local_texinfo//:perl",
    ],
)

DOCS = [
    ("elisp", "elisp_library"),
    ("elisp/proto", "elisp_proto_library"),
    ("elisp", "elisp_cc_module"),
    ("elisp", "elisp_binary"),
    ("elisp", "elisp_test"),
    ("elisp", "elisp_manual"),
    ("elisp/common", "elisp_info"),
    ("elisp/toolchains", "elisp_toolchain"),
    ("elisp/extensions", "elisp"),
    ("emacs", "defs"),
]

merged_manual(
    name = "merged_manual",
    out = "merged.org",
    includes = ["{}/{}.binaryproto".format(pkg, name) for pkg, name in DOCS],
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

[
    starlark_doc_extract(
        name = "{}/{}".format(pkg, name),
        src = "//{}:{}.bzl".format(pkg, name),
        render_main_repo_name = True,
        deps = ["//{}:{}".format(pkg, name)],
    )
    for pkg, name in DOCS
]

py_binary(
    name = "generate",
    srcs = ["generate.py"],
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
