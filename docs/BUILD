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

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("@bazel_skylib//rules:diff_test.bzl", "diff_test")
load("@pip//:requirements.bzl", "requirement")
load("@protobuf//bazel:py_proto_library.bzl", "py_proto_library")
load("@rules_python//python:py_binary.bzl", "py_binary")
load("//elisp:elisp_binary.bzl", "elisp_binary")
load("//elisp:elisp_manual.bzl", "elisp_manual")
load("//private:package_features.bzl", "PACKAGE_FEATURES")
load(":makeinfo.bzl", "makeinfo")
load(":merged_manual.bzl", "merged_manual")

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

# These rules assume that Texinfo is installed locally.
makeinfo(
    name = "info",
    src = "manual.texi",
    out = "rules_elisp.info",
)

makeinfo(
    name = "html",
    src = "manual.texi",
    out = "manual.html",
)

diff_test(
    name = "html_test",
    timeout = "short",
    failure_message = "\n".join([
        "Run the following command to fix:",
        "bazel build //docs:manual.html && cp bazel-bin/docs/manual.html index.html",
    ]),
    file1 = "manual.html",
    file2 = "//:index.html",
    # On the GitHub runners, the installed version of Texinfo depends on the
    # operating system.  Therefore spurious diffs would be generated if the
    # installed version and the version used for development (on macOS) don’t
    # match.
    # FIXME: Find a better solution.
    target_compatible_with = ["@platforms//os:macos"],
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
    ("elisp/toolchains", "elisp_emacs_binary"),
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
    tags = [
        "no-pylint",
        "no-pytype",
    ],
    deps = ["@stardoc//stardoc/proto:stardoc_output_proto"],
)

bzl_library(
    name = "makeinfo",
    srcs = ["makeinfo.bzl"],
    deps = ["@makeinfo//:file"],
)
