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
load("//elisp:defs.bzl", "elisp_binary", "elisp_library", "elisp_manual", "elisp_proto_library", "elisp_test")

elisp_library(
    name = "lib_1",
    srcs = ["lib-1.el"],
    data = ["data.txt"],
    deps = [
        ":lib_2",
        "//elisp/runfiles",
        "@example//:lib_4",
    ],
)

elisp_test(
    name = "lib_1_test",
    size = "small",
    srcs = ["lib-1-test.el"],
    data = ["lib-1-resources/test.txt"],
    skip_tests = ["lib-1-broken-test"],
    deps = [":lib_1"],
)

elisp_library(
    name = "lib_2",
    srcs = [
        "lib-2-a.el",
        "lib-2-b.el",
    ] + select({
        "//constraints:linux": ["module.so"],
        "//constraints:macos": ["module.so"],
        "//constraints:windows": ["module.dll"],
    }),
    fatal_warnings = False,
    load_path = ["."],
)

elisp_test(
    name = "lib_2_test",
    size = "small",
    srcs = ["lib-2-test.el"],
    skip_tags = ["skip"],
    deps = [":lib_2"],
)

elisp_library(
    name = "lib_3",
    srcs = ["subdir/lib-3.el"],
    load_path = ["/examples/subdir"],
)

elisp_test(
    name = "lib_3_test",
    size = "small",
    srcs = ["subdir/lib-3-test.el"],
    deps = [":lib_3"],
)

elisp_binary(
    name = "bin",
    src = "bin.el",
    deps = [":lib_1"],
)

cc_binary(
    name = "module.so",
    srcs = ["module.c"],
    linkshared = True,
    linkstatic = True,
    win_def_file = "module.def",
    deps = ["//emacs:module_header"],
)

# Make a copy with the proper suffix for Windows.
copy_file(
    name = "module_dll",
    src = "module.so",
    out = "module.dll",
    allow_symlink = True,
    is_executable = False,
)

elisp_manual(
    name = "doc",
    src = "doc.org",
    out = "doc.texi",
)

genrule(
    name = "makeinfo",
    srcs = ["doc.texi"],
    outs = ["doc.info"],
    cmd = "makeinfo --no-split --output=$@ -- $<",
)

py_test(
    name = "bin_test",
    size = "medium",
    timeout = "short",
    srcs = ["bin_test.py"],
    data = ["bin"],
    python_version = "PY3",
    srcs_version = "PY3",
    deps = ["//elisp:runfiles"],
)

# You can run this binary interactively: bazel run //examples:interactive
elisp_binary(
    name = "interactive",
    src = "interactive.el",
    interactive = True,
    deps = [
        ":example_elisp_proto",
        "//elisp/proto",
        "//elisp/runfiles",
    ],
)

proto_library(
    name = "example_proto",
    srcs = ["example.proto"],
)

elisp_proto_library(
    name = "example_elisp_proto",
    deps = [":example_proto"],
)

elisp_test(
    name = "proto_test",
    size = "small",
    srcs = ["proto-test.el"],
    deps = [
        ":example_elisp_proto",
        "//elisp/proto",
    ],
)
