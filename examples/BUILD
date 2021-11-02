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

load("@io_bazel_rules_go//go:def.bzl", "go_test")
load(
    "//elisp:defs.bzl",
    "elisp_binary",
    "elisp_library",
    "elisp_manual",
    "elisp_test",
)

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
    skip_tests = ["lib-1-broken-test"],
    deps = [":lib_1"],
)

elisp_library(
    name = "lib_2",
    srcs = [
        "lib-2-a.el",
        "lib-2-b.el",
        "module.so",
    ],
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
    deps = ["//emacs:module_header"],
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

go_test(
    name = "go_default_test",
    size = "medium",
    timeout = "short",
    srcs = ["bin_test.go"],
    data = ["bin"],
    rundir = ".",
    deps = ["@com_github_phst_runfiles//:go_default_library"],
)
