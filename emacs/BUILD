# Copyright 2020 Google LLC
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

load("//elisp:util.bzl", "COPTS")

alias(
    name = "emacs",
    actual = "@gnu_emacs_stable//:emacs",
    visibility = ["//visibility:public"],
)

sh_test(
    name = "emacs_test",
    srcs = ["emacs_test.sh"],
    data = [":emacs"],
)

py_binary(
    name = "build",
    srcs = ["build.py"],
    python_version = "PY3",
    srcs_version = "PY3",
    visibility = ["//visibility:public"],
)

cc_library(
    name = "exec",
    srcs = ["exec.cc"],
    hdrs = ["exec.h"],
    copts = COPTS,
    visibility = ["//visibility:public"],
    # Note: This library can’t depend on Abseil directly because it uses
    # incompatible build flags.  All dependencies need to be mediated through
    # random.h for now.
    deps = [
        ":manifest_cc_proto",
        ":random",
        "@bazel_tools//tools/cpp/runfiles",
        "@com_google_protobuf//:protobuf",
    ],
)

cc_test(
    name = "exec_test",
    srcs = ["exec_test.cc"],
    copts = COPTS,
    data = [
        "exec.h",
        "//emacs/wrap",
    ],
    deps = [
        ":exec",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "random",
    srcs = ["random.cc"],
    hdrs = ["random.h"],
    deps = [
        "@com_google_absl//absl/memory",
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/strings",
    ],
)

proto_library(
    name = "manifest_proto",
    srcs = ["manifest.proto"],
)

cc_proto_library(
    name = "manifest_cc_proto",
    deps = [":manifest_proto"],
)

filegroup(
    name = "driver_template",
    srcs = ["driver.template"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "defs_bzl",
    srcs = ["defs.bzl"],
    visibility = ["//documentation:__pkg__"],
)
