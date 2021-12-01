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

load("@io_bazel_rules_go//go:def.bzl", "go_binary", "go_library")

go_library(
    name = "go_default_library",
    testonly = 1,
    srcs = ["main.go"],
    data = [
        "//elisp:binary.cc",
        "//elisp/runfiles:runfiles.elc",
    ],
    importpath = "github.com/phst/rules_elisp/tests/wrap",
    visibility = ["//visibility:private"],
    deps = [
        "@com_github_google_go_cmp//cmp:go_default_library",
        "@com_github_phst_runfiles//:go_default_library",
    ],
)

go_binary(
    name = "wrap",
    testonly = 1,
    out = select({
        "@platforms//os:windows": "wrap.exe",
        "//conditions:default": "wrap",
    }),
    embed = [":go_default_library"],
    visibility = ["//elisp:__pkg__"],
)
