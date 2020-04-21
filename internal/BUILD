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

cc_library(
    name = "file",
    srcs = ["file.cc"],
    hdrs = ["file.h"],
    copts = COPTS,
    visibility = ["//emacs:__pkg__"],
    deps = [
        ":int",
        ":macros",
        ":random",
        ":statusor",
        "@com_google_absl//absl/base",
        "@com_google_absl//absl/memory",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/utility",
    ],
)

cc_test(
    name = "file_test",
    srcs = ["file_test.cc"],
    copts = COPTS,
    data = ["test.txt"],
    deps = [
        ":file",
        ":random",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/types:optional",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "random",
    srcs = ["random.cc"],
    hdrs = ["random.h"],
    copts = COPTS,
    visibility = ["//emacs:__pkg__"],
    deps = [
        "@com_google_absl//absl/random",
        "@com_google_absl//absl/strings",
    ],
)

cc_test(
    name = "random_test",
    srcs = ["random_test.cc"],
    copts = COPTS,
    deps = [
        ":random",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_library(
    name = "int",
    hdrs = ["int.h"],
    copts = COPTS,
    visibility = ["//emacs:__pkg__"],
)

cc_library(
    name = "macros",
    hdrs = ["macros.h"],
    copts = COPTS,
    visibility = ["//emacs:__pkg__"],
    deps = [
        ":statusor",
        "@com_google_absl//absl/status",
    ],
)

cc_library(
    name = "statusor",
    hdrs = ["statusor.h"],
    copts = COPTS,
    visibility = ["//emacs:__pkg__"],
    deps = [
        "@com_google_absl//absl/base",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/types:variant",
    ],
)
