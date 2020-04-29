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

load("@phst_rules_elisp//elisp:defs.bzl", "elisp_binary", "elisp_library", "elisp_test")

elisp_library(
    name = "lib_1",
    srcs = ["lib-1.el"],
    deps = [
        ":lib_2",
        "@example//:lib_4",
    ],
)

elisp_test(
    name = "lib_1_test",
    srcs = ["lib-1-test.el"],
    deps = [":lib_1"],
)

elisp_library(
    name = "lib_2",
    srcs = ["lib-2.el"],
    load_path = ["."],
)

elisp_test(
    name = "lib_2_test",
    srcs = ["lib-2-test.el"],
    deps = [":lib_2"],
)

elisp_library(
    name = "lib_3",
    srcs = ["subdir/lib-3.el"],
    load_path = ["/examples/subdir"],
)

elisp_test(
    name = "lib_3_test",
    srcs = ["subdir/lib-3-test.el"],
    deps = [":lib_3"],
)

elisp_binary(
    name = "bin",
    src = "bin.el",
    deps = [":lib_1"],
)
