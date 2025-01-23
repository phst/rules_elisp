# Copyright 2020, 2021, 2023, 2024, 2025 Google LLC
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

load("@bazel_skylib//rules:build_test.bzl", "build_test")
load("//elisp:defs.bzl", "elisp_library")
load("//private:defs.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

elisp_library(
    name = "runner",
    testonly = True,
    srcs = ["runner.el"],
    outs = ["runner.elc"],
    visibility = ["//elisp/private/tools:__pkg__"],
)

build_test(
    name = "runner_test",
    targets = [":runner"],
)
