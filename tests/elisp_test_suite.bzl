# Copyright 2020-2023, 2025 Google LLC
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

"""Unit tests for our rules and providers."""

load("@bazel_skylib//lib:partial.bzl", "partial")
load("@bazel_skylib//lib:unittest.bzl", "unittest")
load("//elisp:elisp_library.bzl", "elisp_library")
load(":elisp_info_test.bzl", "provider_test")

visibility("private")

def elisp_test_suite(*, name):
    elisp_library(
        name = "provider_test_subject",
        srcs = ["provider-test.el"],
        tags = ["manual"],
    )
    unittest.suite(
        name,
        partial.make(
            provider_test,
            target_under_test = ":provider_test_subject",
            size = "small",
        ),
    )
