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

load("@rules_license//rules:license.bzl", "license")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_applicable_licenses = [":license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

exports_files(["LICENSE"])

license(
    name = "license",
    license_kind = "@rules_license//licenses/spdx:Apache-2.0",
)

exports_files(
    [".pylintrc"],
    visibility = ["//dev:__pkg__"],
)

exports_files(
    ["MODULE.bazel"],
    visibility = ["//dev:__pkg__"],
)

exports_files(
    ["index.html"],
    visibility = ["//docs:__pkg__"],
)

# gazelle:default_visibility //visibility:private
# gazelle:exclude README.org
