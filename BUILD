# Copyright 2020, 2021, 2022, 2023 Google LLC
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

package(
    default_applicable_licenses = [":license"],
    default_visibility = ["//visibility:private"],
)

licenses(["notice"])

exports_files(["LICENSE"])

license(
    name = "license",
    license_kind = "@rules_license//licenses/generic:notice",
)

py_binary(
    name = "build",
    srcs = ["build.py"],
    main = "build.py",
    python_version = "PY3",
    srcs_version = "PY3",
)

exports_files(
    [".pylintrc"],
    visibility = ["//:__subpackages__"],
)
