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

exports_files(srcs = ["driver.template"])

exports_files(
    ["defs.bzl"],
    visibility = ["//documentation:__pkg__"],
)
