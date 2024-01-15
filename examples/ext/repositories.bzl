# Copyright 2023, 2024 Philipp Stephani
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Contains repository functions for the example repository."""

load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("@phst_rules_elisp//elisp:repositories.bzl", "elisp_http_archive")

visibility("public")

def example_dependencies():
    """Installs necessary dependencies for the example."""
    maybe(
        elisp_http_archive,
        name = "dash",
        exclude = ["dash-functional.el"],
        integrity = "sha256-FSggKxdDwpkenNRiTcKYPrms/7Neno4OZowSzDZSccU=",
        strip_prefix = "dash.el-2.19.1/",
        target_name = "dash",
        urls = [
            "https://github.com/magnars/dash.el/archive/refs/tags/2.19.1.zip",  # 2021-08-26
        ],
    )
