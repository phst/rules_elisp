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

workspace(name = "phst_rules_elisp")

load("//elisp:repositories.bzl", "rules_elisp_dependencies", "rules_elisp_toolchains")

rules_elisp_dependencies()

rules_elisp_toolchains()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "36b8d6c2260068b9ff82faea2f7add164bf3436eac9ba3ec14809f335346d66a",
    strip_prefix = "stardoc-0.4.0/",
    urls = ["https://github.com/bazelbuild/stardoc/archive/0.4.0.zip"],
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

stardoc_repositories()

local_repository(
    name = "example",
    path = "examples/ext",
)
