# Copyright 2021-2025 Google LLC
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

"""Exposes constants to configure C++ launchers."""

load(":cc_config.bzl", "COPTS", "CXXOPTS", "DEFINES", "FEATURES", "LINKOPTS")
load(":cc_default_info.bzl", "CcDefaultInfo")

visibility([
    "//elisp",
    "//elisp/private/tools",
    "//elisp/toolchains",
    "//tests/wrap",
])

LAUNCHER_FEATURES = FEATURES
LAUNCHER_COPTS = COPTS + CXXOPTS
LAUNCHER_DEFINES = DEFINES
LAUNCHER_LINKOPTS = LINKOPTS + select({
    Label(":msvc_or_clang_cl"): ["/SUBSYSTEM:CONSOLE"],
    Label(":gcc_or_clang"): [],
})

# @unsorted-dict-items
LAUNCHER_ATTRS = {
    "_launcher_srcs": attr.label_list(
        default = [Label("//elisp/private/tools:launcher.cc")],
        allow_files = [".cc"],
    ),
    "_launcher_defaults": attr.label(
        default = Label("//elisp/private:launcher_defaults"),
        providers = [CcDefaultInfo],
    ),
}

LAUNCHER_DEPS = [
    Label("//elisp/private/tools:platform"),
    Label("@abseil-cpp//absl/base:log_severity"),
    Label("@abseil-cpp//absl/container:fixed_array"),
    Label("@abseil-cpp//absl/log"),
    Label("@abseil-cpp//absl/log:globals"),
    Label("@abseil-cpp//absl/log:initialize"),
    Label("@abseil-cpp//absl/meta:type_traits"),
    Label("@abseil-cpp//absl/status"),
    Label("@abseil-cpp//absl/status:statusor"),
    Label("@abseil-cpp//absl/types:span"),
]
