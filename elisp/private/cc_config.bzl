# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Defines shared C/C++ configuration constants for this repository."""

visibility(["//elisp/private/tools", "//elisp/proto"])

FEATURES = select({
    Label("//private:treat_warnings_as_errors_enabled"): ["treat_warnings_as_errors"],
    Label("//conditions:default"): [],
})

# Shared C++ compilation options.
COPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [
        "/W4",
        "/utf-8",
        "/permissive-",
        "/Zc:__cplusplus",
        "/external:W2",  # TODO: shouldn’t be needed; file bug against rules_cc
    ],
    Label(":gcc_or_clang"): [
        "-finput-charset=utf-8",
        "-fexec-charset=utf-8",
        "-Wall",
        "-Wextra",
        "-Wconversion",
        "-Wsign-conversion",
        "-pedantic",
    ],
}) + select({
    Label("@rules_cc//cc/compiler:clang"): [
        # Work around https://github.com/llvm/llvm-project/issues/121984.
        "--system-header-prefix=absl/",
        "--system-header-prefix=google/",
        "--system-header-prefix=tools/",
        "--system-header-prefix=upb/",
    ],
    Label("//conditions:default"): [],
})

CXXOPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [],
    Label("@rules_cc//cc/compiler:gcc"): [
        # GCC appears to treat some moves as redundant that are in fact
        # necessary.
        "-Wno-redundant-move",
    ],
    Label("@rules_cc//cc/compiler:clang"): [],
})

CONLYOPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [],
    Label(":gcc_or_clang"): ["-Wvla"],
})

DEFINES = [
    # https://pubs.opengroup.org/onlinepubs/9699919799/functions/V2_chap02.html#tag_15_02_01
    "_POSIX_C_SOURCE=200809L",
    "_XOPEN_SOURCE=700",
] + select({
    Label("@platforms//os:linux"): [],
    Label("@platforms//os:macos"): [],
    Label("@platforms//os:windows"): [
        "_UNICODE",
        "UNICODE",
        "STRICT",
        "NOMINMAX",
        "WIN32_LEAN_AND_MEAN",
    ],
})

LINKOPTS = []
