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

"""Defines shared C/C++ configuration constants for this repository."""

visibility([
    # keep sorted
    "//elisp/private",
    "//elisp/private/tools",
    "//elisp/proto",
    "//tests/tools",
])

FEATURES = select({
    Label("//private:treat_warnings_as_errors_enabled"): ["treat_warnings_as_errors"],
    Label("//conditions:default"): [],
})

# Shared C++ compilation options.
COPTS = select({
    Label(":msvc_or_clang_cl"): [
        "/W4",
        "/utf-8",
        "/permissive-",
        "/Zc:__cplusplus",
        "/external:W0",  # TODO: shouldn’t be needed; file bug against rules_cc
    ],
    Label(":gcc_or_clang"): [
        "-finput-charset=utf-8",
        "-fexec-charset=utf-8",
        "-fvisibility=hidden",
        "-Wall",
        "-Wextra",
        "-Wconversion",
        "-Wsign-conversion",
        "-pedantic",
    ],
}) + select({
    Label("@rules_cc//cc/compiler:clang"): [
        "-Wno-nullability-extension",
        # Work around https://github.com/llvm/llvm-project/issues/121984.
        "--system-header-prefix=absl/",
        "--system-header-prefix=google/",
        "--system-header-prefix=tools/",
        "--system-header-prefix=upb/",
    ],
    Label("//conditions:default"): [],
}) + select({
    Label("@rules_cc//cc/compiler:clang-cl"): [
        # Allow Clang-CL to include windows.h without error.
        "-Wno-error=ignored-attributes",
        "-Wno-error=ignored-pragma-intrinsic",
        "-Wno-error=macro-redefined",
        "-Wno-error=pragma-pack",
        "-Wno-error=unknown-pragmas",
    ],
    Label("//conditions:default"): [],
}) + select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [
        "/wd4702",  # lots of false positives in external templates
    ],
    Label("//conditions:default"): [],
})

CXXOPTS = select({
    Label(":msvc_or_clang_cl"): [],
    Label("@rules_cc//cc/compiler:gcc"): [
        # GCC appears to treat some moves as redundant that are in fact
        # necessary.
        "-Wno-redundant-move",
    ],
    Label("@rules_cc//cc/compiler:clang"): [],
}) + select({
    Label(":gcc_or_clang"): ["-fvisibility-inlines-hidden"],
    Label("//conditions:default"): [],
})

CONLYOPTS = select({
    Label(":msvc_or_clang_cl"): ["/Zc:__STDC__"],
    Label(":gcc_or_clang"): ["-Wvla"],
})

DEFINES = [
    # https://pubs.opengroup.org/onlinepubs/9799919799/functions/V2_chap02.html#tag_16_02_01_01
    "_POSIX_C_SOURCE=202405L",
    # https://pubs.opengroup.org/onlinepubs/9799919799/functions/V2_chap02.html#tag_16_02_01_02
    "_XOPEN_SOURCE=800",
] + select({
    Label("@platforms//os:linux"): [],
    Label("@platforms//os:macos"): [
        # The macOS headers lack some POSIX functionality even if
        # _POSIX_C_SOURCE is defined.  We need to define _DARWIN_C_SOURCE, too.
        "_DARWIN_C_SOURCE",
    ],
    Label("@platforms//os:windows"): [
        "_UNICODE",
        "UNICODE",
        "STRICT",
        "NOMINMAX",
        "WIN32_LEAN_AND_MEAN",
        "NONAMELESSUNION",
    ],
})

LINKOPTS = []

# These libraries have to be added to a cc_library dependencies to defined the
# BAZEL_CURRENT_REPOSITORY preprocessor symbol.
RUNFILES_LIBS = [
    # FIXME: Remove the next dependency once we drop support for Bazel
    # before 8.1.0.  See https://github.com/bazelbuild/rules_cc/issues/285.
    Label("@bazel_tools//tools/cpp/runfiles"),
    Label("@rules_cc//cc/runfiles"),
]
