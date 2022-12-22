# Copyright 2020, 2021, 2022 Google LLC
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

"""Contains workspace functions to use Emacs Lisp rules."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

def rules_elisp_dependencies():
    """Installs necessary dependencies for Emacs Lisp rules.

    Call this function in your `WORKSPACE` file.
    """
    maybe(
        http_archive,
        name = "gnu_emacs_27.1",
        build_file_content = _build_file(macos_arm = False),
        sha256 = "4a4c128f915fc937d61edfc273c98106711b540c9be3cd5d2e2b9b5b2f172e41",
        strip_prefix = "emacs-27.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz",
        ],
        patches = ["@phst_rules_elisp//:emacs-27.patch"],
    )
    maybe(
        http_archive,
        name = "gnu_emacs_27.2",
        build_file_content = _build_file(macos_arm = True),
        sha256 = "b4a7cc4e78e63f378624e0919215b910af5bb2a0afc819fad298272e9f40c1b9",
        strip_prefix = "emacs-27.2/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.xz",
        ],
        patches = ["@phst_rules_elisp//:emacs-27.patch"],
    )
    maybe(
        http_archive,
        name = "gnu_emacs_28.1",
        build_file_content = _build_file(macos_arm = True),
        sha256 = "28b1b3d099037a088f0a4ca251d7e7262eab5ea1677aabffa6c4426961ad75e1",
        strip_prefix = "emacs-28.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-28.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-28.1.tar.xz",
        ],
    )
    maybe(
        http_archive,
        name = "gnu_emacs_28.2",
        build_file_content = _build_file(macos_arm = True),
        sha256 = "ee21182233ef3232dc97b486af2d86e14042dbb65bbc535df562c3a858232488",
        strip_prefix = "emacs-28.2/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-28.2.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-28.2.tar.xz",
        ],
    )
    maybe(
        http_archive,
        name = "platforms",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.6/platforms-0.0.6.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.6/platforms-0.0.6.tar.gz",
        ],
        sha256 = "5308fc1d8865406a49427ba24a9ab53087f17f5266a7aabbfc28823f3916e1ca",
    )
    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.3.0/bazel-skylib-1.3.0.tar.gz",
        ],
        sha256 = "74d544d96f4a5bb630d465ca8bbcfe231e3594e5aae57e1edbf17a6eb3ca2506",
    )
    maybe(
        http_archive,
        name = "com_google_absl",
        sha256 = "54707f411cb62a26a776dad5fd60829098c181700edcd022ea5c2ca49e9b7ef1",
        strip_prefix = "abseil-cpp-20220623.1/",
        urls = [
            "https://github.com/abseil/abseil-cpp/archive/refs/tags/20220623.1.zip",  # 2022-08-31
        ],
    )
    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "0faa3f28b150efcaf044d33a05f613e3b052b852f41d2d420eb7ad49d11a06df",
        strip_prefix = "protobuf-21.11/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.11.zip",  # 2022-12-08
        ],
    )
    maybe(
        http_archive,
        name = "upb",
        sha256 = "13f99a9b2591fdb6dc2997c619e0cf77655047b23fa8fd30c139ae4e8669e391",
        strip_prefix = "upb-83f4988561baf5951bce6f07ddaa1cb325ba0241/",
        urls = [
            "https://github.com/protocolbuffers/upb/archive/83f4988561baf5951bce6f07ddaa1cb325ba0241.zip",  # 2022-06-23
        ],
    )
    _toolchains(name = "phst_rules_elisp_toolchains")

# buildifier: disable=unnamed-macro
def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("@phst_rules_elisp//elisp:hermetic_toolchain")

def _toolchains_impl(repository_ctx):
    if repository_ctx.os.name.startswith("windows"):
        content = _WINDOWS_TOOLCHAINS
    else:
        content = _UNIX_TOOLCHAINS
    repository_ctx.file("BUILD", content)

_toolchains = repository_rule(
    implementation = _toolchains_impl,
)

def _build_file(*, macos_arm):
    return _BUILD_TEMPLATE.format(
        macos_arm = "" if macos_arm else '"@phst_rules_elisp//emacs:incompatible"',
    )

_BUILD_TEMPLATE = """
load("@phst_rules_elisp//emacs:defs.bzl", "emacs_binary")

emacs_binary(
    name = "emacs",
    srcs = glob(["**"]),
    builtin_features = "builtin_features.json",
    module_header = "emacs-module.h",
    readme = "README",
    target_compatible_with = select({{
        "@phst_rules_elisp//emacs:always_supported": [],
        "@phst_rules_elisp//emacs:macos_arm": [{macos_arm}],
        "//conditions:default": ["@phst_rules_elisp//emacs:incompatible"],
    }}),
    visibility = ["@phst_rules_elisp//emacs:__pkg__"],
)

cc_library(
    name = "module_header",
    srcs = ["emacs-module.h"],
    visibility = ["@phst_rules_elisp//emacs:__pkg__"],
)

filegroup(
    name = "builtin_features",
    srcs = ["builtin_features.json"],
    visibility = ["@phst_rules_elisp//emacs:__pkg__"],
)
"""

_UNIX_TOOLCHAINS = """
alias(
    name = "emacs_cc_toolchain",
    actual = "@bazel_tools//tools/cpp:current_cc_toolchain",
    visibility = [
        "@gnu_emacs_27.1//:__pkg__",
        "@gnu_emacs_27.2//:__pkg__",
        "@gnu_emacs_28.1//:__pkg__",
        "@gnu_emacs_28.2//:__pkg__",
    ],
)
"""

_WINDOWS_TOOLCHAINS = """
cc_toolchain_suite(
    name = "emacs_cc_toolchain",
    toolchains = {
        "x64_windows": "@local_config_cc//:cc-compiler-x64_windows_mingw",
        "x64_windows|mingw-gcc": "@local_config_cc//:cc-compiler-x64_windows_mingw",
    },
    visibility = [
        "@gnu_emacs_27.1//:__pkg__",
        "@gnu_emacs_27.2//:__pkg__",
        "@gnu_emacs_28.1//:__pkg__",
        "@gnu_emacs_28.2//:__pkg__",
    ],
)
"""
