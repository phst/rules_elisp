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

def rules_elisp_dependencies():
    """Installs necessary dependencies for Emacs Lisp rules.

    Call this function in your `WORKSPACE` file.
    """
    http_archive(
        name = "gnu_emacs_27.1",
        build_file_content = _build_file(macos_arm = False),
        sha256 = "4a4c128f915fc937d61edfc273c98106711b540c9be3cd5d2e2b9b5b2f172e41",
        strip_prefix = "emacs-27.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz",
        ],
    )
    http_archive(
        name = "gnu_emacs_27.2",
        build_file_content = _build_file(macos_arm = True),
        sha256 = "b4a7cc4e78e63f378624e0919215b910af5bb2a0afc819fad298272e9f40c1b9",
        strip_prefix = "emacs-27.2/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.xz",
        ],
    )
    http_archive(
        name = "gnu_emacs_28.1",
        build_file_content = _build_file(macos_arm = True),
        sha256 = "28b1b3d099037a088f0a4ca251d7e7262eab5ea1677aabffa6c4426961ad75e1",
        strip_prefix = "emacs-28.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-28.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-28.1.tar.xz",
        ],
    )
    http_archive(
        name = "bazel_skylib",
        sha256 = "f7be3474d42aae265405a592bb7da8e171919d74c16f082a5457840f06054728",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.2.1/bazel-skylib-1.2.1.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.2.1/bazel-skylib-1.2.1.tar.gz",
        ],
    )
    http_archive(
        name = "com_google_absl",
        sha256 = "a4567ff02faca671b95e31d315bab18b42b6c6f1a60e91c6ea84e5a2142112c2",
        strip_prefix = "abseil-cpp-20211102.0/",
        urls = [
            "https://github.com/abseil/abseil-cpp/archive/refs/tags/20211102.0.zip",  # 2021-11-03
        ],
    )
    http_archive(
        name = "com_google_protobuf",
        sha256 = "8b28fdd45bab62d15db232ec404248901842e5340299a57765e48abe8a80d930",
        strip_prefix = "protobuf-3.20.1/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v3.20.1.tar.gz",  # 2022-04-22
        ],
    )
    http_archive(
        name = "upb",
        sha256 = "13650f365c8f66b71e1da27ab7c59ddde1c52ab4fca19629c4970b7d4e6b935c",
        strip_prefix = "upb-c7620a4690272eab892bed56b2617b7f841bdc82/",
        urls = [
            "https://github.com/protocolbuffers/upb/archive/c7620a4690272eab892bed56b2617b7f841bdc82.zip",  # 2022-04-29
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

def _build_file(macos_arm):
    return _BUILD_TEMPLATE.format(
        module_header = '"emacs-module.h"',
        dump_mode = "portable",
        windows = "",
        macos_x86 = "",
        macos_arm = "" if macos_arm else '"@phst_rules_elisp//emacs:incompatible"',
    )

_BUILD_TEMPLATE = """
load("@phst_rules_elisp//emacs:defs.bzl", "emacs_binary")

emacs_binary(
    name = "emacs",
    srcs = glob(["**"]),
    builtin_features = "builtin_features.json",
    dump_mode = "{dump_mode}",
    module_header = {module_header},
    readme = "README",
    target_compatible_with = select({{
        "@phst_rules_elisp//emacs:always_supported": [],
        "@phst_rules_elisp//emacs:windows": [{windows}],
        "@phst_rules_elisp//emacs:macos_arm": [{macos_arm}],
        "@phst_rules_elisp//emacs:macos_x86": [{macos_x86}],
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
    ],
)
"""
