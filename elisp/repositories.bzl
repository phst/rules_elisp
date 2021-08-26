# Copyright 2020, 2021 Google LLC
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
        name = "gnu_emacs_26.1",
        build_file_content = _build_file(macos_arm = False, portable = False),
        sha256 = "1cf4fc240cd77c25309d15e18593789c8dbfba5c2b44d8f77c886542300fd32c",
        strip_prefix = "emacs-26.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-26.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-26.1.tar.xz",
        ],
    )
    http_archive(
        name = "gnu_emacs_26.2",
        build_file_content = _build_file(macos_arm = False, portable = False),
        sha256 = "151ce69dbe5b809d4492ffae4a4b153b2778459de6deb26f35691e1281a9c58e",
        strip_prefix = "emacs-26.2/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-26.2.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-26.2.tar.xz",
        ],
    )
    http_archive(
        name = "gnu_emacs_26.3",
        build_file_content = _build_file(macos_arm = False, portable = False),
        sha256 = "4d90e6751ad8967822c6e092db07466b9d383ef1653feb2f95c93e7de66d3485",
        strip_prefix = "emacs-26.3/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-26.3.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-26.3.tar.xz",
        ],
    )
    http_archive(
        name = "gnu_emacs_27.1",
        build_file_content = _build_file(macos_arm = False, portable = True),
        sha256 = "4a4c128f915fc937d61edfc273c98106711b540c9be3cd5d2e2b9b5b2f172e41",
        strip_prefix = "emacs-27.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz",
        ],
    )
    http_archive(
        name = "gnu_emacs_27.2",
        build_file_content = _build_file(macos_arm = True, portable = True),
        sha256 = "b4a7cc4e78e63f378624e0919215b910af5bb2a0afc819fad298272e9f40c1b9",
        strip_prefix = "emacs-27.2/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.xz",
        ],
    )
    http_archive(
        name = "bazel_skylib",
        sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c",
        urls = [
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
        ],
    )
    http_archive(
        name = "com_google_absl",
        sha256 = "59b862f50e710277f8ede96f083a5bb8d7c9595376146838b9580be90374ee1f",
        strip_prefix = "abseil-cpp-20210324.2/",
        urls = ["https://github.com/abseil/abseil-cpp/archive/refs/tags/20210324.2.tar.gz"],
    )
    http_archive(
        name = "nlohmann_json",
        build_file = "@phst_rules_elisp//:json.BUILD",
        sha256 = "61e605be15e88deeac4582aaf01c09d616f8302edde7adcaba9261ddc3b4ceca",
        strip_prefix = "single_include",
        urls = ["https://github.com/nlohmann/json/releases/download/v3.10.2/include.zip"],
    )

# buildifier: disable=unnamed-macro
def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("@phst_rules_elisp//elisp:hermetic_toolchain")

def _build_file(portable, macos_arm):
    return _BUILD_TEMPLATE.format(
        module_header = '"emacs-module.h"' if portable else "None",
        dump_mode = "portable" if portable else "unexec",
        macos_x86 = "" if portable else '"@phst_rules_elisp//emacs:incompatible"',
        macos_arm = "" if macos_arm else '"@phst_rules_elisp//emacs:incompatible"',
    )

_BUILD_TEMPLATE = """
load("@phst_rules_elisp//emacs:defs.bzl", "emacs_binary")

emacs_binary(
    name = "emacs",
    srcs = glob(["**"]),
    dump_mode = "{dump_mode}",
    module_header = {module_header},
    readme = "README",
    target_compatible_with = select({{
        "@phst_rules_elisp//emacs:always_supported": [],
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
"""
