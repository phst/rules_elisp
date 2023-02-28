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
        build_file = Label("//:emacs.BUILD"),
        sha256 = "4a4c128f915fc937d61edfc273c98106711b540c9be3cd5d2e2b9b5b2f172e41",
        strip_prefix = "emacs-27.1/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.1.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.1.tar.xz",
        ],
        patches = [Label("//:emacs-27.patch")],
    )
    maybe(
        http_archive,
        name = "gnu_emacs_27.2",
        build_file = Label("//:emacs.BUILD"),
        sha256 = "b4a7cc4e78e63f378624e0919215b910af5bb2a0afc819fad298272e9f40c1b9",
        strip_prefix = "emacs-27.2/",
        urls = [
            "https://ftpmirror.gnu.org/emacs/emacs-27.2.tar.xz",
            "https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.xz",
        ],
        patches = [Label("//:emacs-27.patch")],
    )
    maybe(
        http_archive,
        name = "gnu_emacs_28.1",
        build_file = Label("//:emacs.BUILD"),
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
        build_file = Label("//:emacs.BUILD"),
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
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.5/platforms-0.0.5.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.5/platforms-0.0.5.tar.gz",
        ],
        sha256 = "379113459b0feaf6bfbb584a91874c065078aa673222846ac765f86661c27407",
    )
    maybe(
        http_archive,
        name = "bazel_skylib",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.0/bazel-skylib-1.4.0.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.0/bazel-skylib-1.4.0.tar.gz",
        ],
        sha256 = "f24ab666394232f834f74d19e2ff142b0af17466ea0c69a3f4c276ee75f6efce",
    )
    maybe(
        http_archive,
        name = "rules_license",
        sha256 = "6157e1e68378532d0241ecd15d3c45f6e5cfd98fc10846045509fb2a7cc9e381",
        urls = ["https://github.com/bazelbuild/rules_license/releases/download/0.0.4/rules_license-0.0.4.tar.gz"],
    )
    maybe(
        http_archive,
        name = "com_google_absl",
        sha256 = "a4567ff02faca671b95e31d315bab18b42b6c6f1a60e91c6ea84e5a2142112c2",
        strip_prefix = "abseil-cpp-20211102.0/",
        urls = [
            "https://github.com/abseil/abseil-cpp/archive/refs/tags/20211102.0.zip",  # 2021-11-03
        ],
    )
    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "e13ca6c2f1522924b8482f3b3a482427d0589ff8ea251088f7e39f4713236053",
        strip_prefix = "protobuf-21.7/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.7.zip",  # 2022-09-29
        ],
    )
    maybe(
        http_archive,
        name = "upb",
        patches = ["@//:upb.patch"],
        sha256 = "0d6af8c8c00b3d733721f8d890ef43dd40f537c2e815b529085c1a6c30a21084",
        strip_prefix = "upb-a5477045acaa34586420942098f5fecd3570f577/",
        urls = [
            "https://github.com/protocolbuffers/upb/archive/a5477045acaa34586420942098f5fecd3570f577.zip",  # 2022-09-23
        ],
    )
    _toolchains(name = "phst_rules_elisp_toolchains")

# buildifier: disable=unnamed-macro
def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("@phst_rules_elisp//elisp:hermetic_toolchain")

def _toolchains_impl(repository_ctx):
    windows = repository_ctx.os.name.startswith("windows")
    target = Label("//elisp:windows-toolchains.BUILD" if windows else "//elisp:unix-toolchains.BUILD")
    repository_ctx.symlink(target, "BUILD")

_toolchains = repository_rule(
    implementation = _toolchains_impl,
)
