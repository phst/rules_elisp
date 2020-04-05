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

"""Contains workspace functions to use Emacs Lisp rules."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def rules_elisp_dependencies():
    """Installs necessary dependencies for Emacs Lisp rules.

    Call this function in your `WORKSPACE` file.
    """
    http_archive(
        name = "gnu_emacs_stable",
        build_file = "@phst_rules_elisp//emacs:emacs.BUILD",
        sha256 = "4d90e6751ad8967822c6e092db07466b9d383ef1653feb2f95c93e7de66d3485",
        strip_prefix = "emacs-26.3/",
        urls = ["https://ftp.gnu.org/gnu/emacs/emacs-26.3.tar.xz"],
    )
    http_archive(
        name = "com_google_protobuf",
        sha256 = "cf754718b0aa945b00550ed7962ddc167167bd922b842199eeb6505e6f344852",
        strip_prefix = "protobuf-3.11.3",
        urls = [
            "https://mirror.bazel.build/github.com/protocolbuffers/protobuf/archive/v3.11.3.tar.gz",
            "https://github.com/protocolbuffers/protobuf/archive/v3.11.3.tar.gz",
        ],
    )

def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("//elisp:all")
