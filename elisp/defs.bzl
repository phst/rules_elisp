# Copyright 2020-2025 Google LLC
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

"""Defines rules to work with Emacs Lisp files in Bazel.

This file is deprecated; use the rule-specific Starlark files in the packages
`elisp`, `elisp/proto`, `elisp/common` and `elisp/toolchains` instead, such as
//elisp:elisp_library.bzl."""

load("//elisp/common:elisp_info.bzl", _EmacsLispInfo = "EmacsLispInfo")
load("//elisp/proto:elisp_proto_library.bzl", _elisp_proto_library = "elisp_proto_library")
load("//elisp/toolchains:elisp_toolchain.bzl", _elisp_toolchain = "elisp_toolchain")
load(":elisp_binary.bzl", _elisp_binary = "elisp_binary")
load(":elisp_cc_module.bzl", _elisp_cc_module = "elisp_cc_module")
load(":elisp_library.bzl", _elisp_library = "elisp_library")
load(":elisp_manual.bzl", _elisp_manual = "elisp_manual")
load(":elisp_test.bzl", _elisp_test = "elisp_test")

visibility("public")

EmacsLispInfo = _EmacsLispInfo

elisp_library = _elisp_library
elisp_proto_library = _elisp_proto_library
elisp_cc_module = _elisp_cc_module
elisp_binary = _elisp_binary
elisp_test = _elisp_test
elisp_manual = _elisp_manual

elisp_toolchain = _elisp_toolchain
