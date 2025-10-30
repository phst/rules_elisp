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

"""Defines the internal `cc_launcher` function."""

load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load(":cc_default_info.bzl", "CcDefaultInfo")

visibility(["//elisp", "//elisp/toolchains"])

def cc_launcher(ctx, *, defines):
    """Builds a launcher executable that starts Emacs.

    The current rule must provide the following attributes:
    - `_launcher_srcs`: a list of C++ source files to be compiled
    - `_launcher_deps`: a list of `cc_library` targets that the launcher depends
      on
    - `_launcher_defaults`: a `cc_defaults` rule to provide default settings

    Args:
      ctx (ctx): rule context
      defines (list of strings): additional local preprocessor definitions

    Returns:
      a pair `(executable, runfiles)` where `executable` is a `File` object
      representing the executable that starts Emacs and `runfiles` is a
      `runfiles` object for the runfiles that the executable will need
    """
    cc_toolchain = find_cc_toolchain(ctx)
    deps = ctx.attr._launcher_deps
    infos = [dep[CcInfo] for dep in deps]
    defaults = ctx.attr._launcher_defaults[CcDefaultInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = defaults.features + ctx.features,
        unsupported_features = defaults.disabled_features + ctx.disabled_features,
    )
    _, objs = cc_common.compile(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        srcs = ctx.files._launcher_srcs,
        compilation_contexts = [info.compilation_context for info in infos],
        local_defines = defaults.defines + defines,
        user_compile_flags = defaults.copts,
    )
    bin = cc_common.link(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = objs,
        linking_contexts = [info.linking_context for info in infos],
        user_link_flags = defaults.linkopts,
    )
    runfiles = ctx.runfiles().merge_all([dep[DefaultInfo].default_runfiles for dep in deps])
    return bin.executable, runfiles
