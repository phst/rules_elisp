# Copyright 2020-2026 Google LLC
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

"""Defines the rule `local_binary`, which runs a locally-installed Emacs."""

load("@rules_cc//cc:use_cc_toolchain.bzl", "use_cc_toolchain")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load("//elisp/private:cc_launcher.bzl", "cc_launcher")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_ATTRS", "LAUNCHER_DEPS")
load("//elisp/private:cc_literals.bzl", "cc_string")

# Not really public, but loaded by external generated repositories
visibility("public")

def _local_binary_impl(ctx):
    """Rule implementation of the “local_binary” rule."""
    executable, runfiles = cc_launcher(
        ctx,
        defines = [
            "RULES_ELISP_LOCAL=1",
            "RULES_ELISP_PROGRAM=" + cc_string(ctx.attr.program),
        ],
    )
    return DefaultInfo(
        executable = executable,
        files = depset(direct = [executable]),
        runfiles = runfiles,
    )

local_binary = rule(
    attrs = LAUNCHER_ATTRS | {
        "program": attr.string(mandatory = True),
        "_launcher_deps": attr.label_list(
            default = LAUNCHER_DEPS + [Label("//elisp/private/tools:local")],
            providers = [CcInfo],
        ),
    },
    executable = True,
    fragments = ["cpp"],
    toolchains = use_cc_toolchain(),
    implementation = _local_binary_impl,
)
