# Copyright 2020-2026 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Defines the internal `cc_launcher_rule` function."""

load("@rules_cc//cc:use_cc_toolchain.bzl", "CC_TOOLCHAIN_ATTRS", "use_cc_toolchain")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load(":cc_launcher_config.bzl", "LAUNCHER_ATTRS", "LAUNCHER_DEPS")

visibility([
    # keep sorted
    "//elisp",
    "//elisp/toolchains",
])

def cc_launcher_rule(*, launcher_deps, attrs = {}, fragments = [], toolchains = [], **kwargs):
    return rule(
        # FIXME: Remove CC_TOOLCHAIN_ATTRS once
        # https://github.com/bazelbuild/bazel/issues/7260 is fixed.
        attrs = CC_TOOLCHAIN_ATTRS | LAUNCHER_ATTRS | {
            "_launcher_deps": attr.label_list(
                default = LAUNCHER_DEPS + launcher_deps,
                providers = [CcInfo],
            ),
        } | attrs,
        fragments = ["cpp"] + fragments,
        toolchains = use_cc_toolchain() + toolchains,
        **kwargs
    )
