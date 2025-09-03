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

"""Defines the internal `module_config` rule."""

load(":cc_default_info.bzl", "CcDefaultInfo")
load(":features.bzl", "parse_features")
load(":module_config_info.bzl", "ModuleConfigInfo")

visibility(["//elisp"])

def _module_config_impl(ctx):
    """Implementation of the `module_config` rule."""
    features, disabled_features = parse_features(ctx.attr.features)
    return [
        CcDefaultInfo(
            features = features,
            disabled_features = disabled_features,
            defines = [],
            copts = [],
            linkopts = [ctx.expand_location(s) for s in ctx.attr.linkopts],
        ),
        ModuleConfigInfo(
            suffix = ctx.attr.suffix,
            additional_linker_inputs = ctx.files.srcs,
        ),
    ]

module_config = rule(
    doc = "Internal rule to configure Emacs modules",
    # @unsorted-dict-items
    attrs = {
        "suffix": attr.string(
            doc = "Filename suffix for Emacs modules",
            mandatory = True,
            values = [".so", ".dll", ".dylib"],
        ),
        "linkopts": attr.string_list(mandatory = True),
        # This ought to be called “additional_linker_inputs”, but
        # ctx.expand_location scans only a hard-coded list of attributes for
        # valid files, among them “srcs”.
        "srcs": attr.label_list(
            doc = "Additional linker inputs for linking Emacs modules",
            mandatory = True,
            allow_files = [".lds", ".def"],
        ),
    },
    provides = [CcDefaultInfo, ModuleConfigInfo],
    implementation = _module_config_impl,
)
