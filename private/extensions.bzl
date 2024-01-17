# Copyright 2023, 2024 Google LLC
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

"""Non-module dependencies."""

load("@bazel_features//:features.bzl", "bazel_features")
load(":repositories.bzl", _config = "config", _deps = "non_module_deps", _dev_deps = "non_module_dev_deps")

visibility("private")

def _non_module_deps_impl(_ctx):
    _deps(name = "phst_rules_elisp_deps")

def _non_module_dev_deps_impl(_ctx):
    _dev_deps(name = "phst_rules_elisp_dev_deps")

def _config_impl(_ctx):
    _config(name = "phst_rules_elisp_config")

non_module_deps = module_extension(implementation = _non_module_deps_impl)
non_module_dev_deps = module_extension(implementation = _non_module_dev_deps_impl)

config = module_extension(
    implementation = _config_impl,
    **({"os_dependent": True} if bazel_features.external_deps.module_extension_has_os_arch_dependent else {})
)
