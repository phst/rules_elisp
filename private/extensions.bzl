# Copyright 2023 Google LLC
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

load(":repositories.bzl", _deps = "non_module_deps", _dev_deps = "non_module_dev_deps")

visibility("private")

def _non_module_deps_impl(_ctx):
    _deps()

def _non_module_dev_deps_impl(_ctx):
    _dev_deps()

non_module_deps = module_extension(implementation = _non_module_deps_impl)
non_module_dev_deps = module_extension(implementation = _non_module_dev_deps_impl)
