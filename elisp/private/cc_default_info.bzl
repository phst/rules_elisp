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

"""Defines the internal `CcDefaultInfo` provider."""

load(":features.bzl", "parse_features")

visibility(["//elisp", "//elisp/toolchains"])

def _init_cc_default_info(*, features, defines, copts, linkopts):
    features, disabled_features = parse_features(features)

    # @unsorted-dict-items
    return {
        "features": features,
        "disabled_features": disabled_features,
        "defines": defines,
        "copts": copts,
        "linkopts": linkopts,
    }

CcDefaultInfo, _ = provider(
    doc = "Internal provider for default C++ flags",
    # @unsorted-dict-items
    fields = {
        "features": "Default features",
        "disabled_features": "Features to disable",
        "defines": "Local preprocessor definitions",
        "copts": "Default compiler flags",
        "linkopts": "Default linker flags",
    },
    init = _init_cc_default_info,
)
