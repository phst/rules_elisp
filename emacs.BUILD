# Copyright 2020, 2021, 2022, 2024 Google LLC
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

load("[defs_bzl]", "emacs_binary")

package(
    default_visibility = ["//visibility:private"],
    features = [
        "layering_check",
        "parse_headers",
        "treat_warnings_as_errors",
        "-macos_default_link_flags",
    ],
)

emacs_binary(
    name = "emacs",
    srcs = glob(
        ["**"],
        allow_empty = False,
    ),
    builtin_features = "builtin_features.json",
    module_header = "emacs-module.h",
    readme = "README",
    visibility = ["[emacs_pkg]"],
)

cc_library(
    name = "module_header",
    hdrs = ["emacs-module.h"],
    features = ["-default_link_libs"],
    linkstatic = True,
    visibility = ["[emacs_pkg]"],
)

filegroup(
    name = "builtin_features",
    srcs = ["builtin_features.json"],
    visibility = ["[emacs_pkg]"],
)

# Local Variables:
# mode: bazel-build
# End:
