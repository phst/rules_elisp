# Copyright 2023, 2024 Philipp Stephani
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

# Generated file; do not edit.

"""Internal definitions for the `phst_rules_elisp_deps` repository."""

BAZEL_VERSION = "[bazel_version]"

# Starlark doesn’t have `chr` or `ord` functions, so we replicate them here.
# CHR maps ordinals to single-character strings, and ORD does the reverse.  This
# only works for single-byte characters.
CHR = [[[chr]]]
ORD = {[[ord]]}
