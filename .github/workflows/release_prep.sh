#!/bin/sh

# Copyright 2026 Philipp Stephani
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

# Generate release archive.  See
# https://github.com/bazel-contrib/.github/blob/master/.github/workflows/release_ruleset.yaml.

# shellcheck disable=SC3040
set -Cefux -o pipefail

tag="$1"
version="${tag:1}"
archive="rules_elisp-${tag:?}.tar.gz"

git archive --output="${archive:?}" "refs/tags/${tag:?}"

cat <<EOF
To use, add this to your \`MODULE.bazel\` file:

\`\`\`starlark
bazel_dep(name = "rules_elisp", version = "${version:?}")
\`\`\`
EOF
