#!/bin/sh

# Copyright 2026 Philipp Stephani
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

# Test whether the patches in .bcr/patches can be applied cleanly.

set -Ceux

cp -R -L -i -p -- "${TEST_SRCDIR:?}/${TEST_WORKSPACE:?}" "${TEST_TMPDIR:?}"

cd -- "${TEST_TMPDIR:?}/${TEST_WORKSPACE:?}"

for patch in .bcr/patches/*.patch; do
  patch -u -p1 -i "${patch:?}"
done
