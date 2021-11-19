# Copyright 2021 Google LLC
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

"""Internal-only rules."""

def _requirements_txt_impl(repository_ctx):
    """Implementation of the “copy_requirements_txt” repository rule."""
    prefixes = {
        "linux": "linux",
        "mac os x": "macos",
        "windows server 2019": "windows",
    }
    prefix = prefixes.get(repository_ctx.os.name, None)
    if not prefix:
        fail("Unsupported operating system “{}”".format(repository_ctx.os.name))
    repository_ctx.symlink(
        Label("@//:{}-requirements.txt".format(prefix)),
        "requirements.txt",
    )
    repository_ctx.file(
        "BUILD",
        'exports_files(["requirements.txt"])',
        executable = False,
    )

requirements_txt = repository_rule(
    implementation = _requirements_txt_impl,
    local = True,
    doc = "Generates requirements.txt for the current platform.",
)
