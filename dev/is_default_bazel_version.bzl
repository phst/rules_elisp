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

"""Defines the internal `is_default_bazel_version` repository rule."""

visibility("private")

def _is_default_bazel_version_impl(ctx):
    is_default_bazel_version = not ctx.getenv("USE_BAZEL_VERSION")
    ctx.file("BUILD.bazel", executable = False)
    ctx.template(
        "is_default_bazel_version.bzl",
        Label(":is_default_bazel_version.bzl.template"),
        {
            "[is_default_bazel_version]": repr(is_default_bazel_version),
        },
        executable = False,
    )

is_default_bazel_version = repository_rule(
    local = True,
    configure = True,
    implementation = _is_default_bazel_version_impl,
)
