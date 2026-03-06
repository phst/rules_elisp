# Copyright 2025, 2026 Philipp Stephani
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

"""Defines the internal `local_binary` repository rule."""

visibility("private")

def _local_binary_impl(ctx):
    windows = ctx.os.name.startswith("windows")
    environment = ctx.attr.environment or fail("missing environment variable name")
    program = ctx.attr.program or fail("missing program name")
    if "/" in program or "\\" in program or program.startswith("-"):
        fail("invalid program name %r" % program)
    program = ctx.getenv(environment, program)
    suffix = ".exe" if windows and not program.lower().endswith(".exe") else ""
    file = ctx.which(program + suffix)
    virtual = False
    if not file and windows:
        # On Windows, retry with MSYS2.
        bash = ctx.getenv("BAZEL_SH") or fail("BAZEL_SH not set")
        result = ctx.execute(
            [bash, "-l", "-c", 'command -v -- "$1"', "-", program],
            timeout = 10,
        )
        if result.return_code != 0:
            fail("command -v failed, standard error:\n", result.stderr)
        file = ctx.path(result.stdout.rstrip())
        virtual = True
    if not file:
        fail("program %r not found" % program)
    if not (virtual or file.exists):
        fail("program file %s doesn’t exist" % file)
    ctx.template(
        "BUILD.bazel",
        Label(":local_binary.BUILD.template"),
        {
            '"[bzl_library.bzl]"': repr(str(Label("@bazel_skylib//:bzl_library.bzl"))),
            "[[visibility]]": repr([str(v) for v in ctx.attr.library_visibility]),
        },
        executable = False,
    )
    ctx.template(
        "file.bzl",
        Label(":local_binary.bzl.template"),
        {
            '"[file]"': repr(str(file)),
        },
        executable = False,
    )

local_binary = repository_rule(
    # @unsorted-dict-items
    attrs = {
        "environment": attr.string(mandatory = True),
        "program": attr.string(mandatory = True),
        "library_visibility": attr.label_list(mandatory = True, allow_empty = False),
    },
    local = True,
    configure = True,
    implementation = _local_binary_impl,
)
