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
    program = ctx.attr.program or fail("missing program name")
    if "/" in program or "\\" in program or program.startswith("-"):
        fail("invalid program name %r" % program)
    if windows:
        file = ctx.which(program + ".exe")
        if file:
            if not file.exists:
                fail("program file %r doesn’t exist" % str(file))
            file = str(file.realpath)
            version = _version(ctx, [file, "--version"])
        else:
            # On Windows, retry with MSYS2.
            bash = ctx.getenv("BAZEL_SH") or fail("BAZEL_SH not set")
            result = ctx.execute(
                [bash, "-l", "-c", 'command -v -- "$1"', "-", program],
                timeout = 10,
            )
            if result.return_code != 0:
                fail("command -v failed, standard error:\n", result.stderr)
            file = result.stdout.rstrip()
            version = _version(ctx, [bash, "-l", "-c", 'exec "$0" --version', file])
    else:
        file = ctx.which(program) or fail("program %r not found" % program)
        if not file.exists:
            fail("program file %r doesn’t exist" % str(file))
        file = str(file.realpath)
        version = _version(ctx, [file, "--version"])
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
            '"[file]"': repr(file),
            '"[version]"': repr(version),
        },
        executable = False,
    )

local_binary = repository_rule(
    # @unsorted-dict-items
    attrs = {
        "program": attr.string(mandatory = True),
        "library_visibility": attr.label_list(mandatory = True, allow_empty = False),
    },
    local = True,
    configure = True,
    implementation = _local_binary_impl,
)

def _version(ctx, command):
    result = ctx.execute(command, timeout = 10)
    if result.return_code != 0:
        fail("%r failed, standard error:\n%s" % (command, result.stderr))
    lines = result.stdout.splitlines() + result.stderr.splitlines()
    return lines[0] if lines else None
