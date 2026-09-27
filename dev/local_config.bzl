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

"""Defines the internal `local_config` repository rule."""

visibility("private")

def _local_config_impl(ctx):
    is_default_bazel_version = not ctx.getenv("USE_BAZEL_VERSION")
    makeinfo = _local_binary(ctx, "makeinfo", "MAKEINFO")
    xmllint = _local_binary(ctx, "xmllint", "XMLLINT")
    ctx.template(
        "BUILD.bazel",
        Label(":local_config.BUILD.template"),
        {
            '"[bzl_library.bzl]"': repr(str(Label("@bazel_skylib//:bzl_library.bzl"))),
            '"[docs_pkg]"': repr(str(Label("//docs:__pkg__"))),
        },
        executable = False,
    )
    ctx.template(
        "config.bzl",
        Label(":local_config.bzl.template"),
        {
            '"[makeinfo]"': repr(makeinfo),
            '"[xmllint]"': repr(xmllint),
            "[is_default_bazel_version]": repr(is_default_bazel_version),
        },
        executable = False,
    )

def _local_binary(ctx, program, var):
    windows = ctx.os.name.startswith("windows")
    program = program or fail("missing program name")
    if "/" in program or "\\" in program or program.startswith("-"):
        fail("invalid program name %r" % program)
    suffix = ".exe" if windows else ""

    value = ctx.getenv(var or fail("missing environment variable for program %r" % program))
    if value:
        if "/" in value or "\\" in value:
            file = ctx.path(value)
        else:
            if suffix and not value.lower().endswith(suffix):
                value += suffix
            file = ctx.which(value)
    else:
        file = ctx.which(program + suffix)

    if file:
        if not file.exists:
            fail("program file %r doesn’t exist" % str(file))
        ctx.watch(file)
        file = file.realpath
        ctx.watch(file)
        return str(file)

    # On Windows, retry with MSYS2.
    if windows:
        bash = ctx.getenv("BAZEL_SH") or fail("BAZEL_SH not set")
        result = ctx.execute(
            [bash, "-l", "-c", 'command -v -- "$1"', "-", program],
            timeout = 10,
        )
        if result.return_code != 0:
            fail("command -v %r failed, standard error:\n" % program, result.stderr)
        file = result.stdout.rstrip()
        if not file.startswith("/"):
            fail("program %r was found as %r instead of absolute file" % (program, file))
        return file

    fail("program %r not found" % program)

local_config = repository_rule(
    local = True,
    configure = True,
    implementation = _local_config_impl,
)
