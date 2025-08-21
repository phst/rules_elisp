# Copyright 2025 Philipp Stephani
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
    file = ctx.which(program + (".exe" if windows else ""))
    if not file:
        # On Windows, retry with MSYS2.
        bash = ctx.getenv("BAZEL_SH") or fail("BAZEL_SH not set")
        result = ctx.execute(
            [bash, "-l", "-c", 'which "$0"', program],
            timeout = 10,
        )
        if result.return_code != 0:
            fail("which failed, standard error:\n", result.stderr)
        file = result.stdout
    if not file:
        fail("program %r not found" % program)
    build = _BUILD_TEMPLATE.format(
        bzl_library = repr(str(Label("@bazel_skylib//:bzl_library.bzl"))),
        visibility = repr([str(v) for v in ctx.attr.visibility]),
    )
    ctx.file("BUILD.bazel", build, executable = False)
    bzl = _BZL_TEMPLATE.format(file = repr(str(file)))
    ctx.file("file.bzl", bzl, executable = False)

local_binary = repository_rule(
    attrs = {"program": attr.string(mandatory = True)},
    local = True,
    configure = True,
    implementation = _local_binary_impl,
)

_BUILD_TEMPLATE = '''# Generated file; do not edit.

load({bzl_library}, "bzl_library")

bzl_library(
    name = "file",
    srcs = ["file.bzl"],
    visibility = {visibility}
)
'''

_BZL_TEMPLATE = """# Generated file; do not edit.

visibility("public")

FILE = {file}
"""
