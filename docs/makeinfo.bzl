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

"""Defines the `makeinfo` rule."""

load("@makeinfo//:file.bzl", "FILE")

visibility("private")

def _makeinfo_impl(ctx):
    out = ctx.outputs.out
    if out.extension not in ["info", "html"]:
        fail("Unknown output file name", out.basename)
    src = ctx.file.src
    args = ctx.actions.args()
    args.add(FILE)
    args.add("--no-split")
    if out.extension == "html":
        args.add("--html")
    args.add(out, format = "--output=%s")
    args.add("--")
    args.add(src)
    ctx.actions.run_shell(
        outputs = [out],
        inputs = [src],
        command = 'exec "$@"',
        arguments = [args],
        mnemonic = "Makeinfo",
        progress_message = "Generating Info manual %{output}",
    )

makeinfo = rule(
    # @unsorted-dict-items
    attrs = {
        "src": attr.label(mandatory = True, allow_single_file = [".texi"]),
        "out": attr.output(mandatory = True),
    },
    implementation = _makeinfo_impl,
)
