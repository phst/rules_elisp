# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Defines the internal `bootstrap` rule."""

load(":run_emacs.bzl", "run_emacs")

visibility(["//elisp/private/tools"])

def _bootstrap_impl(ctx):
    src = ctx.file.src
    out = ctx.outputs.out
    compile = ctx.file._compile
    args = ctx.actions.args()
    args.add(compile, format = "--load=%s")
    args.add("1")  # fatal warnings
    args.add(src.owner.workspace_name)
    args.add(src)
    args.add(out)
    run_emacs(
        ctx,
        arguments = [args],
        inputs = depset([src, compile], order = "preorder"),
        outputs = [out],
        tags = ctx.attr.tags,
        mnemonic = "ElispCompile",
        progress_message = "Compiling %{input}",
        manifest_basename = out.basename,
        manifest_sibling = out,
    )

bootstrap = rule(
    implementation = _bootstrap_impl,
    # @unsorted-dict-items
    attrs = {
        "src": attr.label(mandatory = True, allow_single_file = [".el"]),
        "out": attr.output(mandatory = True),
        "_compile": attr.label(
            allow_single_file = [".el"],
            default = Label("//elisp/private/tools:compile.el"),
        ),
    },
    doc = "Primitive version of `elisp_library` used for bootstrapping",
    toolchains = [Label("//elisp:toolchain_type")],
)
