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

"""Defines the internal `merged_manual` rule."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_skylib//lib:sets.bzl", "sets")

visibility("private")

def _merged_manual_impl(ctx):
    orgs = []
    roots = sets.make()
    for bin in ctx.files.includes:
        org = ctx.actions.declare_file(paths.replace_extension(bin.basename, ".org"), sibling = bin)
        ctx.actions.run(
            outputs = [org],
            inputs = [bin],
            executable = ctx.executable._generate,
            arguments = [ctx.actions.args().add("--").add(bin).add(org)],
            mnemonic = "GenOrg",
            progress_message = "Generating Org file %{output}",
            toolchain = None,
        )
        orgs.append(org)
        sets.insert(roots, org.root)

    if sets.length(roots) != 1:
        fail("multiple roots: %s", sets.str(roots))
    (root,) = sets.to_list(roots)
    include_dir = paths.join(root.path, ctx.file.main.owner.package)

    args = ctx.actions.args()
    args.add(ctx.outputs.out)
    args.add(ctx.file.main)
    args.add(include_dir)
    ctx.actions.run(
        outputs = [ctx.outputs.out],
        inputs = [ctx.file.main] + orgs,
        executable = ctx.executable._merge,
        arguments = [args],
        mnemonic = "MergeManual",
        progress_message = "Generating merged manual %{output}",
        toolchain = None,
    )

merged_manual = rule(
    attrs = {
        "main": attr.label(
            allow_single_file = [".org"],
            mandatory = True,
        ),
        "includes": attr.label_list(
            allow_files = [".binaryproto"],
            mandatory = True,
            allow_empty = False,
        ),
        "out": attr.output(mandatory = True),
        "_generate": attr.label(
            executable = True,
            cfg = "exec",
            default = Label("//docs:generate"),
        ),
        "_merge": attr.label(
            executable = True,
            cfg = "exec",
            default = Label("//docs:merge"),
        ),
    },
    implementation = _merged_manual_impl,
)
