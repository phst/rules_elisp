# Copyright 2022 Google LLC
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

"""Helper definitions for documentation generation."""

load("@bazel_skylib//lib:paths.bzl", "paths")

def _merged_manual_impl(ctx):
    orgs = []
    for bin in ctx.files.includes:
        org = ctx.actions.declare_file(paths.replace_extension(bin.basename, ".org"))
        ctx.actions.run(
            outputs = [org],
            inputs = [bin],
            executable = ctx.executable._org,
            arguments = ["--", bin.path, org.path],
            mnemonic = "GenOrg",
            progress_message = "Generating Org file %{output}",
        )
        orgs.append(org)
    ctx.actions.run(
        outputs = [ctx.outputs.out],
        inputs = [ctx.file.main] + orgs,
        executable = ctx.executable._merge,
        arguments = [ctx.outputs.out.path, ctx.file.main.path] + [o.path for o in orgs],
        mnemonic = "MergeManual",
        progress_message = "Generating merged manual %{output}",
    )

merged_manual = rule(
    attrs = {
        "main": attr.label(
            allow_single_file = [".org"],
            mandatory = True,
        ),
        "includes": attr.label_list(
            allow_files = [".bin"],
            mandatory = True,
            allow_empty = False,
        ),
        "out": attr.output(mandatory = True),
        "_org": attr.label(
            executable = True,
            cfg = "exec",
            default = "//docs:org",
        ),
        "_merge": attr.label(
            executable = True,
            cfg = "exec",
            default = "//docs:merge",
        ),
    },
    implementation = _merged_manual_impl,
)
