# Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Defines the `elisp_manual` rule."""

load("//elisp/private:manual.bzl", "MAX_MANUAL_ADDITIONAL_INPUTS")

visibility("public")

def _elisp_manual_impl(ctx):
    """Rule implementation for the “elisp_manual” rule."""
    src = ctx.file.src
    out = ctx.outputs.out
    if out.extension != "texi":
        fail("Output filename {} doesn’t end in “.texi”".format(out.short_path))
    additional_inputs = ctx.files.additional_inputs
    if len(additional_inputs) > MAX_MANUAL_ADDITIONAL_INPUTS:
        fail("Got {} additional input files; at most {} are allowed".format(len(additional_inputs), MAX_MANUAL_ADDITIONAL_INPUTS))
    ctx.actions.run(
        outputs = [out],
        inputs = [src] + additional_inputs,
        executable = ctx.executable._export,
        arguments = [ctx.actions.args().add(out).add(src)],
        mnemonic = "Export",
        progress_message = "Exporting %{input} into Texinfo file",
        toolchain = None,
    )

elisp_manual = rule(
    attrs = {
        "src": attr.label(
            doc = "Org-mode file to use as manual source; must end in `.org`.",
            allow_single_file = [".org"],
            mandatory = True,
        ),
        "out": attr.output(
            doc = "Texinfo manual file to write; must end in `.texi`.",
            mandatory = True,
        ),
        "additional_inputs": attr.label_list(
            doc = "List of additional files made available during export.",
            allow_files = True,
            cfg = "exec",
        ),
        "_export": attr.label(
            allow_single_file = True,
            executable = True,
            cfg = "exec",
            default = Label("//elisp/private/tools:export_org"),
        ),
    },
    doc = """Generates a GNU Texinfo manual from an Org Mode file.
See [GNU Texinfo](info:texinfo), and see [the Org Mode manual](info:org).
Uses Org’s exporting functionality; see [Exporting](info:org#Exporting).
You can then use `texi2any` to generate other document formats from the output
file; see [`texi2any`](<info:texinfo#Generic Translator texi2any>).""",
    implementation = _elisp_manual_impl,
)
