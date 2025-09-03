# Copyright 2021-2025 Google LLC
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

"""Defines the `check_python` helper function."""

load("@bazel_skylib//lib:paths.bzl", "paths")

visibility("private")

def check_python(
        ctx,
        *,
        info,
        stem,
        program,
        program_args = None,
        additional_inputs = [],
        mnemonic,
        progress_message):
    """Run Pylint on Python source files.

    Args:
      ctx: the rule context
      info: a PyInfo object
      stem: base name of files that will be generated
      program: name of the program to run, must be "pylint"
      program_args: additional arguments for the program, either an Args object
          or None
      additional_inputs: list of additional input files for the program,
          e.g. .pylintrc
      mnemonic: mnemonic for the action, as for ctx.actions.run
      progress_message: progress message for the action, as for ctx.actions.run

    Returns:
      a dummy file that should be added to an optional output group
    """
    output_file = ctx.actions.declare_file(stem + ".stamp")
    args = ctx.actions.args()
    args.add(output_file, format = "--out=%s")
    args.add_all(
        info.transitive_sources,
        format_each = "--src=%s",
        map_each = _source,
        uniquify = True,
    )
    roots = ["", ctx.bin_dir.path]
    args.add_all(
        info.imports,
        map_each = lambda i: [paths.join(r, "external", i) for r in roots],
        format_each = "--import=%s",
        uniquify = True,
        expand_directories = False,
        allow_closure = True,
    )
    args.add_all(
        info.transitive_sources,
        map_each = _import,
        format_each = "--import=%s",
        uniquify = True,
        expand_directories = False,
    )
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = additional_inputs,
            transitive = [info.transitive_sources],
        ),
        executable = ctx.executable._check,
        arguments = [args, program, program_args or ctx.actions.args()],
        mnemonic = mnemonic,
        progress_message = progress_message,
        toolchain = None,
    )
    return output_file

def _source(file):
    # Don’t attempt to check generated protocol buffer files.
    return None if file.owner.workspace_name or file.basename.endswith("_pb2.py") else file.path

def _import(file):
    return paths.join(".", file.root.path, file.owner.workspace_root)
