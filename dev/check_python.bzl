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

"""Defines the `check_python` helper function."""

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
    """Run Pylint or Pytype on Python source files.

    Args:
      ctx: the rule context
      info: a PyInfo object
      stem: base name of files that will be generated
      program: name of the program to run, either "pylint" or "pytype"
      program_args: additional arguments for the program, either an Args object
          or None
      additional_inputs: list of additional input files for the program,
          e.g. .pylintrc
      mnemonic: mnemonic for the action, as for ctx.actions.run
      progress_message: progress message for the action, as for ctx.actions.run

    Returns:
      a dummy file that should be added to an optional output group
    """
    params_file = ctx.actions.declare_file(stem + ".json")
    output_file = ctx.actions.declare_file(stem + ".stamp")
    params = struct(
        srcs = [
            struct(
                rel = file.short_path,
                src = file.path,
                ext = bool(file.owner.workspace_name),
            )
            for file in info.transitive_sources.to_list()
        ],
    )
    ctx.actions.write(params_file, json.encode(params))
    args = ctx.actions.args()
    args.add(output_file, format = "--out=%s")
    args.add(params_file, format = "--params=%s")
    args.add_all(
        info.imports,
        format_each = "--import=%s",
        uniquify = True,
        expand_directories = False,
    )
    args.add_all(
        info.transitive_sources,
        map_each = _repository_name,
        format_each = "--import=%s",
        uniquify = True,
        expand_directories = False,
    )
    args.add(ctx.workspace_name, format = "--import=%s")
    args.add(ctx.workspace_name, format = "--workspace-name=%s")
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = [params_file] + additional_inputs,
            transitive = [info.transitive_sources],
        ),
        executable = ctx.executable._check,
        arguments = [args, program, program_args or ctx.actions.args()],
        mnemonic = mnemonic,
        progress_message = progress_message,
        toolchain = None,
    )
    return output_file

def _repository_name(file):
    # Skip empty string for main repository.
    return file.owner.workspace_name or None
