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

"""Defines the `check_python` aspect."""

load("@rules_python//python:py_info.bzl", "PyInfo")

visibility("private")

def _check_python_impl(target, ctx):
    tags = ctx.rule.attr.tags

    # TODO: Require PyInfo provider using required_providers, see below.
    if "no-python-check" in tags or PyInfo not in target:
        return []
    info = target[PyInfo]
    stem = "_{}.python-check".format(target.label.name)
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
    pylintrc = ctx.file._pylintrc
    args = ctx.actions.args()
    args.add(output_file, format = "--out=%s")
    args.add(pylintrc, format = "--pylintrc=%s")
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
    if "no-pytype" not in tags:
        args.add("--pytype")
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = [params_file, pylintrc],
            transitive = [info.transitive_sources],
        ),
        executable = ctx.executable._check,
        arguments = [args],
        mnemonic = "PythonCheck",
        progress_message = "Performing static analysis of target %{label}",
        toolchain = None,
    )
    return [
        OutputGroupInfo(check_python = depset([output_file])),
    ]

check_python = aspect(
    implementation = _check_python_impl,
    attrs = {
        "_check": attr.label(
            default = Label("//dev:check_python"),
            executable = True,
            cfg = "exec",
        ),
        "_pylintrc": attr.label(
            default = Label("//:.pylintrc"),
            allow_single_file = True,
        ),
    },
    # The Python rules don’t advertise the PyInfo provider, so we can’t use
    # required_providers here.
    # TODO: File bug against rules_python.
    # required_providers = [PyInfo],
)

def _repository_name(file):
    # Skip empty string for main repository.
    return file.owner.workspace_name or None
