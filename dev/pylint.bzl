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

"""Defines the `pylint` aspect."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@rules_python//python:py_info.bzl", "PyInfo")

visibility("private")

def _pylint_impl(target, ctx):
    tags = ctx.rule.attr.tags

    # TODO: Require PyInfo provider using required_providers, see below.
    if "no-pylint" in tags or PyInfo not in target:
        return []
    info = target[PyInfo]
    stem = "_{}.pylint".format(target.label.name)
    output_file = ctx.actions.declare_file(stem + ".stamp")
    pylintrc = ctx.file._pylintrc
    args = ctx.actions.args()
    args.add(output_file, format = "--out=%s")
    args.add_all(
        info.direct_original_sources,
        format_each = "--src=%s",
        uniquify = True,
    )
    args.add(pylintrc, format = "--pylintrc=%s")
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
            direct = [pylintrc],
            transitive = [info.transitive_sources],
        ),
        executable = ctx.executable._run,
        arguments = [args],
        mnemonic = "Pylint",
        progress_message = "Linting Python target %{label}",
        toolchain = None,
    )
    return [
        OutputGroupInfo(_validation = depset([output_file])),
    ]

pylint = aspect(
    implementation = _pylint_impl,
    # @unsorted-dict-items
    attrs = {
        "_run": attr.label(
            default = Label("//dev:run_pylint"),
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

def _import(file):
    return paths.join(".", file.root.path, file.owner.workspace_root)
