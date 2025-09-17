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
    pylint = ctx.executable._pylint
    pylintrc = ctx.file._pylintrc
    args = ctx.actions.args()
    args.add("--persistent=no")
    args.add(pylintrc, format = "--rcfile=%s")

    # Set a fake PYTHONPATH so that Pylint can find imports for the main and
    # external repositories.
    roots = ["", ctx.bin_dir.path]
    args.add_joined(
        info.imports,
        join_with = ", ",
        map_each = lambda i: [repr(paths.join(r, "external", i)) for r in roots],
        format_joined = "--init-hook=import sys; sys.path.extend([%s])",
        uniquify = True,
        expand_directories = False,
        allow_closure = True,
    )
    args.add_joined(
        info.transitive_sources,
        join_with = ", ",
        map_each = _import,
        format_joined = "--init-hook=import sys; sys.path.extend([%s])",
        uniquify = True,
        expand_directories = False,
    )

    args.add(output_file, format = "--output-format=text,text:%s")

    # We’d like to add “--” after the options, but that’s not possible
    # due to https://github.com/PyCQA/pylint/issues/7003.
    args.add_all(info.direct_original_sources, uniquify = True)
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = [pylintrc],
            transitive = [info.transitive_sources],
        ),
        executable = pylint,
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
        "_pylint": attr.label(
            default = Label(":pylint_bin"),
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
    return repr(paths.join(".", file.root.path, file.owner.workspace_root))
