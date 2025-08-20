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

"""Defines the `pylint` aspect."""

load("@rules_python//python:py_info.bzl", "PyInfo")
load(":check_python.bzl", "check_python")

visibility("private")

def _pylint_impl(target, ctx):
    tags = ctx.rule.attr.tags

    # TODO: Require PyInfo provider using required_providers, see below.
    if "no-pylint" in tags or PyInfo not in target:
        return []
    info = target[PyInfo]
    stem = "_{}.pylint".format(target.label.name)
    pylintrc = ctx.file._pylintrc
    args = ctx.actions.args()
    args.add(pylintrc, format = "--pylintrc=%s")
    output_file = check_python(
        ctx,
        info = info,
        stem = stem,
        program = "pylint",
        program_args = args,
        additional_inputs = [pylintrc],
        mnemonic = "Pylint",
        progress_message = "Linting Python target %{label}",
    )
    return [
        OutputGroupInfo(_validation = depset([output_file])),
    ]

pylint = aspect(
    implementation = _pylint_impl,
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
