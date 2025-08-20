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

"""Defines the `pytype` aspect."""

load("@rules_python//python:py_info.bzl", "PyInfo")
load(":check_python.bzl", "check_python")

visibility("private")

def _pytype_impl(target, ctx):
    tags = ctx.rule.attr.tags

    # TODO: Require PyInfo provider using required_providers, see below.
    if "no-pytype" in tags or PyInfo not in target:
        return []
    info = target[PyInfo]
    stem = "_{}.pytype".format(target.label.name)
    output_file = check_python(
        ctx,
        info = info,
        stem = stem,
        program = "pytype",
        mnemonic = "Pytype",
        progress_message = "Checking Python types in %{label}",
    )
    return [
        OutputGroupInfo(_validation = depset([output_file])),
    ]

pytype = aspect(
    implementation = _pytype_impl,
    attrs = {
        "_check": attr.label(
            default = Label("//dev:check_python"),
            executable = True,
            cfg = "exec",
        ),
    },
    # The Python rules don’t advertise the PyInfo provider, so we can’t use
    # required_providers here.
    # TODO: File bug against rules_python.
    # required_providers = [PyInfo],
)
