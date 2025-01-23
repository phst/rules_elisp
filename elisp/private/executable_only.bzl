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

"""Defines the internal `executable_only` rule."""

def _executable_only_impl(ctx):
    info = ctx.attr.src[DefaultInfo]
    files_to_run = info.files_to_run or fail("missing files_to_run")
    executable = files_to_run.executable or fail("missing executable")
    return DefaultInfo(
        files = depset([executable]),
        runfiles = info.default_runfiles,
    )

executable_only = rule(
    implementation = _executable_only_impl,
    attrs = {"src": attr.label(mandatory = True)},
    doc = """Strip non-executable output files from `src`.

Use this rule to wrap a `py_binary` target for use with `$(rlocationpath …)`
etc.  This is necessary because `py_binary` also returns the main source file as
additional file to build.
""",
)
