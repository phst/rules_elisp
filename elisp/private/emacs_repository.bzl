# Copyright 2023, 2024, 2025 Google LLC
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

"""Defines the `emacs_repository` repository rule."""

visibility("private")

def _emacs_repository_impl(ctx):
    output = ctx.attr.output
    ctx.download(
        integrity = ctx.attr.integrity or fail("archive integrity missing"),
        url = ctx.attr.urls or fail("archive URLs missing"),
        output = output,
    )
    ctx.template(
        "BUILD.bazel",
        Label("//elisp/private/tools:emacs.BUILD.template"),
        {
            '"[elisp_emacs_binary.bzl]"': repr(str(Label("//elisp/toolchains:elisp_emacs_binary.bzl"))),
            '"[cc_library.bzl]"': repr(str(Label("@rules_cc//cc:cc_library.bzl"))),
            '"[emacs_pkg]"': repr(str(Label("//emacs:__pkg__"))),
            '"[gazelle_pkg]"': repr(str(Label("//gazelle:__pkg__"))),
            '"[src]"': repr(output),
            '"[strip_prefix]"': repr(ctx.attr.strip_prefix),
            '"[mode]"': repr(ctx.attr.mode),
            "[[compatible_with]]": repr([str(label) for label in ctx.attr.target_compatible_with]),
        },
        executable = False,
    )

emacs_repository = repository_rule(
    attrs = {
        "urls": attr.string_list(mandatory = True, allow_empty = False),
        "integrity": attr.string(mandatory = True),
        "output": attr.string(mandatory = True),
        "strip_prefix": attr.string(),
        "mode": attr.string(mandatory = True, values = ["source", "release"]),
        "target_compatible_with": attr.label_list(),
    },
    implementation = _emacs_repository_impl,
)
