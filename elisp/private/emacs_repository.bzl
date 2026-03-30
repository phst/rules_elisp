# Copyright 2023-2026 Google LLC
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

load("@bazel_tools//tools/build_defs/repo:utils.bzl", "get_auth")

visibility("private")

def _emacs_repository_impl(ctx):
    urls = ctx.attr.urls
    ctx.download_and_extract(
        integrity = ctx.attr.integrity or fail("archive integrity missing"),
        url = urls,
        stripPrefix = ctx.attr.strip_prefix,
        type = ctx.attr.format,
        auth = get_auth(ctx, urls),
    )
    ctx.delete("test")
    ctx.template(
        "BUILD.bazel",
        Label("//elisp/private/tools:emacs.BUILD.template"),
        # @unsorted-dict-items
        {
            '"[elisp_emacs_binary.bzl]"': repr(str(Label("//elisp/toolchains:elisp_emacs_binary.bzl"))),
            '"[cc_library.bzl]"': repr(str(Label("@rules_cc//cc:cc_library.bzl"))),
            '"[emacs_pkg]"': repr(str(Label("//emacs:__pkg__"))),
            '"[gazelle_pkg]"': repr(str(Label("//gazelle/elisp:__pkg__"))),
            '"[type]"': repr(ctx.attr.type),
            "[[compatible_with]]": repr([str(label) for label in ctx.attr.target_compatible_with]),
        },
        executable = False,
    )

emacs_repository = repository_rule(
    # @unsorted-dict-items
    attrs = {
        "urls": attr.string_list(mandatory = True, allow_empty = False),
        "netrc": attr.label(allow_single_file = [".netrc"]),
        "auth_patterns": attr.string_dict(),
        "format": attr.string(mandatory = True, values = ["zip", "tar.gz", "tar.xz"]),
        "integrity": attr.string(mandatory = True),
        "strip_prefix": attr.string(mandatory = True),
        "type": attr.string(mandatory = True, values = ["source", "release"]),
        "target_compatible_with": attr.label_list(),
    },
    implementation = _emacs_repository_impl,
)
