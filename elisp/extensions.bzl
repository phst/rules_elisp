# Copyright 2023, 2024 Google LLC
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

"""Module extensions for Emacs Lisp."""

load("//private:repositories.bzl", "HTTP_ARCHIVE_ATTRS", "HTTP_ARCHIVE_DOC")

visibility("public")

_http_archive = tag_class(
    doc = HTTP_ARCHIVE_DOC.format(kind = "tag class"),
    attrs = HTTP_ARCHIVE_ATTRS | {
        "name": attr.string(
            doc = """Name of the repository to generate.""",
            mandatory = True,
        ),
    },
)

def _elisp_http_archive_impl(ctx):
    """Implementation of the `elisp_http_archive` repository rule."""
    ctx.download_and_extract(
        url = ctx.attr.urls,
        integrity = ctx.attr.integrity or fail("missing archive checksum"),
        stripPrefix = ctx.attr.strip_prefix,
    )
    ctx.template(
        "BUILD.bazel",
        Label("//elisp:BUILD.template"),
        {
            '"[defs_bzl]"': repr(str(ctx.attr._defs_bzl)),
            '"[target_name]"': repr(ctx.attr.target_name),
            "[[exclude]]": repr(ctx.attr.exclude),
        },
        executable = False,
    )

_elisp_http_archive = repository_rule(
    doc = HTTP_ARCHIVE_DOC.format(kind = "repository rule"),
    attrs = HTTP_ARCHIVE_ATTRS | {
        "_defs_bzl": attr.label(default = Label("//elisp:defs.bzl")),
    },
    implementation = _elisp_http_archive_impl,
)

def _elisp_impl(ctx):
    """Implementation of the `elisp` module extension."""
    for module in ctx.modules:
        for arch in module.tags.http_archive:
            _elisp_http_archive(
                name = arch.name,
                urls = arch.urls,
                integrity = arch.integrity,
                strip_prefix = arch.strip_prefix,
                target_name = arch.target_name,
                exclude = arch.exclude,
            )

elisp = module_extension(
    doc = """Module extension for Emacs Lisp.""",
    tag_classes = {
        "http_archive": _http_archive,
    },
    implementation = _elisp_impl,
)
