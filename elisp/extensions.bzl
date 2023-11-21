# Copyright 2023 Google LLC
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
load(":repositories.bzl", "elisp_http_archive")

_http_archive = tag_class(
    doc = HTTP_ARCHIVE_DOC.format(kind = "tag class"),
    attrs = dict(
        HTTP_ARCHIVE_ATTRS,
        name = attr.string(
            doc = """Name of the workspace to generate.""",
            mandatory = True,
        ),
    ),
)

def _elisp_impl(module_ctx):
    """Implementation of the `elisp` module extension."""
    for module in module_ctx.modules:
        for arch in module.tags.http_archive:
            elisp_http_archive(
                name = arch.name,
                urls = arch.urls,
                integrity = arch.integrity,
                strip_prefix = arch.strip_prefix,
            )

elisp = module_extension(
    doc = """Module extension for Emacs Lisp.""",
    tag_classes = {
        "http_archive": _http_archive,
    },
    implementation = _elisp_impl,
)
