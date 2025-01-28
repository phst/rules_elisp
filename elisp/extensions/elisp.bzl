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

"""Defines the `elisp` module extension."""

load("@bazel_skylib//lib:modules.bzl", "modules")

visibility("public")

_http_archive = tag_class(
    doc = """Downloads an archive file over HTTP and makes its contents
available as an `elisp_library`.  This tag class is very similar to
[`http_archive`](https://bazel.build/rules/lib/repo/http#http_archive),
except that it always generates a BUILD file containing a single `elisp_library`
rule in the root package for all Emacs Lisp files in the archive.
Test files (`…-test.el`, `…-tests.el`) and package metadata files (`…-pkg.el`)
are excluded.
The `elisp_library` rule is named `library` by default, unless overridden
by the `target_name` attribute.""",
    attrs = {
        "name": attr.string(
            doc = """Name of the repository to generate.""",
            mandatory = True,
        ),
        "urls": attr.string_list(
            doc = """List of archive URLs to try.
See the [corresponding attribute for
`http_archive`](https://bazel.build/rules/lib/repo/http#http_archive-urls).""",
            mandatory = True,
            allow_empty = False,
        ),
        "integrity": attr.string(
            doc = """Expected checksum of the archive file in [Subresource
Integrity](https://www.w3.org/TR/SRI/) format.
See the [corresponding attribute for
`http_archive`](https://bazel.build/rules/lib/repo/http#http_archive-integrity).""",
            mandatory = True,
        ),
        "strip_prefix": attr.string(
            doc = """Directory prefix to strip from the archive contents.
See the [corresponding attribute for
`http_archive`](https://bazel.build/rules/lib/repo/http#http_archive-strip_prefix).""",
        ),
        "target_name": attr.string(
            doc = """Name of the `elisp_library` target to generate.""",
            default = "library",
        ),
        "exclude": attr.string_list(
            doc = """Glob patterns of additional files to exclude from
the library.""",
        ),
    },
)

def _elisp_http_archive_impl(ctx):
    """Implementation of the `_elisp_http_archive` repository rule."""
    ctx.download_and_extract(
        url = ctx.attr.urls,
        integrity = ctx.attr.integrity or fail("missing archive checksum"),
        stripPrefix = ctx.attr.strip_prefix,
    )
    ctx.template(
        "BUILD.bazel",
        Label("//elisp/private/tools:external.BUILD.template"),
        {
            '"[elisp_library.bzl]"': repr(str(Label("//elisp:elisp_library.bzl"))),
            '"[target_name]"': repr(ctx.attr.target_name),
            "[[exclude]]": repr(ctx.attr.exclude),
        },
        executable = False,
    )

_elisp_http_archive = repository_rule(
    attrs = {
        "urls": attr.string_list(mandatory = True, allow_empty = False),
        "integrity": attr.string(mandatory = True),
        "strip_prefix": attr.string(mandatory = True),
        "target_name": attr.string(mandatory = True),
        "exclude": attr.string_list(mandatory = True),
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
    return modules.use_all_repos(ctx)

elisp = module_extension(
    doc = """Module extension for Emacs Lisp.""",
    tag_classes = {
        "http_archive": _http_archive,
    },
    implementation = _elisp_impl,
)
