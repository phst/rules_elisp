# Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

"""Internal-only repository functions.

These definitions are internal and subject to change without notice."""

visibility(["//", "//elisp"])

def non_module_deps():
    """Installs dependencies that are not available as modules."""
    _emacs(
        version = "28.1",
        integrity = "sha256-KLGz0JkDegiPCkyiUdfnJi6rXqFneqv/psRCaWGtdeE=",
    )
    _emacs(
        version = "28.2",
        integrity = "sha256-7iEYIjPvMjLcl7SGry2G4UBC27ZbvFNd9WLDqFgjJIg=",
    )
    _emacs(
        version = "29.1",
        integrity = "sha256-0viBpcwjHi9aA+hvRYSwQ4+D7ddZignSSiG9jQA+LgE=",
    )
    _emacs(
        version = "29.2",
        integrity = "sha256-fT0kSJiHIL9L9XrXeloIvyLfJhYPkFB6hBuphr4mcNw=",
    )
    _emacs(
        version = "29.3",
        integrity = "sha256-w0wF06zmZu2cf3oPrwcP6jIX/xkQ0ARJm9VFMjPXQqA=",
    )

def _non_module_dev_deps_impl(ctx):
    ctx.download_and_extract(
        sha256 = "ba809d0fedfb392cc604ad38aff7db7d750b77eaf5fed977a51360fa4a6dffdf",
        url = [
            "https://github.com/windyroad/JUnit-Schema/archive/refs/tags/1.0.0.tar.gz",  # 2022-04-09
        ],
        stripPrefix = "JUnit-Schema-1.0.0/",
    )
    ctx.template(
        "BUILD.bazel",
        Label("//:junit_xsd.BUILD"),
        {
            '"[tests_pkg]"': repr(str(Label("//tests:__pkg__"))),
        },
        executable = False,
    )

non_module_dev_deps = repository_rule(
    doc = """Installs development dependencies that are not available as modules.""",
    implementation = _non_module_dev_deps_impl,
)

HTTP_ARCHIVE_DOC = """Downloads an archive file over HTTP and makes its contents
available as an `elisp_library`.  This {kind} is very similar to
[`http_archive`](https://bazel.build/rules/lib/repo/http#http_archive),
except that it always generates a BUILD file containing a single `elisp_library`
rule in the root package for all Emacs Lisp files in the archive.
Test files (`…-test.el`, `…-tests.el`) and package metadata files (`…-pkg.el`)
are excluded.
The `elisp_library` rule is named `library` by default, unless overridden
by the `target_name` attribute."""

HTTP_ARCHIVE_ATTRS = {
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
}

def _emacs(*, version, integrity):
    _emacs_repository(
        name = "gnu_emacs_" + version,
        path = "/emacs/emacs-{}.tar.xz".format(version),
        integrity = integrity,
        strip_prefix = "emacs-{}".format(version),
    )

def _emacs_repository_impl(ctx):
    path = ctx.attr.path
    ctx.download_and_extract(
        integrity = ctx.attr.integrity,
        url = [
            "https://ftpmirror.gnu.org" + path,
            "https://ftp.gnu.org/gnu" + path,
        ],
        stripPrefix = ctx.attr.strip_prefix,
    )
    ctx.template(
        "BUILD.bazel",
        Label("//:emacs.BUILD"),
        {
            '"[emacs_pkg]"': repr(str(Label("//emacs:__pkg__"))),
        },
        executable = False,
    )

_emacs_repository = repository_rule(
    attrs = {
        "path": attr.string(mandatory = True),
        "integrity": attr.string(mandatory = True),
        "strip_prefix": attr.string(),
    },
    implementation = _emacs_repository_impl,
)
