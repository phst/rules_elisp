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

"""Contains repository functions to use Emacs Lisp rules."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("//private:repositories.bzl", "HTTP_ARCHIVE_ATTRS", "HTTP_ARCHIVE_DOC", "non_module_deps")

visibility("public")

def rules_elisp_dependencies():
    """Installs necessary dependencies for Emacs Lisp rules.

    Call this function in your `WORKSPACE` file.
    """
    maybe(
        http_archive,
        name = "platforms",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.10/platforms-0.0.10.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.10/platforms-0.0.10.tar.gz",
        ],
        sha256 = "218efe8ee736d26a3572663b374a253c012b716d8af0c07e842e82f238a0a7ee",
    )
    maybe(
        http_archive,
        name = "bazel_features",
        sha256 = "5d7e4eb0bb17aee392143cd667b67d9044c270a9345776a5e5a3cccbc44aa4b3",
        strip_prefix = "bazel_features-1.13.0",
        url = "https://github.com/bazel-contrib/bazel_features/releases/download/v1.13.0/bazel_features-v1.13.0.tar.gz",
    )
    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "d00f1389ee20b60018e92644e0948e16e350a7707219e7a390fb0a99b6ec9262",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.7.0/bazel-skylib-1.7.0.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.7.0/bazel-skylib-1.7.0.tar.gz",
        ],
    )
    maybe(
        http_archive,
        name = "rules_license",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_license/releases/download/0.0.8/rules_license-0.0.8.tar.gz",
            "https://github.com/bazelbuild/rules_license/releases/download/0.0.8/rules_license-0.0.8.tar.gz",
        ],
        sha256 = "241b06f3097fd186ff468832150d6cc142247dc42a32aaefb56d0099895fd229",
    )
    maybe(
        http_archive,
        name = "rules_proto",
        sha256 = "303e86e722a520f6f326a50b41cfc16b98fe6d1955ce46642a5b7a67c11c0f5d",
        strip_prefix = "rules_proto-6.0.0",
        url = "https://github.com/bazelbuild/rules_proto/releases/download/6.0.0/rules_proto-6.0.0.tar.gz",
    )
    maybe(
        http_archive,
        name = "rules_python",
        sha256 = "5bcfa3852444d084b1d3262714cec151b797648d4d444ea9895c7c7ed79cd715",
        strip_prefix = "rules_python-0.33.1",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.33.1/rules_python-0.33.1.tar.gz",
    )
    maybe(
        http_archive,
        name = "com_google_absl",
        sha256 = "733726b8c3a6d39a4120d7e45ea8b41a434cdacde401cba500f14236c49b39dc",
        strip_prefix = "abseil-cpp-20240116.2/",
        urls = [
            "https://github.com/abseil/abseil-cpp/releases/download/20240116.2/abseil-cpp-20240116.2.tar.gz",  # 2024-04-08
        ],
    )
    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "7d7f2ddccc37e3c1c5dfe65ad69d99923d8fe84beac68ed9cdec489909c4d8d3",
        strip_prefix = "protobuf-27.1/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/releases/download/v27.1/protobuf-27.1.zip",  # 2024-06-05
        ],
    )
    non_module_deps()

# buildifier: disable=unnamed-macro
def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("@phst_rules_elisp//elisp:hermetic_toolchain")

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

elisp_http_archive = repository_rule(
    doc = HTTP_ARCHIVE_DOC.format(kind = "repository rule"),
    attrs = HTTP_ARCHIVE_ATTRS | {
        "_defs_bzl": attr.label(default = Label("//elisp:defs.bzl")),
    },
    implementation = _elisp_http_archive_impl,
)
