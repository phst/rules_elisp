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
        sha256 = "c2596994cf63513bd44180411a4ac3ae95d32bf59148fcb6087a4642b3ffef11",
        strip_prefix = "bazel_features-1.20.0",
        url = "https://github.com/bazel-contrib/bazel_features/releases/download/v1.20.0/bazel_features-v1.20.0.tar.gz",
    )
    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "bc283cdfcd526a52c3201279cda4bc298652efa898b10b4db0837dc51652756f",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.7.1/bazel-skylib-1.7.1.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.7.1/bazel-skylib-1.7.1.tar.gz",
        ],
    )
    maybe(
        http_archive,
        name = "rules_license",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_license/releases/download/1.0.0/rules_license-1.0.0.tar.gz",
            "https://github.com/bazelbuild/rules_license/releases/download/1.0.0/rules_license-1.0.0.tar.gz",
        ],
        sha256 = "26d4021f6898e23b82ef953078389dd49ac2b5618ac564ade4ef87cced147b38",
    )
    maybe(
        http_archive,
        name = "rules_cc",
        urls = ["https://github.com/bazelbuild/rules_cc/releases/download/0.1.0/rules_cc-0.1.0.tar.gz"],
        sha256 = "4b12149a041ddfb8306a8fd0e904e39d673552ce82e4296e96fac9cbf0780e59",
        strip_prefix = "rules_cc-0.1.0",
    )
    maybe(
        http_archive,
        name = "rules_python",
        sha256 = "bd4797821b72b80b69e3c5ab4ad037e7fd1e6a0a27aebf42424c7ab0ce32e254",
        strip_prefix = "rules_python-0.37.1",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.37.1/rules_python-0.37.1.tar.gz",
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
        sha256 = "10a0d58f39a1a909e95e00e8ba0b5b1dc64d02997f741151953a2b3659f6e78c",
        strip_prefix = "protobuf-29.0/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/releases/download/v29.0/protobuf-29.0.tar.gz",  # 2024-11-27
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
