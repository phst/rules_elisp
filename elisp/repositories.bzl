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
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.8/platforms-0.0.8.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.8/platforms-0.0.8.tar.gz",
        ],
        sha256 = "8150406605389ececb6da07cbcb509d5637a3ab9a24bc69b1101531367d89d74",
    )
    maybe(
        http_archive,
        name = "bazel_features",
        sha256 = "22056b03b3f64491244882453022b801438c01d672829d1ac6338876b0fa206a",
        strip_prefix = "bazel_features-1.7.1",
        url = "https://github.com/bazel-contrib/bazel_features/releases/download/v1.7.1/bazel_features-v1.7.1.tar.gz",
    )
    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "cd55a062e763b9349921f0f5db8c3933288dc8ba4f76dd9416aac68acee3cb94",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.5.0/bazel-skylib-1.5.0.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.5.0/bazel-skylib-1.5.0.tar.gz",
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
        sha256 = "dc3fb206a2cb3441b485eb1e423165b231235a1ea9b031b4433cf7bc1fa460dd",
        strip_prefix = "rules_proto-5.3.0-21.7",
        urls = [
            "https://github.com/bazelbuild/rules_proto/archive/refs/tags/5.3.0-21.7.tar.gz",
        ],
    )
    maybe(
        http_archive,
        name = "rules_python",
        sha256 = "c68bdc4fbec25de5b5493b8819cfc877c4ea299c0dcb15c244c5a00208cde311",
        strip_prefix = "rules_python-0.31.0",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.31.0/rules_python-0.31.0.tar.gz",
    )
    maybe(
        http_archive,
        name = "com_google_absl",
        sha256 = "497ebdc3a4885d9209b9bd416e8c3f71e7a1fb8af249f6c2a80b7cbeefcd7e21",
        strip_prefix = "abseil-cpp-20230802.1/",
        urls = [
            "https://github.com/abseil/abseil-cpp/archive/refs/tags/20230802.1.zip",  # 2023-09-18
        ],
    )
    maybe(
        http_archive,
        name = "com_google_protobuf",
        patches = [Label("//:protobuf.patch")],
        sha256 = "9776a431a6fd0730c85c7c083cf1cde7d6774d3f4afdb18cc6f34cbe5687e236",
        strip_prefix = "protobuf-23.1/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/releases/download/v23.1/protobuf-23.1.zip",  # 2023-05-16
        ],
    )
    maybe(
        http_archive,
        name = "upb",
        sha256 = "f2f22ecea7c60f4ef989a91744f2a3ab5bad532b0b14b9f320fe4b37d0aa6b09",
        strip_prefix = "upb-e7430e66d6e51def2a88f0b66fdab62b0d9492c1/",
        urls = [
            "https://github.com/protocolbuffers/upb/archive/e7430e66d6e51def2a88f0b66fdab62b0d9492c1.zip",  # 2023-09-07
        ],
    )
    non_module_deps(name = "phst_rules_elisp_deps")

# buildifier: disable=unnamed-macro
def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("@phst_rules_elisp//elisp:hermetic_toolchain")

def _elisp_http_archive_impl(repository_ctx):
    """Implementation of the `elisp_http_archive` repository rule."""
    repository_ctx.download_and_extract(
        url = repository_ctx.attr.urls,
        integrity = repository_ctx.attr.integrity or fail("missing archive checksum"),
        stripPrefix = repository_ctx.attr.strip_prefix,
    )
    repository_ctx.template(
        "BUILD.bazel",
        Label("//elisp:BUILD.template"),
        {
            '"[defs_bzl]"': repr(str(repository_ctx.attr._defs_bzl)),
            '"[target_name]"': repr(repository_ctx.attr.target_name),
            "[[exclude]]": repr(repository_ctx.attr.exclude),
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
