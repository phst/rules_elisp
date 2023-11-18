# Copyright 2020, 2021, 2022, 2023 Google LLC
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

"""Contains workspace functions to use Emacs Lisp rules."""

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("//private:repositories.bzl", "non_module_deps")

def rules_elisp_dependencies():
    """Installs necessary dependencies for Emacs Lisp rules.

    Call this function in your `WORKSPACE` file.
    """
    maybe(
        http_archive,
        name = "platforms",
        sha256 = "3a561c99e7bdbe9173aa653fd579fe849f1d8d67395780ab4770b1f381431d51",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/platforms/releases/download/0.0.7/platforms-0.0.7.tar.gz",
            "https://github.com/bazelbuild/platforms/releases/download/0.0.7/platforms-0.0.7.tar.gz",
        ],
    )
    maybe(
        http_archive,
        name = "bazel_skylib",
        sha256 = "66ffd9315665bfaafc96b52278f57c7e2dd09f5ede279ea6d39b2be471e7e3aa",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.4.2/bazel-skylib-1.4.2.tar.gz",
            "https://github.com/bazelbuild/bazel-skylib/releases/download/1.4.2/bazel-skylib-1.4.2.tar.gz",
        ],
    )
    maybe(
        http_archive,
        name = "rules_license",
        sha256 = "4531deccb913639c30e5c7512a054d5d875698daeb75d8cf90f284375fe7c360",
        urls = [
            "https://mirror.bazel.build/github.com/bazelbuild/rules_license/releases/download/0.0.7/rules_license-0.0.7.tar.gz",
            "https://github.com/bazelbuild/rules_license/releases/download/0.0.7/rules_license-0.0.7.tar.gz",
        ],
    )
    maybe(
        http_archive,
        name = "rules_python",
        sha256 = "9d04041ac92a0985e344235f5d946f71ac543f1b1565f2cdbc9a2aaee8adf55b",
        strip_prefix = "rules_python-0.26.0",
        url = "https://github.com/bazelbuild/rules_python/releases/download/0.26.0/rules_python-0.26.0.tar.gz",
    )
    maybe(
        http_archive,
        name = "com_google_absl",
        sha256 = "61d0af0262a0131bb8917fcb883e5e831ee5ad1535433f2f13f85906d1607f81",
        strip_prefix = "abseil-cpp-20230125.1/",
        urls = [
            "https://github.com/abseil/abseil-cpp/archive/refs/tags/20230125.1.zip",  # 2023-02-18
        ],
    )
    maybe(
        http_archive,
        name = "com_google_protobuf",
        sha256 = "e13ca6c2f1522924b8482f3b3a482427d0589ff8ea251088f7e39f4713236053",
        strip_prefix = "protobuf-21.7/",
        urls = [
            "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v21.7.zip",  # 2022-09-29
        ],
    )
    maybe(
        http_archive,
        name = "upb",
        patches = ["@//:upb.patch"],
        sha256 = "0d6af8c8c00b3d733721f8d890ef43dd40f537c2e815b529085c1a6c30a21084",
        strip_prefix = "upb-a5477045acaa34586420942098f5fecd3570f577/",
        urls = [
            "https://github.com/protocolbuffers/upb/archive/a5477045acaa34586420942098f5fecd3570f577.zip",  # 2022-09-23
        ],
    )
    non_module_deps()

# buildifier: disable=unnamed-macro
def rules_elisp_toolchains():
    """Registers the default toolchains for Emacs Lisp."""
    native.register_toolchains("@phst_rules_elisp//elisp:hermetic_toolchain")
