# Copyright 2020, 2021, 2022 Google LLC
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

workspace(name = "phst_rules_elisp")

load(
    "//elisp:repositories.bzl",
    "rules_elisp_dependencies",
    "rules_elisp_toolchains",
)

rules_elisp_dependencies()

rules_elisp_toolchains()

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

load("@bazel_skylib//lib:versions.bzl", "versions")

# Note that the versions library only works within a WORKSPACE file, see
# https://github.com/bazelbuild/bazel/issues/8305.
versions.check("4.2.1")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

http_archive(
    name = "rules_python",
    sha256 = "15f84594af9da06750ceb878abbf129241421e3abbd6e36893041188db67f2fb",
    strip_prefix = "rules_python-0.7.0",
    url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.7.0.tar.gz",
)

load("@//:internal.bzl", "requirements_txt")

requirements_txt(name = "requirements_txt")

load("@rules_python//python:pip.bzl", "pip_parse")

pip_parse(
    name = "pip_deps",
    requirements_lock = "@requirements_txt//:requirements.txt",
)

load("@pip_deps//:requirements.bzl", "install_deps")

install_deps()

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "1e19581f8b83bc5ea8aa3959d8900f120eb1411d0e6abff6f510fcd60a5148db",
    strip_prefix = "stardoc-cdd19379490c681563b38ef86299f039bd368ce0/",
    urls = [
        "https://github.com/bazelbuild/stardoc/archive/cdd19379490c681563b38ef86299f039bd368ce0.zip",  # 2022-02-16
    ],
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

stardoc_repositories()

local_repository(
    name = "example",
    path = "examples/ext",
)

http_archive(
    name = "com_google_googletest",
    sha256 = "353571c2440176ded91c2de6d6cd88ddd41401d14692ec1f99e35d013feda55a",
    strip_prefix = "googletest-release-1.11.0/",
    urls = [
        "https://github.com/google/googletest/archive/refs/tags/release-1.11.0.zip",  # 2021-06-11
    ],
)

http_file(
    name = "junit_xsd",
    downloaded_file_path = "JUnit.xsd",
    sha256 = "cfc8bc26da1794da8c3f4c4c4de9d24a671b232076d4e61d92fa72834e28230e",
    urls = ["https://raw.githubusercontent.com/windyroad/JUnit-Schema/d6daa414c448da22b810c8562f9d6fca086983ba/JUnit.xsd"],
)

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "d6b2513456fe2229811da7eb67a444be7785f5323c6708b38d851d2b51e54d83",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.30.0/rules_go-v0.30.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.30.0/rules_go-v0.30.0.zip",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(
    nogo = "@//:nogo",
    version = "1.17.6",
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "9ceef0daf7e8be16cd99ac759271eb08021b53b1c7b6edd399953a76390234cd",
    strip_prefix = "protobuf-3.19.2/",
    urls = [
        "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v3.19.2.zip",  # 2022-01-05
    ],
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

http_archive(
    name = "upb",
    sha256 = "4778f3b460a1566c90a613efce2ecf0a402d5f8bb8c3724df59cd6b1818b5e6c",
    strip_prefix = "upb-721b077724bed8b4146f3235a95be4071f38b7a0/",
    urls = [
        "https://github.com/protocolbuffers/upb/archive/721b077724bed8b4146f3235a95be4071f38b7a0.zip",  # 2022-03-09
    ],
)

load("@upb//bazel:workspace_deps.bzl", "upb_deps")

upb_deps()

http_archive(
    name = "com_grail_bazel_compdb",
    build_file = "@//:compdb.BUILD",
    sha256 = "32483ad5aef7496bf338454d851fb63f7a7c72c6b62c40fd74af4f5a5c3749a4",
    strip_prefix = "bazel-compilation-database-0.5.2/",
    urls = [
        "https://github.com/grailbio/bazel-compilation-database/archive/refs/tags/0.5.2.zip",  # 2021-09-10
    ],
)

load("@com_grail_bazel_compdb//:deps.bzl", "bazel_compdb_deps")

bazel_compdb_deps()

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "518b2ce90b1f8ad7c9a319ca84fd7de9a0979dd91e6d21648906ea68faa4f37a",
    strip_prefix = "buildtools-5.0.1/",
    urls = [
        "https://github.com/bazelbuild/buildtools/archive/refs/tags/5.0.1.zip",  # 2022-02-11
    ],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "de69a09dc70417580aabf20a28619bb3ef60d038470c7cf8442fafcf627c21cb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.24.0/bazel-gazelle-v0.24.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.24.0/bazel-gazelle-v0.24.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

gazelle_dependencies()

go_repository(
    name = "com_github_google_go_cmp",
    importpath = "github.com/google/go-cmp",
    sum = "h1:BKbKCqvP6I+rmFHt06ZmyQtvB8xAkWdhFyr0ZUNZcxQ=",
    version = "v0.5.6",
)

go_repository(
    name = "com_github_phst_runfiles",
    importpath = "github.com/phst/runfiles",
    sum = "h1:B0CrzpfUkDAR73lrWPhLob7zvD5e5t70NVoJcig7JZ8=",
    version = "v0.0.0-20220103114429-bafe238f62cc",
)
