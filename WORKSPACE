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
versions.check("4.0.0")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_file")

http_archive(
    name = "rules_python",
    sha256 = "a30abdfc7126d497a7698c29c46ea9901c6392d6ed315171a6df5ce433aa4502",
    strip_prefix = "rules_python-0.6.0",
    url = "https://github.com/bazelbuild/rules_python/archive/0.6.0.tar.gz",
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
    sha256 = "c9794dcc8026a30ff67cf7cf91ebe245ca294b20b071845d12c192afe243ad72",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/stardoc/releases/download/0.5.0/stardoc-0.5.0.tar.gz",
        "https://github.com/bazelbuild/stardoc/releases/download/0.5.0/stardoc-0.5.0.tar.gz",
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
    sha256 = "2b1641428dff9018f9e85c0384f03ec6c10660d935b750e3fa1492a281a53b0f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.29.0/rules_go-v0.29.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.29.0/rules_go-v0.29.0.zip",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(
    nogo = "@//:nogo",
    version = "1.17.1",
)

http_archive(
    name = "com_google_protobuf",
    sha256 = "25f1292d4ea6666f460a2a30038eef121e6c3937ae0f61d610611dfb14b0bd32",
    strip_prefix = "protobuf-3.19.1/",
    urls = [
        "https://github.com/protocolbuffers/protobuf/archive/refs/tags/v3.19.1.zip",  # 2021-10-28
    ],
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

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
    sha256 = "97eb2b241da89b50a0a28a45ae4408661ab740c682db550ea611e8b57334ea9e",
    strip_prefix = "buildtools-4.2.4/",
    urls = [
        "https://github.com/bazelbuild/buildtools/archive/refs/tags/4.2.4.zip",  # 2021-11-29
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
