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
versions.check("5.4.0")

load("@rules_python//python:repositories.bzl", "py_repositories", "python_register_toolchains")

py_repositories()

python_register_toolchains(
    name = "hermetic_python",
    python_version = "3.10",
)

load("@rules_python//python:pip.bzl", "pip_parse")
load("@hermetic_python//:defs.bzl", "interpreter")

pip_parse(
    name = "pip_deps",
    # Work around https://github.com/ionelmc/python-lazy-object-proxy/issues/70.
    extra_pip_args = ["--use-deprecated=legacy-resolver"],
    python_interpreter_target = interpreter,
    requirements_darwin = "@//dev:macos-requirements.txt",
    requirements_linux = "@//dev:linux-requirements.txt",
    requirements_lock = None,
    requirements_windows = "@//dev:windows-requirements.txt",
)

load("@pip_deps//:requirements.bzl", "install_deps")

install_deps()

load("@rules_license//:deps.bzl", "rules_license_dependencies")

# This must come after the rules_python repository because rules_license
# otherwise adds an ancient version of rules_python.
rules_license_dependencies()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "3fd8fec4ddec3c670bd810904e2e33170bedfe12f90adf943508184be458c8bb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
        "https://github.com/bazelbuild/stardoc/releases/download/0.5.3/stardoc-0.5.3.tar.gz",
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
    sha256 = "24564e3b712d3eb30ac9a85d92f7d720f60cc0173730ac166f27dda7fed76cb2",
    strip_prefix = "googletest-release-1.12.1/",
    urls = [
        "https://github.com/google/googletest/archive/refs/tags/release-1.12.1.zip",  # 2022-06-27
    ],
)

http_archive(
    name = "junit_xsd",
    build_file = "@//:junit_xsd.BUILD",
    sha256 = "ba809d0fedfb392cc604ad38aff7db7d750b77eaf5fed977a51360fa4a6dffdf",
    strip_prefix = "JUnit-Schema-1.0.0/",
    urls = [
        "https://github.com/windyroad/JUnit-Schema/archive/refs/tags/1.0.0.tar.gz",  # 2022-04-09
    ],
)

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "6b65cb7917b4d1709f9410ffe00ecf3e160edf674b78c54a894471320862184f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.39.0/rules_go-v0.39.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.39.0/rules_go-v0.39.0.zip",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(
    nogo = "@//dev:nogo",
    version = "1.20.2",
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

load("@upb//bazel:workspace_deps.bzl", "upb_deps")

upb_deps()

http_archive(
    name = "io_abseil_py",
    sha256 = "480faf554f57e1f1e82700d811fd9ec3658cfd5bd43ed644cf7243a82343734e",
    strip_prefix = "abseil-py-1.4.0/",
    urls = [
        "https://github.com/abseil/abseil-py/archive/refs/tags/v1.4.0.zip",  # 2023-01-11
    ],
)

http_archive(
    name = "hedron_compile_commands",
    sha256 = "22979f20e569219ce80031a0ee051230826390a8d9ccb587529a33003ec1e259",
    strip_prefix = "bazel-compile-commands-extractor-b33a4b05c2287372c8e932c55ff4d3a37e6761ed/",
    urls = [
        "https://github.com/hedronvision/bazel-compile-commands-extractor/archive/b33a4b05c2287372c8e932c55ff4d3a37e6761ed.zip",  # 2023-04-16
    ],
)

load("@hedron_compile_commands//:workspace_setup.bzl", "hedron_compile_commands_setup")

hedron_compile_commands_setup()

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "e3bb0dc8b0274ea1aca75f1f8c0c835adbe589708ea89bf698069d0790701ea3",
    strip_prefix = "buildtools-5.1.0/",
    urls = [
        "https://github.com/bazelbuild/buildtools/archive/refs/tags/5.1.0.tar.gz",  # 2022-04-13
    ],
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "ecba0f04f96b4960a5b250c8e8eeec42281035970aa8852dda73098274d14a1d",
    urls = [
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.29.0/bazel-gazelle-v0.29.0.tar.gz",  # 2023-01-14
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
