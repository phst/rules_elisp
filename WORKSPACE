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
versions.check("6.3.0")

load("@bazel_features//:deps.bzl", "bazel_features_deps")

bazel_features_deps()

load("@rules_python//python:repositories.bzl", "py_repositories", "python_register_toolchains")

py_repositories()

python_register_toolchains(
    name = "hermetic_python",
    # Attempt to work around nondeterminism on Windows.  See
    # https://github.com/bazelbuild/rules_python/pull/713 and
    # https://github.com/bazelbuild/rules_python/pull/907.
    ignore_root_user_error = True,
    python_version = "3.10",
    register_coverage_tool = True,
)

load("@hermetic_python//:defs.bzl", "interpreter")
load("@rules_python//python:pip.bzl", "pip_parse")

pip_parse(
    name = "pip_deps",
    python_interpreter_target = interpreter,
    requirements_darwin = "//dev:macos-requirements.txt",
    requirements_linux = "//dev:linux-requirements.txt",
    requirements_lock = None,
    requirements_windows = "//dev:windows-requirements.txt",
)

load("@pip_deps//:requirements.bzl", "install_deps")

install_deps()

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies")

rules_proto_dependencies()

load("@rules_proto//proto:toolchains.bzl", "rules_proto_toolchains")

rules_proto_toolchains()

load("@rules_license//:deps.bzl", "rules_license_dependencies")

# This must come after the rules_python repository because rules_license
# otherwise adds an ancient version of rules_python.
rules_license_dependencies()

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_stardoc",
    sha256 = "62bd2e60216b7a6fec3ac79341aa201e0956477e7c8f6ccc286f279ad1d96432",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/stardoc/releases/download/0.6.2/stardoc-0.6.2.tar.gz",
        "https://github.com/bazelbuild/stardoc/releases/download/0.6.2/stardoc-0.6.2.tar.gz",
    ],
)

load("@io_bazel_stardoc//:setup.bzl", "stardoc_repositories")

stardoc_repositories()

load("@rules_jvm_external//:repositories.bzl", "rules_jvm_external_deps")

rules_jvm_external_deps()

load("@rules_jvm_external//:setup.bzl", "rules_jvm_external_setup")

rules_jvm_external_setup()

load("@io_bazel_stardoc//:deps.bzl", "stardoc_external_deps")

stardoc_external_deps()

load("@stardoc_maven//:defs.bzl", stardoc_pinned_maven_install = "pinned_maven_install")

stardoc_pinned_maven_install()

local_repository(
    name = "example",
    path = "examples/ext",
)

load("@example//:repositories.bzl", "example_dependencies")

example_dependencies()

load("//private:repositories.bzl", "non_module_dev_deps")

non_module_dev_deps(name = "phst_rules_elisp_dev_deps")

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "33acc4ae0f70502db4b893c9fc1dd7a9bf998c23e7ff2c4517741d4049a976f8",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.48.0/rules_go-v0.48.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.48.0/rules_go-v0.48.0.zip",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(
    nogo = "@//dev:nogo",
    version = "1.22.0",
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

http_archive(
    name = "io_abseil_py",
    sha256 = "480faf554f57e1f1e82700d811fd9ec3658cfd5bd43ed644cf7243a82343734e",
    strip_prefix = "abseil-py-1.4.0/",
    urls = [
        "https://github.com/abseil/abseil-py/archive/refs/tags/v1.4.0.zip",  # 2023-01-11
    ],
)

http_archive(
    name = "buildifier_prebuilt",
    sha256 = "8ada9d88e51ebf5a1fdff37d75ed41d51f5e677cdbeafb0a22dda54747d6e07e",
    strip_prefix = "buildifier-prebuilt-6.4.0",
    urls = [
        "https://github.com/keith/buildifier-prebuilt/archive/refs/tags/6.4.0.tar.gz",
    ],
)

load("@buildifier_prebuilt//:deps.bzl", "buildifier_prebuilt_deps")

buildifier_prebuilt_deps()

load("@buildifier_prebuilt//:defs.bzl", "buildifier_prebuilt_register_toolchains")

buildifier_prebuilt_register_toolchains()

http_archive(
    name = "bazel_gazelle",
    sha256 = "75df288c4b31c81eb50f51e2e14f4763cb7548daae126817247064637fd9ea62",
    urls = [
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.36.0/bazel-gazelle-v0.36.0.tar.gz",  # 2024-04-03
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

gazelle_dependencies()

go_repository(
    name = "com_github_bazelbuild_buildtools",
    importpath = "github.com/bazelbuild/buildtools",
    sum = "h1:2Gc2Q6hVR1SJ8bBI9Ybzoggp8u/ED2WkM4MfvEIn9+c=",
    version = "v0.0.0-20231115204819-d4c9dccdfbb1",
)

go_repository(
    name = "com_github_google_addlicense",
    importpath = "github.com/google/addlicense",
    sum = "h1:jpVf9qPbU8rz5MxKo7d+RMcNHkqxi4YJi/laauX4aAE=",
    version = "v1.1.1",
)

go_repository(
    name = "com_github_google_go_cmp",
    importpath = "github.com/google/go-cmp",
    sum = "h1:ofyhxvXcZhMsU5ulbFiLKl/XBFqE1GSq7atu8tAmTRI=",
    version = "v0.6.0",
)
