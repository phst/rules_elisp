# Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

load("@bazel_skylib//:bzl_library.bzl", "bzl_library")
load("//elisp/private:module_config.bzl", "module_config")
load(
    "//private:defs.bzl",
    "LAUNCHER_COPTS",
    "LAUNCHER_DEFINES",
    "LAUNCHER_FEATURES",
    "LAUNCHER_LINKOPTS",
    "PACKAGE_FEATURES",
    "cc_defaults",
)
load(":defs.bzl", "elisp_toolchain")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

toolchain_type(
    name = "toolchain_type",
    visibility = ["//visibility:public"],
)

toolchain(
    name = "hermetic_toolchain",
    toolchain = ":hermetic",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_28.2_toolchain",
    toolchain = "emacs_28.2",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "emacs_29.4_toolchain",
    toolchain = "emacs_29.4",
    toolchain_type = ":toolchain_type",
)

toolchain(
    name = "local_toolchain",
    toolchain = ":local",
    toolchain_type = ":toolchain_type",
)

elisp_toolchain(
    name = "hermetic",
    emacs = "//emacs",
)

elisp_toolchain(
    name = "local",
    emacs = "@local_emacs//:emacs",
    use_default_shell_env = True,
)

elisp_toolchain(
    name = "emacs_28.2",
    emacs = "//emacs:emacs_28.2",
)

elisp_toolchain(
    name = "emacs_29.4",
    emacs = "//emacs:emacs_29.4",
)

bzl_library(
    name = "defs",
    srcs = ["defs.bzl"],
    visibility = ["//visibility:public"],
    deps = [
        ":elisp_binary",
        ":elisp_cc_module",
        ":elisp_library",
        ":elisp_manual",
        ":elisp_test",
        "//elisp/common:elisp_info",
        "//elisp/proto:elisp_proto_library",
        "//elisp/toolchains:elisp_toolchain",
    ],
)

bzl_library(
    name = "elisp_library",
    srcs = ["elisp_library.bzl"],
    visibility = ["//visibility:public"],
    deps = [
        "//elisp/common:elisp_info",
        "//elisp/private:compile",
    ],
)

bzl_library(
    name = "elisp_cc_module",
    srcs = ["elisp_cc_module.bzl"],
    visibility = ["//visibility:public"],
    deps = [
        "//elisp/common:elisp_info",
        "//elisp/private:load_path",
        "//private:defs",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "elisp_binary",
    srcs = ["elisp_binary.bzl"],
    visibility = ["//visibility:public"],
    deps = [
        "//elisp/common:elisp_info",
        "//elisp/private:binary",
        "//elisp/private:compile",
        "//private:defs",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "elisp_test",
    srcs = [
        "elisp_test.bzl",  # keep
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//elisp/common:elisp_info",
        "//elisp/private:binary",
        "//elisp/private:compile",
        "//private:defs",
        "@rules_cc//cc:find_cc_toolchain_bzl",
        "@rules_cc//cc/common",
    ],
)

bzl_library(
    name = "elisp_manual",
    srcs = ["elisp_manual.bzl"],
    visibility = ["//visibility:public"],
    deps = ["//private:defs"],
)

bzl_library(
    name = "extensions",
    srcs = ["extensions.bzl"],
    visibility = ["//visibility:public"],
    deps = ["//elisp/extensions:elisp"],
)

exports_files(
    [
        # keep sorted
        "elisp_binary.bzl",
        "elisp_cc_module.bzl",
        "elisp_library.bzl",
        "elisp_manual.bzl",
        "elisp_test.bzl",
    ],
    visibility = ["//docs:__pkg__"],
)

cc_defaults(
    name = "launcher_defaults",
    copts = LAUNCHER_COPTS,
    defines = LAUNCHER_DEFINES,
    features = LAUNCHER_FEATURES,
    linkopts = LAUNCHER_LINKOPTS,
    visibility = ["//elisp/toolchains:__pkg__"],
)

module_config(
    name = "module_config",
    srcs = select({
        "@platforms//os:linux": ["//elisp/private/tools:module.lds"],
        "@platforms//os:windows": ["//elisp/private/tools:module.def"],
        "@platforms//os:macos": [],
    }),
    linkopts = select({
        # Export only symbols that make up the module interface.
        "@platforms//os:linux": [
            "-Wl,--version-script=$(execpath //elisp/private/tools:module.lds)",
            "-Wl,--no-undefined",
        ],
        "@platforms//os:windows": [
            "/DEF:$(execpath //elisp/private/tools:module.def)",
        ],
        "@platforms//os:macos": [
            "-Wl,-exported_symbol,_emacs_module_init",
            "-Wl,-exported_symbol,_plugin_is_GPL_compatible",
        ],
    }),
    suffix = select({
        "@platforms//os:linux": ".so",
        "@platforms//os:windows": ".dll",
        "@platforms//os:macos": ".dylib",
    }),
)
