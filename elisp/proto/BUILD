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
load("@protobuf//bazel:upb_c_proto_library.bzl", "upb_c_proto_library")
load("@protobuf//bazel:upb_proto_reflection_library.bzl", "upb_proto_reflection_library")
load("@protobuf//bazel/toolchains:proto_lang_toolchain.bzl", "proto_lang_toolchain")
load("//elisp:elisp_cc_module.bzl", "elisp_cc_module")
load("//elisp:elisp_library.bzl", "elisp_library")
load("//elisp/private:cc_config.bzl", "CONLYOPTS", "COPTS", "DEFINES", "FEATURES")
load("//private:package_features.bzl", "PACKAGE_FEATURES")
load(":elisp_proto_library.bzl", "elisp_proto_library")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

elisp_library(
    name = "proto",
    srcs = ["proto.el"],
    visibility = ["//visibility:public"],
    deps = [":module"],
)

# Well-known protocol buffers.  See @protobuf//:BUILD.
elisp_proto_library(
    name = "any_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:any_proto"],
)

elisp_proto_library(
    name = "api_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:api_proto"],
)

elisp_proto_library(
    name = "compiler_plugin_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:compiler_plugin_proto"],
)

elisp_proto_library(
    name = "descriptor_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:descriptor_proto"],
)

elisp_proto_library(
    name = "duration_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:duration_proto"],
)

elisp_proto_library(
    name = "empty_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:empty_proto"],
)

elisp_proto_library(
    name = "field_mask_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:field_mask_proto"],
)

elisp_proto_library(
    name = "source_context_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:source_context_proto"],
)

elisp_proto_library(
    name = "struct_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:struct_proto"],
)

elisp_proto_library(
    name = "timestamp_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:timestamp_proto"],
)

elisp_proto_library(
    name = "type_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:type_proto"],
)

elisp_proto_library(
    name = "wrappers_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@protobuf//:wrappers_proto"],
)

proto_lang_toolchain(
    name = "toolchain",
    command_line = "--elisp_out=$(OUT)",
    mnemonic = "GenElispProto",
    output_files = "multiple",
    plugin = "//elisp/private/tools:gen_proto",
    plugin_format_flag = "--plugin=protoc-gen-elisp=%s",
    progress_message = "Generating Emacs Lisp protocol buffer library %{label}",
    runtime = ":proto",
    visibility = ["//visibility:public"],
)

bzl_library(
    name = "elisp_proto_library",
    srcs = ["elisp_proto_library.bzl"],
    visibility = ["//visibility:public"],
    deps = [
        "//elisp/common:elisp_info",
        "//elisp/private:compile",
        "//elisp/private:filenames",
        "@bazel_skylib//lib:paths",
        "@protobuf//bazel/common:proto_common_bzl",
        "@protobuf//bazel/common:proto_info_bzl",
    ],
)

elisp_cc_module(
    name = "module",
    srcs = ["module.c"],
    copts = COPTS + CONLYOPTS + select({
        "//elisp/private:gcc_or_clang": ["-fvisibility=hidden"],
        "//elisp/private:msvc_or_clang_cl": ["/wd4100"],
    }),
    features = FEATURES + ["-default_link_libs"],
    local_defines = DEFINES,
    visibility = ["//tests/proto:__pkg__"],
    deps = [
        ":any_upb_c_proto",
        ":any_upb_proto_reflection",
        ":compiler_plugin_upb_c_proto",
        ":descriptor_upb_c_proto",
        ":duration_upb_c_proto",
        ":duration_upb_proto_reflection",
        ":timestamp_upb_c_proto",
        ":timestamp_upb_proto_reflection",
        "//emacs:module_header",
        "@abseil-cpp//absl/base:config",
        "@abseil-cpp//absl/base:core_headers",
        "@protobuf//upb/base",
        "@protobuf//upb/json",
        "@protobuf//upb/mem",
        "@protobuf//upb/message",
        "@protobuf//upb/message:types",
        "@protobuf//upb/reflection",
        "@protobuf//upb/text",
        "@protobuf//upb/util:required_fields",
        "@protobuf//upb/wire",
    ],
)

upb_c_proto_library(
    name = "descriptor_upb_c_proto",
    deps = ["@protobuf//:descriptor_proto"],
)

upb_c_proto_library(
    name = "any_upb_c_proto",
    deps = ["@protobuf//:any_proto"],
)

upb_proto_reflection_library(
    name = "any_upb_proto_reflection",
    deps = ["@protobuf//:any_proto"],
)

upb_c_proto_library(
    name = "duration_upb_c_proto",
    deps = ["@protobuf//:duration_proto"],
)

upb_proto_reflection_library(
    name = "duration_upb_proto_reflection",
    deps = ["@protobuf//:duration_proto"],
)

upb_c_proto_library(
    name = "timestamp_upb_c_proto",
    deps = ["@protobuf//:timestamp_proto"],
)

upb_proto_reflection_library(
    name = "timestamp_upb_proto_reflection",
    deps = ["@protobuf//:timestamp_proto"],
)

upb_c_proto_library(
    name = "compiler_plugin_upb_c_proto",
    deps = ["@protobuf//:compiler_plugin_proto"],
)

exports_files(
    ["elisp_proto_library.bzl"],
    visibility = ["//docs:__pkg__"],
)

# gazelle:resolve elisp elisp/proto/module :module
# gazelle:resolve_regexp elisp ^google/protobuf/(\w+)\.proto$ :${1}_elisp_proto
# gazelle:resolve_regexp elisp ^google/protobuf/(\w+)/(\w+)\.proto$ :${1}_${2}_elisp_proto
