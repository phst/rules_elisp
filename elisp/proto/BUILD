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

load("@rules_proto//proto:defs.bzl", "proto_library")
load("@upb//bazel:upb_proto_library.bzl", "upb_proto_library", "upb_proto_reflection_library")
load("//elisp:defs.bzl", "elisp_binary", "elisp_cc_module", "elisp_library", "elisp_proto_library", "elisp_test")
load("//private:defs.bzl", "CONLYOPTS", "COPTS", "DEFINES", "FEATURES", "PACKAGE_FEATURES")

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

elisp_test(
    name = "proto_test",
    size = "small",
    srcs = ["proto-test.el"],
    flaky = True,  # https://debbugs.gnu.org/46722
    deps = [
        ":any_elisp_proto",
        ":descriptor_elisp_proto",
        ":duration_elisp_proto",
        ":proto",
        ":test_elisp_proto",
        ":timestamp_elisp_proto",
        ":type_elisp_proto",
        ":wrappers_elisp_proto",
    ],
)

# Well-known protocol buffers.  See @com_google_protobuf//:BUILD.
elisp_proto_library(
    name = "any_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:any_proto"],
)

elisp_proto_library(
    name = "api_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:api_proto"],
)

elisp_proto_library(
    name = "compiler_plugin_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:compiler_plugin_proto"],
)

elisp_proto_library(
    name = "descriptor_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:descriptor_proto"],
)

elisp_proto_library(
    name = "duration_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:duration_proto"],
)

elisp_proto_library(
    name = "empty_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:empty_proto"],
)

elisp_proto_library(
    name = "field_mask_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:field_mask_proto"],
)

elisp_proto_library(
    name = "source_context_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:source_context_proto"],
)

elisp_proto_library(
    name = "struct_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:struct_proto"],
)

elisp_proto_library(
    name = "timestamp_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:timestamp_proto"],
)

elisp_proto_library(
    name = "type_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:type_proto"],
)

elisp_proto_library(
    name = "wrappers_elisp_proto",
    visibility = ["//visibility:public"],
    deps = ["@com_google_protobuf//:wrappers_proto"],
)

elisp_cc_module(
    name = "module",
    srcs = ["module.c"],
    copts = COPTS + CONLYOPTS + select({
        "//private:msvc-cl": ["/wd4100"],
        "//private:gcc_or_clang": ["-fvisibility=hidden"],
    }),
    features = FEATURES + ["-default_link_libs"],
    local_defines = DEFINES,
    deps = [
        ":any_upb_proto",
        ":any_upbdefs_proto",
        ":duration_upb_proto",
        ":duration_upbdefs_proto",
        ":timestamp_upb_proto",
        ":timestamp_upbdefs_proto",
        "//emacs:module_header",
        "@com_google_absl//absl/base:config",
        "@com_google_absl//absl/base:core_headers",
        "@upb",
        "@upb//:collections",
        "@upb//:descriptor_upb_proto",
        "@upb//:json",
        "@upb//:message",
        "@upb//:reflection",
        "@upb//:wire",
        "@upb//upb/base",
        "@upb//upb/mem",
        "@upb//upb/message:types",
        "@upb//upb/text",
        "@upb//upb/util:required_fields",
    ],
)

elisp_binary(
    name = "generate",
    src = "generate.el",
    input_args = [1],
    output_args = [2],
    visibility = [
        # FIXME: Make private once
        # https://github.com/bazelbuild/proposals/blob/main/designs/2019-10-15-tool-visibility.md
        # is implemented in all supported Bazel versions.
        "//visibility:public",
    ],
    deps = [":proto"],
)

upb_proto_library(
    name = "any_upb_proto",
    deps = ["@com_google_protobuf//:any_proto"],
)

upb_proto_reflection_library(
    name = "any_upbdefs_proto",
    deps = ["@com_google_protobuf//:any_proto"],
)

upb_proto_library(
    name = "duration_upb_proto",
    deps = ["@com_google_protobuf//:duration_proto"],
)

upb_proto_reflection_library(
    name = "duration_upbdefs_proto",
    deps = ["@com_google_protobuf//:duration_proto"],
)

upb_proto_library(
    name = "timestamp_upb_proto",
    deps = ["@com_google_protobuf//:timestamp_proto"],
)

upb_proto_reflection_library(
    name = "timestamp_upbdefs_proto",
    deps = ["@com_google_protobuf//:timestamp_proto"],
)

proto_library(
    name = "test_proto",
    testonly = True,
    srcs = ["test.proto"],
)

elisp_proto_library(
    name = "test_elisp_proto",
    testonly = True,
    deps = [":test_proto"],
)
