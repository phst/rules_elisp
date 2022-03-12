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

load("//elisp:defs.bzl", "elisp_binary", "elisp_library", "elisp_proto_library", "elisp_test")
load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@upb//bazel:upb_proto_library.bzl", "upb_proto_reflection_library")

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
    deps = [
        ":any_elisp_proto",
        ":descriptor_elisp_proto",
        ":duration_elisp_proto",
        ":proto",
        ":test_messages_proto2_elisp_proto",
        ":test_messages_proto3_elisp_proto",
        ":timestamp_elisp_proto",
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

elisp_library(
    name = "module",
    srcs = select({
        "//constraints:linux": ["module.so"],
        "//constraints:macos": ["module.so"],
        "//constraints:windows": ["module.dll"],
    }),
)

cc_binary(
    name = "module.so",
    srcs = ["module.c"],
    copts = select({
        "//constraints:msvc-cl": [
            "/std:c11",
            "/WX",
            "/W4",
            "/utf-8",
        ],
        # Assume that something compatible with GCC is the default.  See
        # https://github.com/bazelbuild/bazel/issues/12707.
        "//conditions:default": [
            "-finput-charset=utf-8",
            "-fexec-charset=utf-8",
            "-fno-exceptions",
            "-std=c99",
            "-fvisibility=hidden",
            "-Werror",
            "-Wall",
            "-Wextra",
            "-Wconversion",
            "-Wsign-conversion",
            "-Wvla",
            "-pedantic-errors",
        ],
    }),
    linkopts = select({
        # Export only symbols that make up the module interface.
        "//constraints:linux": ["-Wl,--exclude-libs,ALL"],
        "//constraints:windows": [],  # uses ‘win_def_file’
        "//constraints:macos": [
            "-Wl,-exported_symbol,_emacs_module_init",
            "-Wl,-exported_symbol,_plugin_is_GPL_compatible",
            "-Wl,-undefined,error",
        ],
    }),
    linkshared = True,
    local_defines = [
        "_XOPEN_SOURCE=600",  # for ‘struct timespec’
    ],
    win_def_file = "module.def",
    deps = [
        ":any_upb_proto",
        ":duration_upb_proto",
        ":timestamp_upb_proto",
        "//emacs:module_header",
        "@com_google_absl//absl/base:core_headers",
        "@upb",
        "@upb//:descriptor_upb_proto",
        "@upb//:json",
        "@upb//:reflection",
        "@upb//:textformat",
    ],
)

# Make a copy with the proper suffix for Windows.
copy_file(
    name = "module_dll",
    src = "module.so",
    out = "module.dll",
    allow_symlink = True,
    is_executable = False,
)

elisp_binary(
    name = "generate",
    src = "generate.el",
    input_args = [1],
    output_args = [2],
    visibility = ["//visibility:public"],
    deps = [":proto"],
)

upb_proto_reflection_library(
    name = "any_upb_proto",
    deps = ["@com_google_protobuf//:any_proto"],
)

upb_proto_reflection_library(
    name = "duration_upb_proto",
    deps = ["@com_google_protobuf//:duration_proto"],
)

upb_proto_reflection_library(
    name = "timestamp_upb_proto",
    deps = ["@com_google_protobuf//:timestamp_proto"],
)

elisp_proto_library(
    name = "test_messages_proto2_elisp_proto",
    deps = ["@com_google_protobuf//:test_messages_proto2_proto"],
)

elisp_proto_library(
    name = "test_messages_proto3_elisp_proto",
    deps = ["@com_google_protobuf//:test_messages_proto3_proto"],
)
