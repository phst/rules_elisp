# Copyright 2020-2026 Google LLC
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

load("@rules_go//go:go_binary.bzl", "go_binary")
load("@rules_go//go:go_library.bzl", "go_library")
load("@rules_go//proto:def.bzl", "go_proto_library")
load("//private:package_features.bzl", "PACKAGE_FEATURES")

package(
    default_package_metadata = ["//:license"],
    default_visibility = ["//visibility:private"],
    features = PACKAGE_FEATURES,
)

licenses(["notice"])

go_binary(
    name = "generate",
    embed = [":docs"],
    visibility = ["//docs:__pkg__"],
)

go_library(
    name = "docs",
    srcs = ["generate.go"],
    importpath = "github.com/phst/rules_elisp/internal/docs",
    deps = [
        ":stardoc_output_go_proto",
        "@com_github_yuin_goldmark_v2//ast",
        "@com_github_yuin_goldmark_v2//parser",
        "@com_github_yuin_goldmark_v2//renderer",
        "@org_golang_google_protobuf//proto",
        "@org_golang_x_text//cases",
        "@org_golang_x_text//encoding/charmap",
        "@org_golang_x_text//language",
    ],
)

go_proto_library(
    name = "stardoc_output_go_proto",
    importpath = "github.com/phst/rules_elisp/internal/docs/stardoc_output_go_proto",
    protos = ["@stardoc//stardoc/proto:stardoc_output_proto"],
)
