# Copyright 2021, 2022 Google LLC
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

load("@bazel_skylib//rules:copy_file.bzl", "copy_file")
load("@rules_perl//perl:perl.bzl", "perl_binary", "perl_library")

perl_binary(
    name = "texi2any",
    srcs = ["tp/texi2any.pl"],
    data = ["configure.ac"],
    visibility = ["//visibility:public"],
    deps = [":texinfo"],
)

perl_library(
    name = "texinfo",
    srcs = glob([
        "tp/Texinfo/*.pm",
        "tp/Texinfo/**/*.pm",
        "tp/maintain/lib/**/*.pm",
    ]) + ["tp/Texinfo/ModulePath.pm"],
)

copy_file(
    name = "module_path",
    src = "tp/Texinfo/ModulePath.pm.in",
    out = "tp/Texinfo/ModulePath.pm",
)

# Local Variables:
# mode: bazel-build
# End:
