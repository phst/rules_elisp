# Copyright 2023, 2024, 2025 Google LLC
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

"""Non-module dependencies."""

load("@bazel_skylib//lib:modules.bzl", "modules")

visibility("private")

def _emacs_repository_impl(ctx):
    path = ctx.attr.path
    output = ctx.attr.output
    ctx.download(
        integrity = ctx.attr.integrity or fail("archive integrity missing"),
        url = [
            "https://ftpmirror.gnu.org" + path,
            "https://ftp.gnu.org/gnu" + path,
        ],
        output = output,
    )
    ctx.template(
        "BUILD.bazel",
        Label("//elisp/private/tools:emacs.BUILD.template"),
        {
            '"[elisp_emacs_binary.bzl]"': repr(str(Label("//elisp/toolchains:elisp_emacs_binary.bzl"))),
            '"[cc_library.bzl]"': repr(str(Label("@rules_cc//cc:cc_library.bzl"))),
            '"[emacs_pkg]"': repr(str(Label("//emacs:__pkg__"))),
            '"[gazelle_pkg]"': repr(str(Label("//gazelle:__pkg__"))),
            '"[src]"': repr(output),
            '"[strip_prefix]"': repr(ctx.attr.strip_prefix),
            '"[mode]"': repr(ctx.attr.mode),
            "[[compatible_with]]": repr([str(label) for label in ctx.attr.target_compatible_with]),
        },
        executable = False,
    )

emacs_repository = repository_rule(
    attrs = {
        "path": attr.string(mandatory = True),
        "integrity": attr.string(mandatory = True),
        "output": attr.string(mandatory = True),
        "strip_prefix": attr.string(),
        "mode": attr.string(mandatory = True, values = ["source", "release"]),
        "target_compatible_with": attr.label_list(),
    },
    implementation = _emacs_repository_impl,
)

def _local_emacs_repository_impl(ctx):
    windows = ctx.os.name.startswith("windows")
    emacs = ctx.getenv("EMACS", "emacs")
    if windows and not emacs.lower().endswith(".exe"):
        emacs += ".exe"
    sep = "\\" if windows else "/"
    if sep not in emacs:
        emacs = ctx.which(emacs)

    # Don’t fail during the loading phase if Emacs isn’t locally installed, only
    # when Emacs is actually needed.
    if emacs:
        ctx.symlink(emacs, "source.exe")
    ctx.template(
        "BUILD.bazel",
        Label("//elisp/private/tools:local.BUILD.template"),
        {
            '"[native_binary.bzl]"': repr(str(Label("@bazel_skylib//rules:native_binary.bzl"))),
            '"[elisp_pkg]"': repr(str(Label("//elisp:__pkg__"))),
        },
        executable = False,
    )

local_emacs_repository = repository_rule(
    implementation = _local_emacs_repository_impl,
    local = True,
)

def _deps_impl(ctx):
    return modules.use_all_repos(ctx)

deps = module_extension(
    implementation = _deps_impl,
)
