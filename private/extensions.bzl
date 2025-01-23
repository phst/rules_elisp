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
        Label("//private:emacs.BUILD.template"),
        {
            '"[defs_bzl]"': repr(str(Label("//emacs:defs.bzl"))),
            '"[emacs_pkg]"': repr(str(Label("//emacs:__pkg__"))),
            '"[src]"': repr(output),
            '"[strip_prefix]"': repr(ctx.attr.strip_prefix),
            '"[mode]"': repr(ctx.attr.mode),
            "[[compatible_with]]": repr([str(label) for label in ctx.attr.target_compatible_with]),
        },
        executable = False,
    )

_emacs_repository = repository_rule(
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
        Label("//private:local.BUILD.template"),
        {
            '"[native_binary.bzl]"': repr(str(Label("@bazel_skylib//rules:native_binary.bzl"))),
            '"[elisp_pkg]"': repr(str(Label("//elisp:__pkg__"))),
        },
        executable = False,
    )

_local_emacs_repository = repository_rule(
    implementation = _local_emacs_repository_impl,
    local = True,
)

def _deps_impl(ctx):
    for module in ctx.modules:
        for emacs in module.tags.emacs:
            major, _, _ = emacs.version.partition(".")
            _emacs_repository(
                name = "gnu_emacs_" + emacs.version,
                path = "/emacs/emacs-{}.tar.xz".format(emacs.version),
                integrity = emacs.source_integrity,
                output = "emacs.tar.xz",
                strip_prefix = "emacs-{}".format(emacs.version),
                mode = "source",
            )
            _emacs_repository(
                name = "gnu_emacs_windows_" + emacs.version,
                path = "/emacs/windows/emacs-{}/emacs-{}.zip".format(major, emacs.version),
                integrity = emacs.windows_integrity,
                output = "emacs.zip",
                strip_prefix = "emacs-{}".format(emacs.version) if major == "28" else "",
                mode = "release",
                target_compatible_with = [
                    Label("@platforms//os:windows"),
                    Label("@platforms//cpu:x86_64"),
                ],
            )
        for local_emacs in module.tags.local_emacs:
            _local_emacs_repository(name = local_emacs.name)
    return modules.use_all_repos(ctx)

deps = module_extension(
    tag_classes = {
        "emacs": tag_class(
            attrs = {
                "version": attr.string(mandatory = True),
                "source_integrity": attr.string(mandatory = True),
                "windows_integrity": attr.string(mandatory = True),
            },
        ),
        "local_emacs": tag_class(
            attrs = {
                "name": attr.string(mandatory = True),
            },
        ),
    },
    implementation = _deps_impl,
)
