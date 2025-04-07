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

"""Defines the `local_emacs_repository` repository rule."""

visibility("private")

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
        # @unsorted-dict-items
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
