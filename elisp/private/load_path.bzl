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

"""Private utility functions to work with the Emacs load path."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    ":filenames.bzl",
    "check_relative_filename",
)

visibility("//elisp")

def resolve_load_path(ctx, dir):
    """Return an entry for the load path suitable for `EmacsLispInfo`.

    Args:
      ctx (ctx): action context
      dir (string): directory relative to the workspace root

    Returns:
      a load directory structure as described in the `EmacsLispInfo`
      documentation
    """
    return struct(
        # Actions should load byte-compiled files.  Since we place them into the
        # bin directory, we need to start from there, append the repository root
        # (see https://bazel.build/rules/lib/Label#workspace_root), and then the
        # directory name relative to the repository root.  The repository root
        # will only be nonempty if the current rule lives in a different
        # repository than the one that Bazel is run from.  This approach also
        # works for dynamic modules placed in the bin directory.
        for_actions = check_relative_filename(
            paths.join(ctx.bin_dir.path, ctx.label.workspace_root, dir),
        ),
        # The runfiles tree looks different, see
        # https://bazel.build/remote/output-directories#layout-diagram.  The
        # top-level directories in the runfiles root are always the repository
        # names, and the load directories are relative to those.  The repository
        # name is the repository part of the lexical label, see
        # https://bazel.build/rules/lib/Label#workspace_name.  Therefore, it can
        # be empty, in which case we need to use the current repository.
        for_runfiles = check_relative_filename(
            paths.join(ctx.label.workspace_name or ctx.workspace_name, dir),
        ),
    )
