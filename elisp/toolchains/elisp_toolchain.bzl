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

"""Defines the `elisp_toolchain` rule."""

visibility("public")

def _elisp_toolchain_impl(ctx):
    """Rule implementation for the “elisp_toolchain” toolchain rule."""
    return [
        platform_common.ToolchainInfo(
            emacs = ctx.attr.emacs,
            use_default_shell_env = ctx.attr.use_default_shell_env,
            execution_requirements = ctx.attr.execution_requirements,
            wrap = ctx.attr.wrap,
        ),
    ]

elisp_toolchain = rule(
    implementation = _elisp_toolchain_impl,
    attrs = {
        "emacs": attr.label(
            doc = """An executable file that behaves like the Emacs binary.
Depending on whether `wrap` is `True`, Bazel invokes this executable
with a command line like `emacs --manifest=MANIFEST -- ARGS…` or `emacs ARGS…`.
The `--manifest` flag is only present if `wrap` is `True`.
See the rule documentation for details.""",
            mandatory = True,
            executable = True,
            cfg = "target",
        ),
        "use_default_shell_env": attr.bool(
            doc = "Whether actions should inherit the external shell environment.",
            default = False,
        ),
        "execution_requirements": attr.string_dict(
            doc = "Execution requirements for compilation and test actions.",
        ),
        "wrap": attr.bool(
            doc = """Whether the binary given in the `emacs` attribute is a
wrapper around Emacs proper.
If `True`, Bazel passes a manifest file using the `--manifest` option.
See the rule documentation for details.""",
            default = False,
        ),
    },
    doc = """Toolchain rule for Emacs Lisp.
This toolchain configures how to run Emacs.
The executable passed to the `emacs` attribute must be a binary
that behaves like Emacs.
If `wrap` is `False`, Bazel calls it as is, passing arguments
that a normal Emacs binary would accept.
If `wrap` is `True`, Bazel calls the binary with a special `--manifest` option.
The value of the option is the filename of a JSON file containing a manifest.
The manifest specifies which files should be readable and/or writable by Emacs.
Toolchains can use this to sandbox Emacs, if desired.

If `wrap` is `True`, the format of the command line is as follows:

```bash
emacs --manifest=MANIFEST -- ARGS…
```

That is, the original arguments for Emacs are separated by a double hyphen
(`--`) so that argument parsers can distinguish between the `--manifest` option
and Emacs arguments.

The manifest is a JSON object with the following keys:
- `root` can be either `EXECUTION_ROOT` or `RUNFILES_ROOT` and specifies
  the root directory for relative file names.
- `loadPath` is a list of directory names making up the load path.
- `inputFiles` is a list of files that should be readable.
- `outputFiles` is a list of files that should be writable.
- `tags` is the list of tags for the current rule.

When executing an action, file names are relative to the execution root.
Otherwise, file names are relative to the runfiles root.  File names in
`inputFiles` or `outputFiles` can also be absolute; in this case they specify
temporary files that are deleted after the action completes, or files passed on
the command line interpreted according to the `input_args` and `output_args`
attributes of the `elisp_binary` rule.""",
    provides = [platform_common.ToolchainInfo],
)
