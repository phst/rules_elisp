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

"""Defines the `elisp_binary` rule."""

load("@rules_cc//cc:find_cc_toolchain.bzl", "CC_TOOLCHAIN_ATTRS", "use_cc_toolchain")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")
load("//elisp/private:binary.bzl", "binary")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_DEPS")
load("//elisp/private:compile.bzl", "COMPILE_ATTRS")

visibility("public")

def _elisp_binary_impl(ctx):
    """Rule implementation for the “elisp_binary” rules."""
    args = []
    if ctx.attr.interactive:
        args.append("--interactive")
    if 0 in ctx.attr.input_args:
        fail("input argument index may not be zero")
    if 0 in ctx.attr.output_args:
        fail("output argument index may not be zero")
    args += ["--input-arg=" + str(i) for i in ctx.attr.input_args]
    args += ["--output-arg=" + str(i) for i in ctx.attr.output_args]
    executable, runfiles = binary(
        ctx,
        srcs = ctx.files.src,
        tags = [],
        args = args,
    )
    return [
        DefaultInfo(
            executable = executable,
            runfiles = runfiles,
        ),
    ]

elisp_binary = rule(
    # FIXME: Remove CC_TOOLCHAIN_ATTRS once
    # https://github.com/bazelbuild/bazel/issues/7260 is fixed.
    attrs = CC_TOOLCHAIN_ATTRS | COMPILE_ATTRS | {
        "src": attr.label(
            doc = "Source file to load.",
            allow_single_file = [".el"],
            mandatory = True,
        ),
        "_launcher_deps": attr.label_list(
            default = LAUNCHER_DEPS + [Label("//elisp/private/tools:binary")],
            providers = [CcInfo],
        ),
        "data": attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        "deps": attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
        "interactive": attr.bool(
            doc = "Run Emacs in interactive instead of batch mode.",
        ),
        "input_args": attr.int_list(
            doc = """Indices of command-line arguments that represent input
filenames.  These numbers specify indices into the `argv` array.  Negative
indices are interpreted as counting from the end of the array.  For example,
the index `2` stands for `argv[2]`, and the index `-2` stands for
`argv[argc - 2]`.  When passing arguments to an `emacs_binary` program on the
command line, the corresponding arguments are treated as filenames for input
files and added to the `inputFiles` field of the manifest.  This only has an
effect for toolchains that specify `wrap = True`.""",
        ),
        "output_args": attr.int_list(
            doc = """Indices of command-line arguments that represent output
filenames.  These numbers specify indices into the `argv` array.  Negative
indices are interpreted as counting from the end of the array.  For example,
the index `2` stands for `argv[2]`, and the index `-2` stands for
`argv[argc - 2]`.  When passing arguments to an `emacs_binary` program on the
command line, the corresponding arguments are treated as filenames for output
files and added to the `outputFiles` field of the manifest.  This only has an
effect for toolchains that specify `wrap = True`.""",
        ),
    },
    doc = """Binary rule that loads a single Emacs Lisp file.
The source file is byte-compiled.  At runtime, the compiled version is loaded
in batch mode unless `interactive` is `True`.""",
    executable = True,
    fragments = ["cpp"],
    host_fragments = ["cpp"],
    toolchains = use_cc_toolchain() + [Label("//elisp:toolchain_type")],
    implementation = _elisp_binary_impl,
)
