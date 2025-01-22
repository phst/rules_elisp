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

"""Defines the `elisp_library` rule."""

load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")
load("//elisp/private:compile.bzl", "COMPILE_ATTRS", "compile")

visibility("public")

def _elisp_library_impl(ctx):
    """Rule implementation for the “elisp_library” rule."""
    result = compile(
        ctx = ctx,
        srcs = ctx.files.srcs,
        deps = ctx.attr.deps,
        load_path = ctx.attr.load_path,
        data = ctx.files.data,
        tags = ctx.attr.tags,
        fatal_warnings = ctx.attr.fatal_warnings,
    )
    return [
        DefaultInfo(
            files = depset(direct = result.outs),
            runfiles = result.runfiles,
        ),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps", "srcs"],
        ),
        EmacsLispInfo(
            source_files = ctx.files.srcs,
            compiled_files = result.outs,
            load_path = result.load_path,
            data_files = ctx.files.data,
            transitive_source_files = result.transitive_srcs,
            transitive_compiled_files = result.transitive_outs,
            transitive_load_path = result.transitive_load_path,
        ),
    ]

elisp_library = rule(
    attrs = COMPILE_ATTRS | {
        "srcs": attr.label_list(
            allow_empty = False,
            doc = """List of source files.  These must either be Emacs Lisp
files ending in `.el`, or module objects ending in `.so`, `.dylib`, or
`.dll`.""",
            allow_files = [".el", ".so", ".dylib", ".dll"],
            mandatory = True,
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/7.4.1/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L74.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        "outs": attr.output_list(
            doc = """List of byte-compiled Emacs Lisp files to be made available
as targets.""",
        ),
        "data": attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        "load_path": attr.string_list(
            doc = """List of additional load path elements.
The elements are directory names, which can be either relative or absolute.
Relative names are relative to the current package.
Absolute names are relative to the repository root.
To add a load path entry for the current package, specify `.` here.""",
        ),
        "deps": attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
    },
    doc = """Byte-compiles Emacs Lisp source files and makes the compiled output
available to dependencies.  All sources are byte-compiled.
`elisp_library`, `elisp_binary`, and `elisp_test` rules depending on this binary
can then use `load` or `require` to load them.

By default, libraries need to be loaded using a filename relative to the
repository root, i.e., <var>package</var>/<var>file</var>.  If you want to add
further elements to the load path, use the `load_path` attribute.

If there are multiple source files specified in `srcs`, these source files can
also load each other.  However, it’s often preferable to only have one
`elisp_library` target per source file to make dependencies more obvious and
ensure that files get only loaded in their byte-compiled form.

The source files in `srcs` can also list shared objects.  The rule treats them
as Emacs modules and doesn’t try to byte-compile them.  You can use
e.g. `cc_binary` with `linkshared = True` to create shared objects.""",
    provides = [EmacsLispInfo],
    toolchains = [Label("//elisp:toolchain_type")],
    implementation = _elisp_library_impl,
)
