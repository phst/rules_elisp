# Copyright 2020-2025 Google LLC
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
load("//elisp/private:build_package.bzl", "build_package")
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
    extra_out = []
    package_file = None
    metadata_file = None
    autoloads_file = None
    if ctx.attr.enable_package and not ctx.attr.testonly:
        pkg = build_package(ctx, ctx.files.srcs, ctx.files.data)
        extra_out += [pkg.package_file, pkg.metadata_file, pkg.autoloads_file]
        package_file = pkg.package_file
        metadata_file = pkg.metadata_file
        autoloads_file = pkg.autoloads_file
    return [
        DefaultInfo(
            files = depset(direct = result.outs + extra_out),
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
            package_file = package_file,
            metadata_file = metadata_file,
            autoloads_file = autoloads_file,
        ),
    ]

elisp_library = rule(
    # @unsorted-dict-items
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
        "enable_package": attr.bool(
            doc = """Enable generation of package.el package for this library.
This value is forced to False if testonly is True.""",
            default = True,
        ),
        "emacs_package_name": attr.string(
            doc = """The name used for the package.el package.
This attribute is ignored if enable_package is False.
Otherwise, srcs should contain a package description file `<name>-pkg.el`.
If there is no such package description file, then srcs must contain a file
`<name>.el` containing the appropriate package headers.

If there is only one file in srcs, then the default value is the file basename
with the .el suffix removed.  Otherwise, the default is the target label name,
with underscores replaced with dashes.""",
        ),
        "_gen_pkg_el": attr.label(
            default = "//elisp:gen-pkg-el.elc",
            allow_single_file = [".elc"],
        ),
        "_gen_metadata": attr.label(
            default = "//elisp:gen-metadata.elc",
            allow_single_file = [".elc"],
        ),
        "_gen_autoloads": attr.label(
            default = "//elisp:gen-autoloads.elc",
            allow_single_file = [".elc"],
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
