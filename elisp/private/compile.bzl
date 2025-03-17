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

"""Private utility functions to byte-compile Emacs Lisp source files."""

load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")
load(":cc_launcher_config.bzl", "LAUNCHER_ATTRS")
load(":filenames.bzl", "check_relative_filename", "repository_relative_filename")
load(":load_path.bzl", "resolve_load_path")
load(":run_emacs.bzl", "run_emacs")

visibility(["//elisp", "//elisp/proto"])

# Compilation-related attributes shared between elisp_library, elisp_binary,
# and elisp_test.
COMPILE_ATTRS = LAUNCHER_ATTRS | {
    "fatal_warnings": attr.bool(
        doc = """If `True` (the default), then byte compile warnings should be
treated as errors.  If `False`, they still show up in the output, but don’t
cause the compilation to fail.  Most targets should leave this attribute as
`True`, because otherwise important issues might remain undetected.  Set this
attribute to `False` only for integrating third-party libraries that don’t
compile cleanly and that you don’t control.""",
        default = True,
    ),
    "_compile": attr.label(
        default = Label("//elisp/private/tools:compile.elc"),
        allow_single_file = [".elc"],
    ),
}

def compile(ctx, *, srcs, deps, load_path, data, tags, fatal_warnings):
    """Byte-compiles Emacs Lisp source files.

    Args:
      ctx (ctx): rule context
      srcs (list of Files): Emacs Lisp sources files to compile; can also
          include module objects
      deps (list of targets): Emacs Lisp libraries that the sources depend on
      load_path (list of strings): additional load path directories, relative
          to the current package
      data (list of Files): data files to be made available at runtime
      tags (list of strings): list of rule tags to write into the manifest
      fatal_warnings (bool): whether compilation warnings should be treated as
          errors

    Returns:
      A structure with the following fields:
        outs: a list of File objects containing the byte-compiled files and
            module objects
        load_path: the load path required to load the compiled files
        runfiles: a runfiles object for the set of input files
        transitive_load_path: the load path required to load the compiled files
            and all their transitive dependencies
        transitive_srcs: a depset of source files for this compilation unit
            and all its transitive dependencies
        transitive_outs: a depset of compiled files and module objects for this
            compilation unit and all its transitive dependencies
    """

    # Only byte-compile Lisp source files.  Use module objects directly as
    # outputs.
    lisp = [src for src in srcs if src.extension == "el"]
    mods = [src for src in srcs if src.extension in ("so", "dylib", "dll")]
    outs = mods

    # If any file comes for a different package, we can’t place the compiled
    # files adjacent to the source files.  See
    # https://bazel.build/rules/lib/actions#declare_file.
    relocate_output = any([
        src.owner.workspace_name != ctx.label.workspace_name or
        src.owner.package != ctx.label.package
        for src in lisp
    ])

    # When relocating output, we’d need to copy modules into the relocated
    # places, which is possible, but not yet supported.  Likewise, we require
    # all modules to reside in the bin directory for now.
    for mod in mods:
        if mod.root != ctx.bin_dir or relocate_output:
            fail("module object {} in unsupported location".format(mod.path))

    # Directory relative to the repository root where outputs should be stored.
    # We prefer storing them adjacent to source files to reduce the number of
    # load path entries, but if necessary, we generate a subdirectory in the
    # current package.
    output_base = (
        paths.join(ctx.label.package, _OUTPUT_DIR) if relocate_output else ""
    )
    resolved_load_path = []
    source_load_path = []
    source_roots = sorted(collections.uniq([
        check_relative_filename(paths.normalize(paths.join(src.root.path, src.owner.workspace_root)))
        for src in srcs
    ]))
    for dir in ["/"] + load_path:
        if not dir:
            fail("empty directory in load path")
        if paths.is_absolute(dir):
            dir = "." + dir
        else:
            dir = paths.join(ctx.label.package, check_relative_filename(dir))
        dir = check_relative_filename(dir)
        if len(srcs) > 1:
            # If we have more than one source file, we need to add the
            # respective source directory to the load path for this rule’s
            # actions only, so that the source files can load each other.
            source_load_path += [
                check_relative_filename(paths.join(root, dir))
                for root in source_roots
            ]

        # At least some of the sources must be reachable from the directory.
        prefix = "./" if dir == "." else "./" + dir + "/"
        if not any([("./" + repository_relative_filename(src)).startswith(prefix) for src in srcs]):
            fail("None of the files [{}] are reachable from load path directory {}"
                .format(", ".join([repository_relative_filename(src) for src in srcs]), dir))

        # If we’re compiling source files from another package, we need to
        # insert the output base directory for this rule.  In that case, we
        # still have to append the repository-relative directory, so that
        # filenames relative to the (relocated) repository root work.
        dir = check_relative_filename(paths.join(output_base, dir))
        resolved_load_path.append(resolve_load_path(ctx, dir))

    indirect_srcs = [
        dep[EmacsLispInfo].transitive_source_files
        for dep in deps
    ]
    indirect_outs = [
        dep[EmacsLispInfo].transitive_compiled_files
        for dep in deps
    ]
    indirect_load_path = [
        dep[EmacsLispInfo].transitive_load_path
        for dep in deps
    ]
    transitive_load_path = depset(
        direct = resolved_load_path,
        # We explicitly specify preorder traversal.  The load path is an
        # ordered list, not a set, so the traversal order matters when
        # generating the --directory flags for tests and binaries.  Using
        # preorder traversal makes most sense since it causes libraries closer
        # to the binary/test in the dependency graph to be considered first.
        order = "preorder",
        transitive = indirect_load_path,
    )
    transitive_data = depset(
        direct = data,
        transitive = [
            dep[DefaultInfo].default_runfiles.files
            for dep in deps
        ],
    )

    toolchain = ctx.toolchains[Label("//elisp:toolchain_type")]

    # Expand load path only if needed.  It’s important that the expanded load
    # path is equivalent to the --directory arguments below.
    flat_load_path = [
        _load_directory_for_actions(d)
        for d in depset(
            order = "preorder",
            transitive = indirect_load_path,
        ).to_list()
    ] + source_load_path if toolchain.wrap else None

    # We compile only one file per Emacs process.  This might seem wasteful,
    # but since compilation can execute arbitrary code, it ensures that
    # compilation actions don’t interfere with each other.
    for src in lisp:
        out = (
            ctx.actions.declare_file(
                paths.join(
                    _OUTPUT_DIR,
                    paths.replace_extension(repository_relative_filename(src), ".elc"),
                ),
            ) if relocate_output else ctx.actions.declare_file(
                paths.replace_extension(src.basename, ".elc"),
                sibling = src,
            )
        )
        args = ctx.actions.args()
        args.add_all(
            # We don’t add the full transitive load path here because the
            # direct load path would only contain the file to be compiled.
            depset(order = "preorder", transitive = indirect_load_path),
            map_each = _load_directory_for_actions,
            format_each = "--directory=%s",
            uniquify = True,
            expand_directories = False,
        )
        args.add_all(
            source_load_path,
            format_each = "--directory=%s",
            uniquify = True,
            expand_directories = False,
        )
        args.add(ctx.file._compile, format = "--load=%s")
        if fatal_warnings:
            args.add("--fatal-warnings")
        args.add(src.owner.workspace_name)
        args.add(src)
        args.add(out)
        run_emacs(
            ctx = ctx,
            outputs = [out],
            # Add all other source files as secondary input files so they can
            # load each other if necessary.  The main source file needs to come
            # first for %{input} to work.
            inputs = depset(
                direct = [src, ctx.file._compile] + [s for s in srcs if s != src],
                transitive = indirect_outs + [transitive_data],
                order = "preorder",
            ),
            arguments = [args],
            tags = tags,
            mnemonic = "ElispCompile",
            progress_message = "Compiling %{input}",
            manifest_basename = out.basename,
            manifest_sibling = out,
            manifest_load_path = flat_load_path,
        )
        outs.append(out)

    return struct(
        outs = outs,
        load_path = resolved_load_path,
        runfiles = ctx.runfiles(transitive_files = transitive_data),
        transitive_load_path = transitive_load_path,
        transitive_srcs = depset(direct = srcs, transitive = indirect_srcs),
        transitive_outs = depset(direct = outs, transitive = indirect_outs),
    )

def _load_directory_for_actions(directory):
    """Returns the load directory to be used for build-time actions.

    Args:
      directory (struct): an element of the load path of `EmacsLispProvider`

    Returns:
      the directory name of the load directory relative to the action’s
          execution root
    """

    # This trivial function exists because we have to pass a function to
    # map_each above.
    return check_relative_filename(directory.for_actions)

# Directory relative to the current package where to store compiled files.  This
# is equivalent to _objs for C++ rules.  See
# https://bazel.build/remote/output-directories#layout-diagram.
_OUTPUT_DIR = "_elisp"
