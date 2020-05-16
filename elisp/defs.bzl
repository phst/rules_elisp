# Copyright 2020 Google LLC
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

"""Defines rules to work with Emacs Lisp files in Bazel."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    ":util.bzl",
    "cc_wrapper",
    "check_relative_filename",
    "configure_cc_toolchain",
)

EmacsLispInfo = provider(
    doc = """Provider for Emacs Lisp libraries.
The `elisp_library` rule produces this provider.

Load path directory entries are structures with the following fields:
- `for_actions` is a string specifying the load directory to use for actions,
  relative to the execution root.
- `for_runfiles` is a string specifying the load directory to use at runtime,
  relative to the runfiles root.""",
    fields = {
        "source_files": """A list of `File` objects containing
the Emacs Lisp source files of this library.""",
        "compiled_files": """A list of `File` objects containing
the byte-compiled Emacs Lisp files of this library.""",
        "load_path": """A list containing necessary load path
additions for this library.  The `depset` elements are structures as
described in the provider documentation.""",
        "data_files": """A list of `File` object that this library requires
at runtime.""",
        "transitive_source_files": """A `depset` of `File` objects containing
the Emacs Lisp source files of this library
and all its transitive dependencies.""",
        "transitive_compiled_files": """A `depset` of `File` objects containing
the byte-compiled Emacs Lisp files of this library
and all its transitive dependencies.""",
        "transitive_load_path": """A `depset` containing necessary load path
additions for this library and all its transitive dependencies.
The `depset` uses preorder traversal: entries for libraries closer to the root
of the dependency graph come first.  The `depset` elements are structures as
described in the provider documentation.""",
    },
)

def _toolchain_rule(ctx):
    """Rule implementation for the “elisp_toolchain” toolchain rule."""
    return platform_common.ToolchainInfo(
        emacs = ctx.attr.emacs,
        use_default_shell_env = ctx.attr.use_default_shell_env,
        wrap = ctx.attr.wrap,
    )

# Note: Toolchain names need to be fully qualified, otherwise external
# workspaces won’t find them.
_TOOLCHAIN_TYPE = "@phst_rules_elisp//elisp:toolchain_type"

def _toolchain(ctx):
    """Return the Emacs Lisp toolchain for the given context."""
    return ctx.toolchains[_TOOLCHAIN_TYPE]

def _library(ctx):
    """Rule implementation for the “elisp_library” rule."""
    result = _compile(ctx, ctx.files.srcs, ctx.attr.deps, ctx.attr.load_path)
    return [
        DefaultInfo(
            files = depset(direct = result.outs),
            runfiles = result.runfiles,
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

def _binary(ctx):
    """Rule implementation for the “elisp_binary” and “elisp_test” rules.

    The rule should define a “_template” attribute containing the C++ template
    file to be expanded.
    """
    is_test = hasattr(ctx.attr, "_lcov_merger")
    srcs = ctx.files.srcs if hasattr(ctx.files, "srcs") else ctx.files.src
    result = _compile(ctx, srcs, ctx.attr.deps, [])
    toolchain = _toolchain(ctx)
    emacs = toolchain.emacs

    # Only pass in data files when needed.
    data_files_for_manifest = (
        result.runfiles.files.to_list() if toolchain.wrap else []
    )

    # If we’re supposed to generate coverage information, use source files
    # instead of compiled files because we can’t instrument compiled files for
    # coverage.  We ignore ctx.coverage_instrumented because that doesn’t work
    # here: it assumes that coverage is generated during compilation, but we
    # can generate coverage information only at runtime.  Bazel’s coverage
    # support isn’t really documented; some information is available in the
    # source code comments of the file
    # https://github.com/bazelbuild/bazel/blob/3.0.0/src/main/java/com/google/devtools/build/lib/bazel/coverage/CoverageReportActionBuilder.java.
    transitive_files = (
        result.transitive_srcs if ctx.configuration.coverage_enabled else result.transitive_outs
    )

    # We use a C++ driver because the C++ toolchain framework exposes
    # individual actions (unlike Python), and the runfiles implementation
    # doesn’t have bugs (unlike Go).  We use raw strings to minimize the risk
    # of misinterpreting special characters in a filename.
    # check_relative_filename should already reject all special characters, but
    # better be sure.
    driver = ctx.actions.declare_file("_" + ctx.label.name + ".cc")
    ctx.actions.expand_template(
        template = ctx.file._template,
        output = driver,
        substitutions = {
            "[[directory]]": ", ".join([
                'R"**({})**"'.format(check_relative_filename(dir.for_runfiles))
                for dir in result.transitive_load_path.to_list()
            ]),
            "[[emacs]]": check_relative_filename(paths.join(
                ctx.workspace_name,
                emacs.files_to_run.executable.short_path,
            )),
            "[[load]]": ", ".join([
                'R"**({})**"'.format(check_relative_filename(
                    paths.join(ctx.workspace_name, src.short_path),
                ))
                for src in result.outs
            ]),
            "[[data]]": ", ".join([
                'R"**({})**"'.format(check_relative_filename(
                    paths.join(ctx.workspace_name, file.short_path),
                ))
                for file in data_files_for_manifest
            ]),
            "[[skip_tests]]": _cpp_strings(getattr(ctx.attr, "skip_tests", [])),
            "[[skip_tags]]": _cpp_strings(getattr(ctx.attr, "skip_tags", [])),
            "[[mode]]": "kWrap" if toolchain.wrap else "kDirect",
        },
    )
    cc_toolchain, feature_configuration = configure_cc_toolchain(ctx)
    executable = cc_wrapper(ctx, cc_toolchain, feature_configuration, driver)
    bin_runfiles = ctx.runfiles(
        files = (
            [emacs.files_to_run.executable] + ctx.files._default_libs +
            result.outs +
            # We include the original source files in the runfiles so that
            # error messages in tests can link back to them.
            (srcs if is_test else [])
        ),
        transitive_files = depset(
            transitive = [transitive_files, result.runfiles.files],
        ),
    )
    emacs_runfiles = emacs.default_runfiles
    runfiles = bin_runfiles.merge(emacs_runfiles)

    test_env = {}
    if ctx.configuration.coverage_enabled and hasattr(ctx.attr, "_lcov_merger"):
        # Bazel’s coverage runner
        # (https://github.com/bazelbuild/bazel/blob/3.0.0/tools/test/collect_coverage.sh)
        # needs a binary called “lcov_merge.”  Its location is passed in the
        # LCOV_MERGER environmental variable.  For builtin rules, this variable
        # is set automatically based on a magic “$lcov_merger” or
        # “:lcov_merger” attribute, but it’s not possible to create such
        # attributes in Starlark.  Therefore we specify the variable ourselves.
        # Note that the coverage runner runs in the runfiles root instead of
        # the execution root, therefore we use “path” instead of “short_path.”
        runfiles = runfiles.merge(
            ctx.attr._lcov_merger[DefaultInfo].default_runfiles,
        )
        test_env["LCOV_MERGER"] = ctx.executable._lcov_merger.path

    # The InstrumentedFilesInfo provider needs to be added here instead of in
    # the “elisp_library” rule for coverage collection to work.
    return [
        DefaultInfo(
            executable = executable,
            runfiles = runfiles,
        ),
        testing.TestEnvironment(test_env),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps"],
            extensions = ["el"],
        ),
    ]

elisp_toolchain = rule(
    implementation = _toolchain_rule,
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

When executing an action, file names are relative to the execution root.
Otherwise, file names are relative to the runfiles root.
File names in `outputFiles` can also be absolute; in this case they
specify temporary files that are deleted after the action completes.""",
    provides = [platform_common.ToolchainInfo],
)

# Compilation-related attributes shared between elisp_library, elisp_binary,
# and elisp_test.
_COMPILE_ATTRS = {
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
        default = "//elisp:compile.el",
        allow_single_file = [".el"],
    ),
}

elisp_library = rule(
    attrs = dict(
        _COMPILE_ATTRS,
        srcs = attr.label_list(
            allow_empty = False,
            doc = "List of source files.",
            allow_files = [".el"],
            mandatory = True,
        ),
        data = attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        load_path = attr.string_list(
            doc = """List of additional load path elements.
The elements are directory names, which can be either relative or absolute.
Relative names are relative to the current package.
Absolute names are relative to the workspace root.
To add a load path entry for the current package, specify `.` here.""",
        ),
        deps = attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
    ),
    doc = """Byte-compiles Emacs Lisp source files and makes the compiled output
available to dependencies. All sources are byte-compiled.
`elisp_library`, `elisp_binary`, and `elisp_test` rules depending on this binary
can then use `load` or `require` to load them.

By default, libraries need to be loaded using a filename relative to the
workspace root, i.e., <var>package</var>/<var>file</var>.  If you want to add
further elements to the load path, use the `load_path` attribute.

If there are multiple source files specified in `srcs`, these source files can
also load each other.  However, it’s often preferable to only have one
`elisp_library` target per source file to make dependencies more obvious and
ensure that files get only loaded in their byte-compiled form.""",
    provides = [EmacsLispInfo],
    toolchains = [_TOOLCHAIN_TYPE],
    implementation = _library,
)

elisp_binary = rule(
    attrs = dict(
        _COMPILE_ATTRS,
        src = attr.label(
            doc = "Source file to load.",
            allow_single_file = [".el"],
            mandatory = True,
        ),
        _cc_toolchain = attr.label(
            default = "@bazel_tools//tools/cpp:current_cc_toolchain",
            providers = [cc_common.CcToolchainInfo],
        ),
        _exec = attr.label(
            default = "//elisp:exec",
            providers = [CcInfo],
        ),
        _default_libs = attr.label_list(
            default = ["//elisp/runfiles"],
            allow_files = [".elc"],
            allow_empty = False,
        ),
        _template = attr.label(
            default = "//elisp:binary.template",
            allow_single_file = [".template"],
        ),
        data = attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        deps = attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
    ),
    doc = """Binary rule that loads a single Emacs Lisp file.
The source file is byte-compiled.  At runtime, the compiled version is loaded
in batch mode.""",
    executable = True,
    fragments = ["cpp"],
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        _TOOLCHAIN_TYPE,
    ],
    implementation = _binary,
)

elisp_test = rule(
    attrs = dict(
        _COMPILE_ATTRS,
        srcs = attr.label_list(
            allow_empty = False,
            doc = "List of source files to load.",
            allow_files = [".el"],
            mandatory = True,
        ),
        _cc_toolchain = attr.label(
            default = "@bazel_tools//tools/cpp:current_cc_toolchain",
            providers = [cc_common.CcToolchainInfo],
        ),
        _exec = attr.label(
            default = "//elisp:exec",
            providers = [CcInfo],
        ),
        _default_libs = attr.label_list(
            default = ["//elisp/runfiles", "//elisp/ert:runner"],
            allow_files = [".elc"],
            allow_empty = False,
        ),
        _template = attr.label(
            default = "//elisp:test.template",
            allow_single_file = [".template"],
        ),
        _lcov_merger = attr.label(
            default = "@bazel_tools//tools/test:lcov_merger",
            executable = True,
            cfg = "target",
        ),
        data = attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        deps = attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
        skip_tests = attr.string_list(
            doc = """List of tests to skip.  This attribute contains a list of
ERT test symbols; when running the test rule, these tests are skipped.

Most of the time, you should use [the `skip-unless`
macro](https://www.gnu.org/software/emacs/manual/html_node/ert/Tests-and-Their-Environment.html)
instead.  The `skip_tests` attribute is mainly useful for third-party code that
you don’t control.""",
        ),
        skip_tags = attr.string_list(
            doc = """List of test tags to skip.  This attribute contains a list
of tag names; if a test is tagged with one of the tags from this list, it is
skipped.  This can be useful to e.g. skip tests that are flaky or only work in
interactive mode.  Use the `:tags` keyword argument to `ert-deftest` to tag
tests.""",
        ),
    ),
    doc = """Runs ERT tests that are defined in the source files.
The given source files should contain ERT tests defined with `ert-deftest`.
See the [ERT
manual](https://www.gnu.org/software/emacs/manual/html_node/ert/How-to-Write-Tests.html)
for details.  The generated test binary loads all source files and executes all
tests like `ert-run-tests-batch-and-exit`.

You can restrict the tests to be run using the `--test_filter` option.  If set,
the value of `--test_filter` must be a Lisp expression usable as an [ERT test
selector](https://www.gnu.org/software/emacs/manual/html_node/ert/Test-Selectors.html).
You can also restrict the tests to be run using the `skip_tests` and
`skip_tags` rule attributes.  These restrictions are additive, i.e., a test
only runs if it’s not suppressed by either facility.""",
    fragments = ["cpp"],
    test = True,
    toolchains = [
        "@bazel_tools//tools/cpp:toolchain_type",
        _TOOLCHAIN_TYPE,
    ],
    implementation = _binary,
)

def _compile(ctx, srcs, deps, load_path):
    """Byte-compiles Emacs Lisp source files.

    Args:
      ctx (ctx): rule context
      srcs (list of Files): Emacs Lisp sources files to compile
      deps (list of targets): Emacs Lisp libraries that the sources depend on
      load_path (list of strings): additional load path directories, relative
          to the current package

    Returns:
      A structure with the following fields:
        outs: a list of File objects containing the byte-compiled files
        load_path: the load path required to load the compiled files
        runfiles: a runfiles object for the set of input files
        transitive_load_path: the load path required to load the compiled files
            and all their transitive dependencies
        transitive_srcs: a depset of source files for this compilation unit
            and all its transitive dependencies
        transitive_outs: a depset of compiled files for this compilation unit
            and all its transitive dependencies
    """
    outs = []

    # If any file comes for a different package, we can’t place the compiled
    # files adjacent to the source files.  See
    # https://docs.bazel.build/versions/3.1.0/skylark/lib/actions.html#declare_file.
    relocate_output = any([
        src.owner.workspace_name != ctx.label.workspace_name or
        src.owner.package != ctx.label.package
        for src in srcs
    ])

    # Directory relative to the workspace root where outputs should be stored.
    # We prefer storing them adjacent to source files to reduce the number of
    # load path entries, but if necessary, we generate a subdirectory in the
    # current package.
    output_base = (
        paths.join(ctx.label.package, _OUTPUT_DIR) if relocate_output else ""
    )
    resolved_load_path = []
    source_load_path = []
    for dir in ["/"] + load_path:
        if paths.is_absolute(dir):
            dir = "." + dir
        else:
            dir = paths.join(ctx.label.package, check_relative_filename(dir))

        # If we’re compiling source files from another package, we need to
        # insert the output base directory for this rule.  In that case, we
        # still have to append the workspace-relative directory, so that
        # filenames relative to the (relocated) workspace root work.
        dir = check_relative_filename(paths.join(output_base, dir))
        resolved = struct(
            # Actions should load byte-compiled files.  Since we place them
            # into the bin directory, we need to start from there, append the
            # workspace root (see
            # https://docs.bazel.build/versions/2.0.0/skylark/lib/Label.html#workspace_root),
            # and then the directory name relative to the workspace root.  The
            # workspace root will only be nonempty if the current rule lives in
            # a different workspace than the one that Bazel is run from.
            for_actions = check_relative_filename(
                paths.join(ctx.bin_dir.path, ctx.label.workspace_root, dir),
            ),
            # The runfiles tree looks different, see
            # https://docs.bazel.build/versions/2.0.0/output_directories.html.
            # The top-level directories in the runfiles root are always the
            # workspace names, and the load directories are relative to those.
            # The workspace name is the workspace part of the lexical label,
            # see
            # https://docs.bazel.build/versions/2.0.0/skylark/lib/Label.html#workspace_name.
            # Therefore, it can be empty, in which case we need to use the
            # current workspace.
            for_runfiles = check_relative_filename(
                paths.join(ctx.label.workspace_name or ctx.workspace_name, dir),
            ),
        )
        resolved_load_path.append(resolved)
        if len(srcs) > 1:
            # If we have more than one source file, we need to add the
            # respective source directory to the load path for this rule’s
            # actions only, so that the source files can load each other.
            source_load_path.append(check_relative_filename(
                paths.join(ctx.label.workspace_root, dir),
            ))

    indirect_srcs = [
        dep[EmacsLispInfo].transitive_source_files
        for dep in deps
    ]
    indirect_outs = [
        dep[EmacsLispInfo].transitive_compiled_files
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
        transitive = [dep[EmacsLispInfo].transitive_load_path for dep in deps],
    )
    transitive_data = depset(
        direct = ctx.files.data,
        transitive = [
            dep[DefaultInfo].default_runfiles.files
            for dep in ctx.attr.deps
        ],
    )

    toolchain = _toolchain(ctx)
    emacs = toolchain.emacs

    # Expand load path only if needed.
    flat_load_path = [
        _load_directory_for_actions(d)
        for d in transitive_load_path.to_list()
    ] if toolchain.wrap else None

    # We compile only one file per Emacs process.  This might seem wasteful,
    # but since compilation can execute arbitrary code, it ensures that
    # compilation actions don’t interfere with each other.
    for src in srcs:
        if relocate_output:
            out = ctx.actions.declare_file(
                paths.join(_OUTPUT_DIR, src.short_path + "c"),
            )
        else:
            out = ctx.actions.declare_file(src.basename + "c", sibling = src)
        args = []
        inputs = depset(
            # Add all source files as input files so they can load each other
            # if necessary.
            direct = srcs + [ctx.file._compile],
            transitive = indirect_outs + [transitive_data],
        )
        if toolchain.wrap:
            manifest = ctx.actions.declare_file(
                src.basename + ".manifest.json",
                sibling = src,
            )
            ctx.actions.write(
                output = manifest,
                content = struct(
                    root = "EXECUTION_ROOT",
                    loadPath = flat_load_path,
                    inputFiles = [f.path for f in inputs.to_list()],
                    outputFiles = [out.path],
                ).to_json(),
            )
            args += ["--manifest=" + manifest.path, "--"]
            inputs = depset(direct = [manifest], transitive = [inputs])
        args += [
            "--quick",
            "--batch",
            "--load=" + ctx.file._compile.path,
            ctx.actions.args().add_all(
                transitive_load_path,
                map_each = _load_directory_for_actions,
                format_each = "--directory=%s",
                uniquify = True,
                expand_directories = False,
            ).add_all(
                source_load_path,
                format_each = "--directory=%s",
                uniquify = True,
                expand_directories = False,
            ).add_all(
                ["--fatal-warnings"] if ctx.attr.fatal_warnings else [],
            ),
            "--funcall=elisp/compile-batch-and-exit",
            src.path,
            out.path,
        ]
        ctx.actions.run(
            outputs = [out],
            inputs = inputs,
            executable = emacs.files_to_run,
            arguments = args,
            mnemonic = "ElispCompile",
            progress_message = (
                "Compiling Emacs Lisp library {}".format(out.short_path)
            ),
            use_default_shell_env = toolchain.use_default_shell_env,
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

def _cpp_strings(strings):
    """Format the given string list as C++ initializer list."""
    return ", ".join([_cpp_string(s) for s in strings])

def _cpp_string(string):
    """Format the given string as C++ string literal."""

    # Use raw strings and choose a delimiter that’s extremely unlikely to occur
    # in real-world code.
    delim = "#*?&"
    open = 'R"' + delim + "("
    close = ")" + delim + '"'
    if close in string:
        fail("String {} can’t be transferred to C++".format(string))
    return open + string + close

# Directory relative to the current package where to store compiled files.
# This equivalent to _objs for C++ rules.  See
# https://docs.bazel.build/versions/3.1.0/output_directories.html#layout-diagram.
_OUTPUT_DIR = "_elisp"
