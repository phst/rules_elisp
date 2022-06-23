# Copyright 2020, 2021, 2022 Google LLC
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

load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain", "use_cpp_toolchain")
load(
    "//private:defs.bzl",
    "CcDefaultInfo",
    "cc_launcher",
    "check_relative_filename",
    "cpp_strings",
    "run_emacs",
    "runfile_location",
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
the Emacs Lisp source files of this library.""",
        "compiled_files": """A list of `File` objects containing
the byte-compiled Emacs Lisp files and module objects of this library.""",
        "load_path": """A list containing necessary load path
additions for this library.  The list elements are structures as
described in the provider documentation.""",
        "data_files": """A list of `File` objects that this library requires
at runtime.""",
        "transitive_source_files": """A `depset` of `File` objects containing
the Emacs Lisp source files of this library
and all its transitive dependencies.""",
        "transitive_compiled_files": """A `depset` of `File` objects containing
the byte-compiled Emacs Lisp files and module objects of this library
and all its transitive dependencies.""",
        "transitive_load_path": """A `depset` containing necessary load path
additions for this library and all its transitive dependencies.
The `depset` uses preorder traversal: entries for libraries closer to the root
of the dependency graph come first.  The `depset` elements are structures as
described in the provider documentation.""",
    },
)

def _elisp_toolchain_impl(ctx):
    """Rule implementation for the “elisp_toolchain” toolchain rule."""
    return [platform_common.ToolchainInfo(
        emacs = ctx.attr.emacs,
        use_default_shell_env = ctx.attr.use_default_shell_env,
        execution_requirements = ctx.attr.execution_requirements,
        wrap = ctx.attr.wrap,
    )]

# Note: Toolchain names need to be fully qualified, otherwise external
# workspaces won’t find them.

def _elisp_library_impl(ctx):
    """Rule implementation for the “elisp_library” rule."""
    result = _compile(
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

def _elisp_proto_aspect_impl(target, ctx):
    """Aspect implementation for the “elisp_proto_aspect” aspect."""
    info = target[ProtoInfo]
    deps = depset(transitive = [
        depset(dep[EmacsLispInfo].source_files)
        for dep in ctx.rule.attr.deps
    ])
    src = ctx.actions.declare_file(ctx.label.name + ".el")
    ctx.actions.run(
        outputs = [src],
        inputs = [info.direct_descriptor_set],
        executable = ctx.executable._generate,
        arguments = [
            info.direct_descriptor_set.path,
            src.path,
            "//{}:{}".format(ctx.label.package, ctx.label.name),
            _elisp_proto_feature(src),
        ] + [_elisp_proto_feature(d) for d in deps.to_list()],
        mnemonic = "GenElispProto",
        progress_message = "Generating Emacs Lisp protocol buffer library {}".format(src.short_path),
    )
    result = _compile(
        ctx = ctx,
        srcs = [src],
        deps = [ctx.attr._protobuf_lib] + ctx.rule.attr.deps,
        load_path = [],
        data = None,
        tags = ctx.rule.attr.tags,
        fatal_warnings = True,
    )
    return [
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps"],
            extensions = ["el"],
        ),
        EmacsLispInfo(
            source_files = [src],
            compiled_files = result.outs,
            load_path = result.load_path,
            data_files = ctx.rule.files.data,
            transitive_source_files = result.transitive_srcs,
            transitive_compiled_files = result.transitive_outs,
            transitive_load_path = result.transitive_load_path,
        ),
    ]

def _elisp_proto_feature(file):
    """Returns the Emacs feature name for a protocol buffer library."""
    stem, ext = paths.split_extension(file.short_path)
    if ext != ".el":
        fail("invalid extension {}".format(ext))
    if stem.startswith("../"):
        # If the file is from another workspace, its short_path is of the form
        # “../WORKSPACE/PACKAGE/FILE.el”.  Strip off the leading “../WORKSPACE”
        # part.
        stem = stem[3:]
        ws, sep, stem = stem.partition("/")
        if not ws or not sep:
            fail("invalid name {}", file.short_path)
    return stem

def _elisp_proto_library_impl(ctx):
    """Rule implementation for the “elisp_proto_library” rule."""
    deps = ctx.attr.deps
    if len(deps) != 1:
        fail("exactly one proto_library in ‘deps’ required, got {}".format(len(deps)))
    dep = deps[0]

    # All work is done by the ‘elisp_proto_aspect’ aspect.
    info = dep[EmacsLispInfo]
    return [
        DefaultInfo(files = depset(info.source_files)),
        EmacsLispInfo(
            transitive_source_files = info.transitive_source_files,
            transitive_compiled_files = info.transitive_compiled_files,
            transitive_load_path = info.transitive_load_path,
        ),
    ]

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
    executable, runfiles = _binary(
        ctx,
        srcs = ctx.files.src,
        tags = [],
        args = args,
        libs = ctx.attr._binary_libs,
    )
    return [DefaultInfo(
        executable = executable,
        runfiles = runfiles,
    )]

def _elisp_test_impl(ctx):
    """Rule implementation for the “elisp_test” rule."""
    toolchain = ctx.toolchains["@phst_rules_elisp//elisp:toolchain_type"]

    args = [
        "--skip-test=" + test
        for test in ctx.attr.skip_tests
    ] + [
        "--skip-tag=" + tag
        for tag in ctx.attr.skip_tags
    ]
    if ctx.var["COMPILATION_MODE"] != "opt":
        args.append("--module-assertions")
    executable, runfiles = _binary(
        ctx,
        srcs = ctx.files.srcs,
        # “local = 1” is equivalent to adding a “local” tag,
        # cf. https://docs.bazel.build/versions/4.1.0/be/common-definitions.html#test.local.
        tags = ["local"] if ctx.attr.local else [],
        args = args,
        libs = ctx.attr._test_libs,
    )

    # We include the original source files in the runfiles so that error
    # messages in tests can link back to them.
    runfiles = runfiles.merge(ctx.runfiles(files = ctx.files.srcs))

    test_env = {}
    if ctx.configuration.coverage_enabled:
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

        # C/C++ coverage instrumentation needs another binary that wraps gcov;
        # see
        # https://github.com/bazelbuild/bazel/blob/5.0.0/tools/test/collect_coverage.sh#L199.
        # This is normally set from a hidden “$collect_cc_coverage” attribute;
        # see
        # https://github.com/bazelbuild/bazel/blob/5.0.0/src/main/java/com/google/devtools/build/lib/analysis/test/TestActionBuilder.java#L253-L258.
        # We also need to inject its location here, like above.
        runfiles = runfiles.merge(
            ctx.attr._collect_cc_coverage[DefaultInfo].default_runfiles,
        )
        test_env["CC_CODE_COVERAGE_SCRIPT"] = ctx.executable._collect_cc_coverage.path

    # The InstrumentedFilesInfo provider needs to be added here as well as in
    # the “elisp_library” rule for coverage collection to work.
    return [
        DefaultInfo(
            executable = executable,
            runfiles = runfiles,
        ),
        testing.ExecutionInfo(toolchain.execution_requirements),
        testing.TestEnvironment(test_env),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps", "srcs"],
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
        default = "//elisp:compile.elc",
        allow_single_file = [".elc"],
    ),
}

elisp_library = rule(
    attrs = dict(
        _COMPILE_ATTRS,
        srcs = attr.label_list(
            allow_empty = False,
            doc = """List of source files.  These must either be Emacs Lisp
files ending in `.el`, or module objects ending in `.so`, `.dylib`, or
`.dll`.""",
            allow_files = [".el", ".so", ".dylib", ".dll"],
            mandatory = True,
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/09c621e4cf5b968f4c6cdf905ab142d5961f9ddc/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L75.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        outs = attr.output_list(
            doc = """List of byte-compiled Emacs Lisp files to be made available
as targets.""",
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
available to dependencies.  All sources are byte-compiled.
`elisp_library`, `elisp_binary`, and `elisp_test` rules depending on this binary
can then use `load` or `require` to load them.

By default, libraries need to be loaded using a filename relative to the
workspace root, i.e., <var>package</var>/<var>file</var>.  If you want to add
further elements to the load path, use the `load_path` attribute.

If there are multiple source files specified in `srcs`, these source files can
also load each other.  However, it’s often preferable to only have one
`elisp_library` target per source file to make dependencies more obvious and
ensure that files get only loaded in their byte-compiled form.

The source files in `srcs` can also list shared objects.  The rule treats them
as Emacs modules and doesn’t try to byte-compile them.  You can use
e.g. `cc_binary` with `linkshared = True` to create shared objects.""",
    provides = [EmacsLispInfo],
    toolchains = ["@phst_rules_elisp//elisp:toolchain_type"],
    incompatible_use_toolchain_transition = True,
    implementation = _elisp_library_impl,
)

# The protocol buffer aspect is private for now.
_elisp_proto_aspect = aspect(
    doc = "An aspect to generate protocol buffer libraries for Emacs Lisp.",
    attr_aspects = ["deps"],
    attrs = {
        "_compile": attr.label(
            default = "//elisp:compile.elc",
            allow_single_file = [".elc"],
        ),
        "_generate": attr.label(
            default = "//elisp/proto:generate",
            executable = True,
            cfg = "exec",
        ),
        "_protobuf_lib": attr.label(
            default = "//elisp/proto",
            providers = [EmacsLispInfo],
        ),
    },
    required_providers = [ProtoInfo],
    provides = [EmacsLispInfo],
    toolchains = ["@phst_rules_elisp//elisp:toolchain_type"],
    implementation = _elisp_proto_aspect_impl,
)

elisp_proto_library = rule(
    attrs = {
        "deps": attr.label_list(
            doc = "List of exactly one `proto_library` rule.",
            mandatory = True,
            allow_empty = False,
            providers = [ProtoInfo],
            aspects = [_elisp_proto_aspect],
        ),
    },
    doc = r"""Generates Emacs bindings for a protocol buffer library.
By convention, for a `proto_library` rule named
<code><var>prefix</var>\_proto</code> there should be a corresponding
`elisp_proto_library` rule named <code><var>prefix</var>\_elisp\_proto</code>.
Other `elisp_library`, `elisp_binary`, and `elisp_test` rules can then depend
on this rule.  This rule generates and byte-compiles Emacs Lisp representations
of the protocol buffer definitions listed in the `deps` attribute and all their
direct and indirect dependencies.  The feature symbol for `require` is
<code><var>package</var>/<var>name</var></code>, where
<code>//<var>package</var>:<var>name</var></code> is the label of the
corresponding `proto_library` rule.""",
    provides = [EmacsLispInfo],
    toolchains = ["@phst_rules_elisp//elisp:toolchain_type"],
    incompatible_use_toolchain_transition = True,
    implementation = _elisp_proto_library_impl,
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
        _grep_includes = attr.label(
            allow_single_file = True,
            executable = True,
            cfg = "exec",
            default = Label("@bazel_tools//tools/cpp:grep-includes"),
        ),
        _binary_libs = attr.label_list(
            default = ["//elisp:binary"],
            providers = [CcInfo],
        ),
        _template = attr.label(
            default = "//elisp:binary.template",
            allow_single_file = [".template"],
        ),
        _launcher_defaults = attr.label(
            default = "//elisp:launcher_defaults",
            providers = [CcDefaultInfo],
        ),
        data = attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        deps = attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
        interactive = attr.bool(
            doc = "Run Emacs in interactive instead of batch mode.",
        ),
        input_args = attr.int_list(
            doc = """Indices of command-line arguments that represent input
filenames.  These number specify indices into the `argv` array.  Negative
indices are interpreted as counting from the end of the array.  For example,
the index `2` stands for `argv[2]`, and the index `-2` stands for
`argv[argc - 2]`.  When passing arguments to an `emacs_binary` program on the
command line, the corresponding arguments are treated as filenames for input
files and added to the `inputFiles` field of the manifest.  This only has an
effect for toolchains that specify `wrap = True`.""",
        ),
        output_args = attr.int_list(
            doc = """Indices of command-line arguments that represent output
filenames.  These number specify indices into the `argv` array.  Negative
indices are interpreted as counting from the end of the array.  For example,
the index `2` stands for `argv[2]`, and the index `-2` stands for
`argv[argc - 2]`.  When passing arguments to an `emacs_binary` program on the
command line, the corresponding arguments are treated as filenames for output
files and added to the `outputFiles` field of the manifest.  This only has an
effect for toolchains that specify `wrap = True`.""",
        ),
    ),
    doc = """Binary rule that loads a single Emacs Lisp file.
The source file is byte-compiled.  At runtime, the compiled version is loaded
in batch mode unless `interactive` is `True`.""",
    executable = True,
    fragments = ["cpp"],
    toolchains = use_cpp_toolchain() + [
        "@phst_rules_elisp//elisp:toolchain_type",
    ],
    incompatible_use_toolchain_transition = True,
    implementation = _elisp_binary_impl,
)

elisp_test = rule(
    attrs = dict(
        _COMPILE_ATTRS,
        srcs = attr.label_list(
            allow_empty = False,
            doc = "List of source files to load.",
            allow_files = [".el"],
            mandatory = True,
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/09c621e4cf5b968f4c6cdf905ab142d5961f9ddc/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L75.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        _cc_toolchain = attr.label(
            default = "@bazel_tools//tools/cpp:current_cc_toolchain",
            providers = [cc_common.CcToolchainInfo],
        ),
        _grep_includes = attr.label(
            allow_single_file = True,
            executable = True,
            cfg = "exec",
            default = Label("@bazel_tools//tools/cpp:grep-includes"),
        ),
        _test_libs = attr.label_list(
            default = ["//elisp:test"],
            providers = [CcInfo],
        ),
        _template = attr.label(
            default = "//elisp:test.template",
            allow_single_file = [".template"],
        ),
        _launcher_defaults = attr.label(
            default = "//elisp:launcher_defaults",
            providers = [CcDefaultInfo],
        ),
        _lcov_merger = attr.label(
            default = "@bazel_tools//tools/test:lcov_merger",
            executable = True,
            cfg = "target",
        ),
        _collect_cc_coverage = attr.label(
            default = "@bazel_tools//tools/test:collect_cc_coverage",
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
only runs if it’s not suppressed by either facility.

In coverage mode (i.e., when run under `bazel coverage`), all tests tagged with
the `:nocover` tag are also skipped.  You can use this tag to skip tests that
normally pass, but don’t work under coverage for some reason.""",
    fragments = ["cpp"],
    test = True,
    toolchains = use_cpp_toolchain() + [
        "@phst_rules_elisp//elisp:toolchain_type",
    ],
    incompatible_use_toolchain_transition = True,
    implementation = _elisp_test_impl,
)

def _elisp_manual_impl(ctx):
    """Rule implementation for the “elisp_manual” rule."""
    src = ctx.file.src
    out = ctx.outputs.out
    if out.extension != "texi":
        fail("Output filename {} doesn’t end in “.texi”".format(out.short_path))
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [ctx.attr._export])
    ctx.actions.run(
        outputs = [out],
        inputs = depset(direct = [src], transitive = [tool_inputs]),
        executable = ctx.executable._export,
        arguments = [src.path, out.path],
        mnemonic = "Export",
        progress_message = "Exporting {} into Texinfo file".format(src.short_path),
        input_manifests = input_manifests,
    )

elisp_manual = rule(
    attrs = {
        "src": attr.label(
            doc = "Org-mode file to use as manual source; must end in `.org`.",
            allow_single_file = [".org"],
            mandatory = True,
        ),
        "out": attr.output(
            doc = "Texinfo manual file to write; must end in `.texi`.",
            mandatory = True,
        ),
        "_export": attr.label(
            allow_single_file = True,
            executable = True,
            cfg = "exec",
            default = Label("//elisp:export_org"),
        ),
    },
    doc = """Generates a [GNU Texinfo](https://www.gnu.org/software/texinfo/)
manual from an [Org Mode file](https://orgmode.org/) using
[Org’s exporting functionality](https://orgmode.org/manual/Exporting.html).
You can then use
[`texi2any`](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Generic-Translator-texi2any.html)
to generate other document formats from the output file.""",
    implementation = _elisp_manual_impl,
)

def _compile(ctx, srcs, deps, load_path, data, tags, fatal_warnings):
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
    # https://docs.bazel.build/versions/4.1.0/skylark/lib/actions.html#declare_file.
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
            source_load_path.append(check_relative_filename(
                paths.join(ctx.label.workspace_root, dir),
            ))

        # At least some of the sources must be reachable from the directory.
        prefix = "./" if dir == "." else "./" + dir + "/"
        if not any([("./" + src.short_path).startswith(prefix) for src in srcs]):
            fail("None of the files [{}] are reachable from load path directory {}"
                .format(", ".join([src.short_path for src in srcs]), dir))

        # If we’re compiling source files from another package, we need to
        # insert the output base directory for this rule.  In that case, we
        # still have to append the workspace-relative directory, so that
        # filenames relative to the (relocated) workspace root work.
        dir = check_relative_filename(paths.join(output_base, dir))
        resolved = struct(
            # Actions should load byte-compiled files.  Since we place them into
            # the bin directory, we need to start from there, append the
            # workspace root (see
            # https://docs.bazel.build/versions/4.1.0/skylark/lib/Label.html#workspace_root),
            # and then the directory name relative to the workspace root.  The
            # workspace root will only be nonempty if the current rule lives in
            # a different workspace than the one that Bazel is run from.  This
            # approach also works for dynamic modules placed in the bin
            # directory.
            for_actions = check_relative_filename(
                paths.join(ctx.bin_dir.path, ctx.label.workspace_root, dir),
            ),
            # The runfiles tree looks different, see
            # https://docs.bazel.build/versions/4.1.0/output_directories.html.
            # The top-level directories in the runfiles root are always the
            # workspace names, and the load directories are relative to those.
            # The workspace name is the workspace part of the lexical label, see
            # https://docs.bazel.build/versions/4.1.0/skylark/lib/Label.html#workspace_name.
            # Therefore, it can be empty, in which case we need to use the
            # current workspace.
            for_runfiles = check_relative_filename(
                paths.join(ctx.label.workspace_name or ctx.workspace_name, dir),
            ),
        )
        resolved_load_path.append(resolved)

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

    toolchain = ctx.toolchains["@phst_rules_elisp//elisp:toolchain_type"]

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
                    paths.replace_extension(src.short_path, ".elc"),
                ),
            ) if relocate_output else ctx.actions.declare_file(
                paths.replace_extension(src.basename, ".elc"),
                sibling = src,
            )
        )
        inputs = depset(
            # Add all source files as input files so they can load each other
            # if necessary.
            direct = srcs + [ctx.file._compile],
            transitive = indirect_outs + [transitive_data],
        )
        args = [
            "--load=" + ctx.file._compile.path,
            ctx.actions.args().add_all(
                # We don’t add the full transitive load path here because the
                # direct load path would only contain the file to be compiled.
                depset(order = "preorder", transitive = indirect_load_path),
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
                ["--fatal-warnings"] if fatal_warnings else [],
            ),
            "--funcall=elisp/compile-batch-and-exit",
            src.path,
            out.path,
        ]
        run_emacs(
            ctx = ctx,
            outputs = [out],
            inputs = inputs,
            arguments = args,
            tags = tags,
            mnemonic = "ElispCompile",
            progress_message = "Compiling {}".format(src.short_path),
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

def _binary(ctx, srcs, tags, args, libs):
    """Shared implementation for the “elisp_binary” and “elisp_test” rules.

    The rule should define a “_template” attribute containing the C++ template
    file to be expanded.

    Args:
      ctx: rule context
      srcs: list of File objects denoting the source files to load
      tags: list of strings with additional rule-specific tags
      args: a list of rule-specific program arguments
      libs (list of Targets): `cc_library` targets to be added as dependencies

    Returns:
      a pair (executable, runfiles) containing the compiled binary and the
          runfiles it needs.
    """
    result = _compile(
        ctx = ctx,
        srcs = srcs,
        deps = ctx.attr.deps,
        load_path = [],
        data = ctx.files.data,
        tags = ctx.attr.tags,
        fatal_warnings = ctx.attr.fatal_warnings,
    )
    toolchain = ctx.toolchains["@phst_rules_elisp//elisp:toolchain_type"]
    emacs = toolchain.emacs

    # Only pass in data files when needed.
    data_files_for_manifest = (
        result.runfiles.files.to_list() if toolchain.wrap else []
    )

    # If we’re supposed to generate coverage information, use source files in
    # addition to compiled files because we can’t instrument compiled files for
    # coverage.  We ignore ctx.coverage_instrumented because that doesn’t work
    # here: it assumes that coverage is generated during compilation, but we can
    # generate coverage information only at runtime.  Bazel’s coverage support
    # isn’t really documented; some information is available at
    # https://github.com/bazelbuild/bazel/blob/3.7.0/CODEBASE.md#coverage-collection
    # and in the source code comments of the file
    # https://github.com/bazelbuild/bazel/blob/3.0.0/src/main/java/com/google/devtools/build/lib/bazel/coverage/CoverageReportActionBuilder.java.
    # When runtime coverage support is enabled, Bazel writes a list of filenames
    # that are covered by --instrumentation_filter to a text file whose filename
    # is in COVERAGE_MANIFEST.  The test runner then parses that file and only
    # instruments the files that should be instrumented.  This saves time and
    # increases robustness.  Because we might load some combination of source
    # and compiled files, always supply both files at runtime.
    transitive_files = (
        depset(transitive = [
            result.transitive_srcs,
            result.transitive_outs,
        ]) if ctx.configuration.coverage_enabled else result.transitive_outs
    )

    # When collecting coverage information, the ERT test runner needs a way to
    # determine whether or not to instrument a file.  In particular, we should
    # avoid instrumenting files that are part of Emacs, because instrumenting
    # them often causes spurious errors and likely makes the run much slower.
    # To distinguish between files to instrument and other files, we employ the
    # following trick: For each file to be instrumented, we create an adjacent
    # symbolic link with the same name, but a magic “.instrument” extension.
    # The test runner can then use the presence of these links to make the
    # decision.  The type and target of the file is actually irrelevant;
    # creating a symbolic link is just the easiest approach because we can use
    # Bazel’s “root_symlinks” feature.  We could rely solely on the
    # COVERAGE_MANIFEST file (see above), however, that would increase the
    # complexity of the test runner because it would have to find the correct
    # compiled file for each source file.  Using the magic extension allows us
    # to use the ‘load-suffixes’ variable for this purpose.
    links = {
        runfile_location(ctx, src) + ".instrument": src
        for src in result.transitive_srcs.to_list()
    } if ctx.configuration.coverage_enabled else {}

    # We use a C++ launcher because the C++ toolchain framework exposes
    # individual actions (unlike Python), and the runfiles implementation
    # doesn’t have bugs (unlike Go).  We use raw strings to minimize the risk
    # of misinterpreting special characters in a filename.
    # check_relative_filename should already reject all special characters, but
    # better be sure.
    launcher_src = ctx.actions.declare_file("_" + ctx.label.name + ".cc")
    ctx.actions.expand_template(
        template = ctx.file._template,
        output = launcher_src,
        substitutions = {
            "[[args]]": cpp_strings([
                "--wrapper=" + runfile_location(ctx, emacs.files_to_run.executable),
                "--mode=" + ("wrap" if toolchain.wrap else "direct"),
            ] + [
                "--rule-tag=" + tag
                for tag in collections.uniq(ctx.attr.tags + tags)
            ] + [
                "--load-directory=" + check_relative_filename(dir.for_runfiles)
                for dir in result.transitive_load_path.to_list()
            ] + [
                "--load-file=" + runfile_location(ctx, src)
                for src in result.outs
            ] + [
                "--data-file=" + runfile_location(ctx, file)
                for file in data_files_for_manifest
            ] + args),
        },
    )
    cc_toolchain = find_cpp_toolchain(ctx)
    executable, launcher_runfiles = cc_launcher(
        ctx,
        cc_toolchain,
        launcher_src,
        libs,
    )
    bin_runfiles = ctx.runfiles(
        files = [emacs.files_to_run.executable] + result.outs,
        transitive_files = depset(
            transitive = [transitive_files, result.runfiles.files],
        ),
        root_symlinks = links,
    )
    emacs_runfiles = emacs.default_runfiles
    runfiles = bin_runfiles.merge(emacs_runfiles).merge(launcher_runfiles)
    return executable, runfiles

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
# https://docs.bazel.build/versions/4.1.0/output_directories.html#layout-diagram.
_OUTPUT_DIR = "_elisp"
