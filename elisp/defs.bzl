# Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

load("@bazel_features//:features.bzl", "bazel_features")
load("@bazel_skylib//lib:collections.bzl", "collections")
load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain", "use_cpp_toolchain")
load("@rules_proto//proto:defs.bzl", "ProtoInfo", "proto_common")
load(
    "//private:defs.bzl",
    "CcDefaultInfo",
    "LAUNCHER_ATTRS",
    "LAUNCHER_DEPS",
    "ModuleConfigInfo",
    "cc_launcher",
    "check_relative_filename",
    "repository_relative_filename",
    "run_emacs",
    "runfile_location",
)

visibility("public")

EmacsLispInfo = provider(
    doc = """Provider for Emacs Lisp libraries.
The `elisp_library`, `elisp_proto_library`, and `elisp_cc_module` rules
produce this provider.

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
        "package_file": """A `File` object for the -pkg.el file.
None if `enable_package` is False.""",
        "metadata_file": """A `File` object for the metadata file.
None if `enable_package` is False.""",
        "autoloads_file": """A `File` object for the autoloads file.
None if `enable_package` is False.""",
    },
)

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
    extra_out = []
    package_file = None
    metadata_file = None
    autoloads_file = None
    if ctx.attr.enable_package and not ctx.attr.testonly:
        pkg = _build_package(ctx, ctx.files.srcs, ctx.files.data)
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

def _elisp_proto_aspect_impl(target, ctx):
    """Aspect implementation for the “elisp_proto_aspect” aspect."""
    info = target[ProtoInfo]
    srcs = proto_common.declare_generated_files(ctx.actions, info, ".proto.el")
    proto_common.compile(
        actions = ctx.actions,
        proto_info = info,
        proto_lang_toolchain_info = ctx.attr._proto_toolchain[proto_common.ProtoLangToolchainInfo],
        generated_files = srcs,
    )

    # TODO: We probably shouldn’t generate this bundle, but right now it’s
    # needed for compatibility.
    bundle = ctx.actions.declare_file(ctx.label.name + ".el")
    args = ctx.actions.args()
    args.add(bundle)
    args.add(str(ctx.label))
    args.add(_elisp_proto_feature(bundle))
    args.add_all(srcs, map_each = _elisp_proto_feature, uniquify = True, expand_directories = False)
    ctx.actions.run(
        outputs = [bundle],
        executable = ctx.executable._generate_bundle,
        arguments = [args],
        mnemonic = "GenElispProtoBundle",
        progress_message = "Generating Emacs Lisp protocol buffer bundle %{output}",
        toolchain = None,
    )

    load_dir = info.proto_source_root.removeprefix(ctx.bin_dir.path + "/")
    if load_dir.startswith("external/"):
        _, _, load_dir = load_dir.partition("/")
        _, _, load_dir = load_dir.partition("/")
    load_dir = "/" + check_relative_filename(load_dir)
    load_path = [load_dir]
    if ctx.label.package == "src/google/protobuf":
        # See the comment in _elisp_proto_feature why we’re doing this.
        load_path.append(".")
    result = _compile(
        ctx = ctx,
        srcs = srcs + [bundle],
        deps = [ctx.attr._protobuf_lib] + ctx.rule.attr.deps,
        load_path = load_path,
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
            source_files = srcs + [bundle],
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
    stem, ext = paths.split_extension(repository_relative_filename(file))
    if ext != ".el":
        fail("invalid extension {}".format(ext))

    # Strip “virtual import” prefix.
    _, _, after = stem.partition("/_virtual_imports/")
    if after:
        _, _, stem = after.partition("/")

    # Work around https://github.com/bazelbuild/bazel/issues/11044 for built-in
    # types.
    # FIXME: It would probably better to keep the “google/protobuf/” prefix and
    # fix users instead.
    return stem.removeprefix("src/google/protobuf/")

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
    )
    return [
        DefaultInfo(
            executable = executable,
            runfiles = runfiles,
        ),
    ]

def _elisp_test_impl(ctx):
    """Rule implementation for the “elisp_test” rule."""
    toolchain = ctx.toolchains[Label("//elisp:toolchain_type")]

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
        # cf. https://bazel.build/reference/be/common-definitions#test.local.
        tags = ["local"] if ctx.attr.local else [],
        args = args,
    )

    # We include the original source files in the runfiles so that error
    # messages in tests can link back to them.
    runfiles = runfiles.merge(ctx.runfiles(files = ctx.files.srcs))

    test_env = {}
    if ctx.configuration.coverage_enabled:
        # Bazel’s coverage runner
        # (https://github.com/bazelbuild/bazel/blob/6.4.0/tools/test/collect_coverage.sh)
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
        # https://github.com/bazelbuild/bazel/blob/6.4.0/tools/test/collect_coverage.sh#L183.
        # This is normally set from a hidden “$collect_cc_coverage” attribute;
        # see
        # https://github.com/bazelbuild/bazel/blob/6.4.0/src/main/java/com/google/devtools/build/lib/analysis/test/TestActionBuilder.java#L256-L261.
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

# Compilation-related attributes shared between elisp_library, elisp_binary,
# and elisp_test.
_COMPILE_ATTRS = LAUNCHER_ATTRS | {
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
        default = Label("//elisp:compile.elc"),
        allow_single_file = [".elc"],
    ),
}

elisp_library = rule(
    attrs = _COMPILE_ATTRS | {
        "srcs": attr.label_list(
            allow_empty = False,
            doc = """List of source files.  These must either be Emacs Lisp
files ending in `.el`, or module objects ending in `.so`, `.dylib`, or
`.dll`.""",
            allow_files = [".el", ".so", ".dylib", ".dll"],
            mandatory = True,
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/6.4.0/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L73.
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

# The protocol buffer aspect is private for now.
_elisp_proto_aspect = aspect(
    doc = "An aspect to generate protocol buffer libraries for Emacs Lisp.",
    attr_aspects = ["deps"],
    attrs = {
        "_compile": attr.label(
            default = Label("//elisp:compile.elc"),
            allow_single_file = [".elc"],
        ),
        "_generate_bundle": attr.label(
            default = Label("//elisp/proto:generate_bundle"),
            executable = True,
            cfg = "exec",
        ),
        "_protobuf_lib": attr.label(
            default = Label("//elisp/proto"),
            providers = [EmacsLispInfo],
        ),
        "_proto_toolchain": attr.label(
            default = Label("//elisp/proto:toolchain"),
            providers = [proto_common.ProtoLangToolchainInfo],
        ),
    },
    required_providers = [ProtoInfo],
    provides = [EmacsLispInfo],
    toolchains = [Label("//elisp:toolchain_type")],
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
    toolchains = [Label("//elisp:toolchain_type")],
    implementation = _elisp_proto_library_impl,
)

def _elisp_cc_module_impl(ctx):
    """Implementation of the `elisp_cc_module` rule."""
    cc_toolchain = find_cpp_toolchain(ctx)
    deps = ctx.attr.deps + [ctx.attr._module_header]
    infos = [dep[CcInfo] for dep in deps]
    defaults = ctx.attr._module_config[CcDefaultInfo]
    config = ctx.attr._module_config[ModuleConfigInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = defaults.features + ctx.features,
        unsupported_features = defaults.disabled_features + ctx.disabled_features,
    )
    _, objs = cc_common.compile(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        srcs = ctx.files.srcs,
        compilation_contexts = [info.compilation_context for info in infos],
        local_defines = defaults.defines + ctx.attr.local_defines,
        user_compile_flags = defaults.copts + ctx.attr.copts,
    )
    out = cc_common.link(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = objs,
        linking_contexts = [info.linking_context for info in infos],
        output_type = "dynamic_library",
        user_link_flags = defaults.linkopts + ctx.attr.linkopts,
        additional_inputs = config.additional_linker_inputs,
    )
    if not (out.library_to_link and out.library_to_link.dynamic_library):
        fail("linking Emacs module didn’t produce a dynamic library")

    # Ensure that the library file has the expected name.
    filename = ctx.label.name + config.suffix
    if out.library_to_link.dynamic_library.basename == filename:
        lib = out.library_to_link.dynamic_library
    else:
        lib = ctx.actions.declare_file(filename)
        ctx.actions.symlink(
            output = lib,
            target_file = out.library_to_link.dynamic_library,
            progress_message = "Creating symbolic link " + lib.short_path,
        )

    # Replicate some implementation details of cc_binary to make coverage work,
    # at least with llvm-cov.  Do this only in Bazel 7, because Bazel 6 doesn’t
    # support the metdata_files parameter for
    # coverage_common.instrumented_files_info.  See
    # https://github.com/bazelbuild/bazel/issues/15974.
    instrumented_files_info_kwargs = {}
    if (bazel_features.rules.instrumented_files_info_has_metadata_files and
        ctx.configuration.coverage_enabled and ctx.coverage_instrumented()):
        # @bazel_tools//tools/test:collect_cc_coverage.sh requires a file whose
        # name ends in “runtime_objects_list.txt”.
        objects_list = ctx.actions.declare_file(ctx.label.name + ".runtime_objects_list.txt")
        ctx.actions.write(objects_list, lib.path + "\n")
        instrumented_files_info_kwargs["metadata_files"] = [lib, objects_list]

    load_path = [_resolve_load_path(ctx, "")]
    return [
        DefaultInfo(
            files = depset([lib]),
            runfiles = ctx.runfiles(ctx.files.data).merge_all([
                dep[DefaultInfo].default_runfiles
                for dep in ctx.attr.deps
            ]),
        ),
        EmacsLispInfo(
            source_files = [lib],
            compiled_files = [lib],
            load_path = load_path,
            data_files = ctx.files.data,
            transitive_source_files = depset([lib]),
            transitive_compiled_files = depset([lib]),
            transitive_load_path = depset(load_path),
        ),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps"],
            **instrumented_files_info_kwargs
        ),
    ]

elisp_cc_module = rule(
    doc = """Builds an Emacs dynamic module from C and C++ source files.
For background on Emacs modules, see
[Emacs Dynamic Modules](<info:elisp#Dynamic Modules>), and see
[Writing Dynamically-Loaded Modules](<info:elisp#Writing Dynamic Modules>).
The `emacs_module` rule is similar to
[`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library),
but it always builds a dynamic library (shared object) that can be loaded
into Emacs.  The module can also be used directly as dependency
for `elisp_library`, `elisp_binary`, and `elisp_test` rules.
The dynamic library will only export the symbols necessary for Emacs modules,
`plugin_is_GPL_compatible` and `emacs_module_init`; see
[Module Initialization Code](<info:elisp#Module Initialization>).
The file name of the dynamic library will be
<code><var>name</var>.<var>extension</var></code>, where <var>name</var>
is the rule name as specified in the `name` attribute, and <var>extension</var>
is the correct extension for dynamic libraries for the target operating system.
The file name will not be prefixed with `lib`.

The library code in `srcs` can include a recent version of `emacs-module.h`
using

```c
#include "emacs-module.h"
```

to implement module functions.""",
    attrs = {
        "srcs": attr.label_list(
            doc = """C and C++ source files for the module.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.srcs).""",
            allow_files = [".cc", ".c"],
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/6.4.0/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L73.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        "deps": attr.label_list(
            doc = """`cc_library` targets that the module depends on.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.deps).""",
            providers = [CcInfo],
        ),
        "data": attr.label_list(
            doc = """Data dependencies to make available at runtime.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.data).""",
            allow_files = True,
        ),
        "copts": attr.string_list(
            doc = """Additional options to pass to the C/C++ compiler.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.copts).""",
        ),
        "linkopts": attr.string_list(
            doc = """Additional options to pass to the C/C++ linker.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.linkopts).""",
        ),
        "local_defines": attr.string_list(
            doc = """Additional preprocessor definitions to pass to the
C/C++ compiler.  See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.local_defines).""",
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_module_header": attr.label(
            default = Label("//emacs:module_header"),
            providers = [CcInfo],
        ),
        "_module_config": attr.label(
            default = Label("//elisp:module_config"),
            providers = [CcDefaultInfo, ModuleConfigInfo],
        ),
    },
    provides = [EmacsLispInfo],
    fragments = ["cpp"],
    toolchains = use_cpp_toolchain(),
    implementation = _elisp_cc_module_impl,
)

elisp_binary = rule(
    attrs = _COMPILE_ATTRS | {
        "src": attr.label(
            doc = "Source file to load.",
            allow_single_file = [".el"],
            mandatory = True,
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_launcher_deps": attr.label_list(
            default = LAUNCHER_DEPS + [Label("//elisp:binary")],
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
    toolchains = use_cpp_toolchain() + [Label("//elisp:toolchain_type")],
    implementation = _elisp_binary_impl,
)

elisp_test = rule(
    attrs = _COMPILE_ATTRS | {
        "srcs": attr.label_list(
            allow_empty = False,
            doc = "List of source files to load.",
            allow_files = [".el"],
            mandatory = True,
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/6.4.0/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L73.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_launcher_deps": attr.label_list(
            default = LAUNCHER_DEPS + [Label("//elisp:test")],
            providers = [CcInfo],
        ),
        # Magic coverage attributes.  This is only partially documented
        # (https://bazel.build/rules/lib/coverage#output_generator), but we can
        # take over the values from
        # https://github.com/bazelbuild/bazel/blob/7.0.0-pre.20231018.3/src/main/starlark/builtins_bzl/common/python/py_test_bazel.bzl.
        "_lcov_merger": attr.label(
            default = configuration_field("coverage", "output_generator"),
            executable = True,
            cfg = "exec",
        ),
        "_collect_cc_coverage": attr.label(
            default = Label("@bazel_tools//tools/test:collect_cc_coverage"),
            executable = True,
            cfg = "exec",
        ),
        "data": attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        "deps": attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
        "skip_tests": attr.string_list(
            doc = """List of tests to skip.  This attribute contains a list of
ERT test symbols; when running the test rule, these tests are skipped.

Most of the time, you should use the `skip-unless` macro instead; see
[Tests and Their Environment](<info:ert#Tests and Their Environment>).
The `skip_tests` attribute is mainly useful for third-party code that
you don’t control.""",
        ),
        "skip_tags": attr.string_list(
            doc = """List of test tags to skip.  This attribute contains a list
of tag names; if a test is tagged with one of the tags from this list, it is
skipped.  This can be useful to e.g. skip tests that are flaky or only work in
interactive mode.  Use the `:tags` keyword argument to `ert-deftest` to tag
tests.""",
        ),
    },
    doc = """Runs ERT tests that are defined in the source files.
The given source files should contain ERT tests defined with `ert-deftest`.
For details, see [How to Write Tests](<info:ert#How to Write Tests>).
The generated test binary loads all source files and executes all
tests like `ert-run-tests-batch-and-exit`.

You can restrict the tests to be run using the `--test_filter` option.  If set,
the value of `--test_filter` must be a Lisp expression usable as an
ERT test selector; see [Test Selectors](<info:ert#Test Selectors>).
You can also restrict the tests to be run using the `skip_tests` and
`skip_tags` rule attributes.  These restrictions are additive, i.e., a test
only runs if it’s not suppressed by either facility.

In coverage mode (i.e., when run under `bazel coverage`), all tests tagged with
the `:nocover` tag are also skipped.  You can use this tag to skip tests that
normally pass, but don’t work under coverage for some reason.""",
    fragments = ["cpp"],
    test = True,
    toolchains = use_cpp_toolchain() + [Label("//elisp:toolchain_type")],
    implementation = _elisp_test_impl,
)

def _elisp_manual_impl(ctx):
    """Rule implementation for the “elisp_manual” rule."""
    src = ctx.file.src
    out = ctx.outputs.out
    if out.extension != "texi":
        fail("Output filename {} doesn’t end in “.texi”".format(out.short_path))
    ctx.actions.run(
        outputs = [out],
        inputs = [src],
        executable = ctx.executable._export,
        arguments = [ctx.actions.args().add(src).add(out)],
        mnemonic = "Export",
        progress_message = "Exporting %{input} into Texinfo file",
        toolchain = None,
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
    doc = """Generates a GNU Texinfo manual from an Org Mode file.
See [GNU Texinfo](info:texinfo), and see [the Org Mode manual](info:org).
Uses Org’s exporting functionality; see [Exporting](info:org#Exporting).
You can then use `texi2any` to generate other document formats from the output
file; see [`texi2any`](<info:texinfo#Generic Translator texi2any>).""",
    implementation = _elisp_manual_impl,
)

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

def _compile(ctx, *, srcs, deps, load_path, data, tags, fatal_warnings):
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
        resolved_load_path.append(_resolve_load_path(ctx, dir))

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
        args.add(ctx.file._compile, format = "--load=%s")
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
        if fatal_warnings:
            args.add("--fatal-warnings")
        args.add("--funcall=elisp/compile-batch-and-exit")
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

def _binary(ctx, *, srcs, tags, args):
    """Shared implementation for the “elisp_binary” and “elisp_test” rules.

    The rule should define a “_launcher_srcs” attribute containing the main C++
    program source file.

    Args:
      ctx: rule context
      srcs: list of File objects denoting the source files to load
      tags: list of strings with additional rule-specific tags
      args: a list of rule-specific program arguments

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
    toolchain = ctx.toolchains[Label("//elisp:toolchain_type")]
    emacs = toolchain.emacs[DefaultInfo]

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
    # https://bazel.build/contribute/codebase#coverage-collection and in the
    # source code comments of the file
    # https://github.com/bazelbuild/bazel/blob/6.4.0/src/main/java/com/google/devtools/build/lib/bazel/coverage/CoverageReportActionBuilder.java.
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
    executable, launcher_runfiles = cc_launcher(
        ctx,
        header = "elisp/binary.h",
        args = [
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
        ] + args,
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

def _resolve_load_path(ctx, dir):
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

def _get_emacs_package_name(ctx):
    """Returns the package name to use for `elisp_library' rules."""
    if ctx.attr.emacs_package_name:
        return ctx.attr.emacs_package_name
    if len(ctx.files.srcs) != 1:
        return ctx.label.name.replace("_", "-")
    basename = ctx.files.srcs[0].basename
    if not basename.endswith(".el"):
        fail("Suspicious single file when guessing package_name for target", ctx.label)
    if basename.endswith("-pkg.el"):
        fail("Suspicious package_name derived from single source file for target", ctx.label)
    return basename[:-len(".el")]

def _build_package(ctx, srcs, data):
    """Build package files.

    Args:
      ctx (ctx): rule context
      srcs (list of Files): Emacs Lisp sources files
      data (list of Files): data files

    Returns:
      A structure with the following fields:
        package_file: the File object for the -pkg.el file
        metadata_file: the File object containing the package metadata
        autoloads_file: the File object for the autoloads file
    """
    package_name = _get_emacs_package_name(ctx)

    pkg_file = None

    # Try to find an existing -pkg.el file
    expected = package_name + "-pkg.el"
    for file in srcs + data:
        if file.basename == expected:
            pkg_file = file
            break

    # Generate a -pkg.el file
    if pkg_file == None:
        expected = package_name + ".el"
        for file in srcs + data:
            if file.basename == expected:
                pkg_file = _generate_pkg_el(ctx, file)
                break
    if pkg_file == None:
        fail("No package metadata found for target", ctx.label)

    # Try to find an existing autoloads file
    autoloads_file = None
    expected = package_name + "-autoloads.el"
    for file in srcs + data:
        if file.basename == expected:
            autoloads_file = file
            break
    if autoloads_file == None:
        autoloads_file = _generate_autoloads(ctx, package_name, srcs)
    metadata_file = _generate_metadata(ctx, pkg_file)
    return struct(
        package_file = pkg_file,
        metadata_file = metadata_file,
        autoloads_file = autoloads_file,
    )

def _generate_pkg_el(ctx, src):
    """Generate -pkg.el file.

    Args:
      ctx (ctx): rule context
      src (File): Emacs Lisp source file to parse for package metadata

    Returns:
      the File object for the -pkg.el file
    """
    package_name = src.basename.rsplit(".")[0]
    out = ctx.actions.declare_file(paths.join(
        _OUTPUT_DIR,
        ctx.attr.name,
        "{}-pkg.el".format(package_name),
    ))
    inputs = depset(direct = [src, ctx.file._gen_pkg_el])
    run_emacs(
        ctx = ctx,
        arguments = [
            "--load=" + ctx.file._gen_pkg_el.path,
            "--funcall=elisp/gen-pkg-el-and-exit",
            src.path,
            out.path,
        ],
        inputs = inputs,
        outputs = [out],
        tags = ctx.attr.tags,
        mnemonic = "GenPkgEl",
        progress_message = "Generating -pkg.el {}".format(out.short_path),
        manifest_basename = out.basename,
        manifest_sibling = out,
    )
    return out

def _generate_metadata(ctx, package_file):
    """Generate metadata file.

    Args:
      ctx (ctx): rule context
      package_file (File): the File object for the -pkg.el file

    Returns:
      The File object for the metadata file
    """
    if not package_file.basename.endswith("-pkg.el"):
        fail("Unexpected package_file", package_file)
    package_name = package_file.basename[:-len("-pkg.el")]
    out = ctx.actions.declare_file(paths.join(_OUTPUT_DIR, ctx.attr.name, "{}.json".format(package_name)))
    inputs = depset(direct = [package_file, ctx.file._gen_metadata])
    run_emacs(
        ctx = ctx,
        arguments = [
            "--load=" + ctx.file._gen_metadata.path,
            "--funcall=elisp/gen-metadata-and-exit",
            package_file.path,
            out.path,
        ],
        inputs = inputs,
        outputs = [out],
        tags = ctx.attr.tags,
        mnemonic = "GenMetadata",
        progress_message = "Generating metadata {}".format(out.short_path),
        manifest_basename = out.basename,
        manifest_sibling = out,
    )
    return out

def _generate_autoloads(ctx, package_name, srcs):
    """Generate autoloads file.

    Args:
      ctx (ctx): rule context
      package_name (string): name of package
      srcs (list of Files): Emacs Lisp source files for which to generate autoloads

    Returns:
      The generated File.
    """
    out = ctx.actions.declare_file(paths.join(_OUTPUT_DIR, ctx.attr.name, "{}-autoloads.el".format(package_name)))
    inputs = depset(direct = srcs + [ctx.file._gen_autoloads])
    run_emacs(
        ctx = ctx,
        arguments = [
            "--load=" + ctx.file._gen_autoloads.path,
            "--funcall=elisp/gen-autoloads-and-exit",
            out.path,
            package_name,
            ctx.actions.args().add_all(srcs),
        ],
        inputs = inputs,
        outputs = [out],
        tags = ctx.attr.tags,
        mnemonic = "GenAutoloads",
        progress_message = "Generating autoloads {}".format(out.short_path),
        manifest_basename = out.basename,
        manifest_sibling = out,
    )
    return out

# Directory relative to the current package where to store compiled files.  This
# is equivalent to _objs for C++ rules.  See
# https://bazel.build/remote/output-directories#layout-diagram.
_OUTPUT_DIR = "_elisp"
