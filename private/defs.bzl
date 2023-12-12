# Copyright 2021, 2022, 2023 Google LLC
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

"""Internal-only rules and functions.

These definitions are internal and subject to change without notice."""

load("@bazel_skylib//lib:paths.bzl", "paths")

def _check_python_impl(target, ctx):
    tags = ctx.rule.attr.tags
    if "no-python-check" in tags:
        return []
    info = target[PyInfo]
    stem = "_{}.python-check".format(target.label.name)
    params_file = ctx.actions.declare_file(stem + ".json")
    output_file = ctx.actions.declare_file(stem + ".stamp")
    params = struct(
        srcs = [
            struct(
                rel = file.short_path,
                src = file.path,
                ext = bool(file.owner.workspace_name),
            )
            for file in info.transitive_sources.to_list()
        ],
    )
    ctx.actions.write(params_file, json.encode(params))
    pylintrc = ctx.file._pylintrc
    args = ctx.actions.args()
    args.add(output_file, format = "--out=%s")
    args.add(pylintrc, format = "--pylintrc=%s")
    args.add(params_file, format = "--params=%s")
    args.add_all(
        info.imports,
        format_each = "--import=%s",
        uniquify = True,
        expand_directories = False,
    )
    args.add_all(
        info.transitive_sources,
        map_each = _workspace_name,
        format_each = "--import=%s",
        uniquify = True,
        expand_directories = False,
    )
    args.add(ctx.workspace_name, format = "--import=%s")
    args.add(ctx.workspace_name, format = "--workspace-name=%s")
    if "no-pytype" not in tags:
        args.add("--pytype")
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [ctx.attr._check])
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = [params_file, pylintrc],
            transitive = [info.transitive_sources, tool_inputs],
        ),
        executable = ctx.executable._check,
        arguments = [args],
        mnemonic = "PythonCheck",
        progress_message = "Performing static analysis of target {}".format(target.label),
        input_manifests = input_manifests,
        toolchain = None,
    )
    return [OutputGroupInfo(check_python = depset([output_file]))]

check_python = aspect(
    implementation = _check_python_impl,
    attrs = {
        "_check": attr.label(
            default = Label("//dev:check_python"),
            executable = True,
            cfg = "exec",
        ),
        "_pylintrc": attr.label(
            default = Label("//:.pylintrc"),
            allow_single_file = True,
        ),
    },
    required_providers = [PyInfo],
)

def check_relative_filename(filename):
    """Returns `filename`, checking whether it is relative.

    The file name must be relative and represent either the current directory or
    an entry within the current directory or any of its subdirectories.  In
    other words, it may not point above the current directory.  To specify the
    current directory, pass a single dot (`.`).  This function also checks
    whether the filename contains special characters.  If the filename is
    invalid in any way (absolute, containing special characters, or pointing
    above the current directory), this function calls `fail` with a descriptive
    error message.  Otherwise, it returns the normalized version of `filename`,
    using purely lexical simplifications (not resolving symbolic links).

    Args:
      filename (string): the filename to check

    Returns:
      the normalized version of the `filename` argument
    """
    if not filename:
        fail("empty filename")
    if paths.is_absolute(filename):
        fail("filename {} is absolute".format(filename))
    filename = paths.normalize(filename)
    if not (filename == "." or filename[0].isalpha() or filename.startswith("_")):
        fail("filename {} has to start with a letter or underscore".format(filename))
    for char in filename.elems():
        if not (char.isalnum() or char in "-_./+$@%=,~"):
            fail("invalid character {} in filename {}".format(char, filename))
    return filename

def runfile_location(ctx, file):
    """Return the filename of the given file relative to the runfiles root.

    Args:
      ctx (ctx): the current rule context
      file (File): any file that’s included in the runfiles

    Returns:
      a string representing the filename of the file relative to the runfiles
      root
    """

    # It might seem surprising that we can use “ctx.workspace_name”
    # unconditionally.  However, for files in external workspaces, “short_path”
    # will start with “../〈workspace〉/…”, so the logic here is correct.
    # “check_relative_filename” not only ensures that the filename is relative,
    # but also canonicalizes it.
    return check_relative_filename(
        paths.join(ctx.workspace_name, file.short_path),
    )

def workspace_relative_filename(file):
    """Return the filename of the given file relative to its workspace root.

    Within an action, the file doesn’t necessarily exist at that location since
    it could be generated; use `file.path` instead to obtain a location that’s
    guaranteed to exist.  Within the runfiles tree, the file will be placed
    under the workspace directory for its owning target.  Since files can be
    present in multiple workspaces, the resulting name isn’t necessarily
    globally unique.

    Args:
      file (File): any file object

    Returns:
      a string representing the filename of the file relative to its workspace
      root
    """
    name = file.short_path
    if name.startswith("../"):
        # If the file is from another workspace, its short_path is of the form
        # “../WORKSPACE/PACKAGE/FILE.el”.  Strip off the leading “../WORKSPACE”
        # part.
        name = name[3:]
        ws, sep, name = name.partition("/")
        if not ws or not sep:
            fail("invalid name {}", file.short_path)
    return name

def cc_launcher(ctx, cc_toolchain, src, deps):
    """Builds a launcher executable that starts Emacs.

    You can use `find_cpp_toolchain` to construct an appropriate value for
    `cc_toolchain`.

    Args:
      ctx (ctx): rule context
      cc_toolchain (Provider): the C++ toolchain to use to compile the launcher
      src (File): C++ source file to compile; should contain a `main` function
      deps (list of Targets): `cc_library` targets to add as dependencies

    Returns:
      a pair `(executable, runfiles)` where `executable` is a `File` object
      representing the executable that starts Emacs and `runfiles` is a
      `runfiles` object for the runfiles that the executable will need
    """
    infos = [dep[CcInfo] for dep in deps]
    defaults = ctx.attr._launcher_defaults[CcDefaultInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = defaults.features + ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    _, objs = cc_common.compile(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        srcs = [src],
        compilation_contexts = [info.compilation_context for info in infos],
        local_defines = defaults.defines,
        user_compile_flags = defaults.copts,
    )
    bin = cc_common.link(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = objs,
        linking_contexts = [info.linking_context for info in infos],
        user_link_flags = defaults.linkopts,
    )
    runfiles = ctx.runfiles()
    for dep in deps:
        runfiles = runfiles.merge(dep[DefaultInfo].default_runfiles)
    return bin.executable, runfiles

def cpp_strings(strings):
    """Formats the given string list as C++ initializer list.

    This function makes an effort to support strings with special characters.

    Args:
      strings (list of string): strings to be formatted

    Returns:
      a string containing C++ code representing the given string list
    """
    return ", ".join([cpp_string(s) for s in strings])

def cpp_string(string):
    """Formats the given string as C++ string literal.

    This function makes an effort to support strings with special characters.

    Args:
      string: any string

    Returns:
      a string containing a properly escaped C++ string literal
    """
    if "\000" in string:
        fail("String {} can’t be transferred to C++".format(string))
    string = (
        string
            .replace("\\", "\\\\")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
    )
    for char in ("?", "'", '"'):
        string = string.replace(char, "\\" + char)
    return 'PHST_RULES_ELISP_NATIVE_LITERAL("' + string + '")'

def run_emacs(
        ctx,
        *,
        arguments,
        inputs,
        outputs,
        tags,
        mnemonic,
        progress_message,
        manifest_basename,
        manifest_sibling = None,
        manifest_load_path = None):
    """Runs Emacs with the default toolchain, wrapping it if necessary.

    Most parameters are mostly passed directly to ctx.actions.run.  The
    command-line arguments are prefixed with
    `--quick --batch --no-build-details` and `--wrap` as necessary.

    Args:
      ctx (ctx): rule context
      arguments (list of strings or Args objects): command-line arguments
      inputs (depset of File objects): input files
      outputs (list of File objects): output files
      tags (list of strings): list of rule tags to write into the manifest
      mnemonic (str): one-word action mnemonic
      progress_message (str): progress message
      manifest_basename (str): base name of the manifest file without extension
      manifest_sibling (File or None): file to use as sibling for the manifest
      manifest_load_path: (list of strings or None): additional load path for
          manifest with directories relative to the execution root
    """
    toolchain = ctx.toolchains[Label("//elisp:toolchain_type")]
    emacs = toolchain.emacs
    arguments = ["--quick", "--batch", "--no-build-details"] + arguments
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [emacs])
    inputs = depset(transitive = [inputs, tool_inputs])
    if toolchain.wrap:
        manifest = ctx.actions.declare_file(
            manifest_basename + ".manifest.json",
            sibling = manifest_sibling,
        )
        ctx.actions.write(
            output = manifest,
            content = struct(
                root = "EXECUTION_ROOT",
                loadPath = manifest_load_path,
                inputFiles = [
                    f.path
                    for f in inputs.to_list()
                    # Exclude middlemen, which don’t exist in the filesystem.
                    if not f.short_path.startswith("_middlemen/")
                ],
                outputFiles = [f.path for f in outputs],
                tags = tags,
            ).to_json(),
        )
        arguments = ["--manifest=" + manifest.path, "--"] + arguments
        inputs = depset(direct = [manifest], transitive = [inputs])
    ctx.actions.run(
        outputs = outputs,
        inputs = inputs,
        executable = emacs.files_to_run,
        arguments = arguments,
        mnemonic = mnemonic,
        progress_message = progress_message,
        use_default_shell_env = toolchain.use_default_shell_env,
        execution_requirements = toolchain.execution_requirements,
        input_manifests = input_manifests,
        toolchain = Label("//elisp:toolchain_type"),
    )

# Shared C++ compilation options.
COPTS = select({
    Label("//private:msvc-cl"): [
        "/WX",
        "/W4",
        "/utf-8",
        "/permissive-",
        "/Zc:__cplusplus",
    ],
    # Assume that something compatible with GCC is the default.  See
    # https://github.com/bazelbuild/bazel/issues/12707.
    Label("//conditions:default"): [
        "-finput-charset=utf-8",
        "-fexec-charset=utf-8",
        "-Werror",
        "-Wall",
        "-Wextra",
        "-Wconversion",
        "-Wsign-conversion",
        "-pedantic",
        "-pedantic-errors",
    ],
})

CXXOPTS = select({
    Label("//private:msvc-cl"): [],
    # Assume that something compatible with GCC is the default.  See
    # https://github.com/bazelbuild/bazel/issues/12707.
    Label("//conditions:default"): [
        # GCC appears to treat some moves as redundant that are in fact
        # necessary.
        "-Wno-redundant-move",
    ],
})

CONLYOPTS = select({
    Label("//private:msvc-cl"): [],
    # Assume that something compatible with GCC is the default.  See
    # https://github.com/bazelbuild/bazel/issues/12707.
    Label("//conditions:default"): ["-Wvla"],
})

DEFINES = ["_GNU_SOURCE"] + select({
    Label("@platforms//os:linux"): [],
    Label("@platforms//os:macos"): [],
    Label("@platforms//os:windows"): [
        "_UNICODE",
        "UNICODE",
        "STRICT",
        "NOMINMAX",
        "WIN32_LEAN_AND_MEAN",
    ],
})

CcDefaultInfo = provider(
    doc = "Internal provider for default C++ flags",
    fields = {
        "features": "Default features",
        "defines": "Local preprocessor definitions",
        "copts": "Default compiler flags",
        "linkopts": "Default linker flags",
    },
)

def _cc_defaults_impl(ctx):
    return CcDefaultInfo(
        features = ctx.attr.features,
        defines = ctx.attr.defines,
        copts = ctx.attr.copts,
        linkopts = ctx.attr.linkopts,
    )

cc_defaults = rule(
    implementation = _cc_defaults_impl,
    attrs = {
        "defines": attr.string_list(mandatory = True),
        "copts": attr.string_list(mandatory = True),
        "linkopts": attr.string_list(mandatory = True),
    },
    doc = "Internal rule for default C++ flags",
    provides = [CcDefaultInfo],
)

def _bootstrap_impl(ctx):
    src = ctx.file.src
    out = ctx.outputs.out
    compile = ctx.file._compile
    run_emacs(
        ctx,
        arguments = [
            "--load=" + compile.path,
            "--fatal-warnings",
            "--funcall=elisp/compile-batch-and-exit",
            ctx.label.workspace_name,
            src.path,
            out.path,
        ],
        inputs = depset([src, compile]),
        outputs = [out],
        tags = ctx.attr.tags,
        mnemonic = "ElispCompile",
        progress_message = "Compiling {}".format(src.short_path),
        manifest_basename = out.basename,
        manifest_sibling = out,
    )

bootstrap = rule(
    implementation = _bootstrap_impl,
    attrs = {
        "src": attr.label(mandatory = True, allow_single_file = [".el"]),
        "out": attr.output(mandatory = True),
        "_compile": attr.label(
            allow_single_file = [".el"],
            default = Label("//elisp:compile.el"),
        ),
    },
    doc = "Primitive version of `elisp_library` used for bootstrapping",
    toolchains = ["//elisp:toolchain_type"],
    incompatible_use_toolchain_transition = True,
)

def _merged_manual_impl(ctx):
    orgs = []
    for bin in ctx.files.includes:
        org = ctx.actions.declare_file(paths.replace_extension(bin.basename, ".org"))
        ctx.actions.run(
            outputs = [org],
            inputs = [bin],
            executable = ctx.executable._generate,
            arguments = ["--", bin.path, org.path],
            mnemonic = "GenOrg",
            progress_message = "Generating Org file {}".format(org.short_path),
            toolchain = None,
        )
        orgs.append(org)
    ctx.actions.run(
        outputs = [ctx.outputs.out],
        inputs = [ctx.file.main] + orgs,
        executable = ctx.executable._merge,
        arguments = [ctx.outputs.out.path, ctx.file.main.path] + [o.path for o in orgs],
        mnemonic = "MergeManual",
        progress_message = "Generating merged manual {}".format(ctx.outputs.out.short_path),
        toolchain = None,
    )

merged_manual = rule(
    attrs = {
        "main": attr.label(
            allow_single_file = [".org"],
            mandatory = True,
        ),
        "includes": attr.label_list(
            allow_files = [".bin"],
            mandatory = True,
            allow_empty = False,
        ),
        "out": attr.output(mandatory = True),
        "_generate": attr.label(
            executable = True,
            cfg = "exec",
            default = Label("//docs:generate"),
        ),
        "_merge": attr.label(
            executable = True,
            cfg = "exec",
            default = Label("//docs:merge"),
        ),
    },
    implementation = _merged_manual_impl,
)

def _workspace_name(file):
    # Skip empty string for main repository.
    return file.owner.workspace_name or None
