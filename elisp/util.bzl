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

"""Contains various utility functions for Emacs Lisp rules.

These functions are internal and subject to change without notice."""

load("@bazel_skylib//lib:paths.bzl", "paths")

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
    if filename != "." and not filename[0].isalpha():
        fail("filename {} has to start with a letter".format(filename))
    for char in filename.elems():
        if not char.isalnum() and char not in "-_./+$@%":
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
    # will start with “../〈workspace〉/…”, so the logic here is correct.
    # “check_relative_filename” not only ensures that the filename is relative,
    # but also canonicalizes it.
    return check_relative_filename(
        paths.join(ctx.workspace_name, file.short_path),
    )

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
        grep_includes = ctx.executable._grep_includes,
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

def cpp_ints(ints):
    """Formats the given integer list as C++ initializer list.

    Args:
      ints (list of int): numbers to be formatted

    Returns:
      a string containing C++ code representing the given number list
    """
    return ", ".join([cpp_int(i) for i in ints])

def cpp_int(int):
    """Format the given integer as C++ decimal literal.

    Args:
      int: an integer

    Returns:
      a string containing a C++ decimal literal
    """

    # See https://stackoverflow.com/a/1819236 for the guarantees on the C++ int
    # type range.
    if int < -32767 or int > 32767:
        fail("integer {} out of range".format(int))
    return str(int)

# Note: Toolchain names need to be fully qualified, otherwise external
# workspaces won’t find them.
TOOLCHAIN_TYPE = "@phst_rules_elisp//elisp:toolchain_type"

def run_emacs(
        ctx,
        arguments,
        inputs,
        outputs,
        mnemonic,
        progress_message,
        manifest_basename,
        manifest_sibling = None,
        manifest_load_path = None):
    """Runs Emacs with the default toolchain, wrapping it if necessary.

    Most parameters are mostly passed directly to ctx.actions.run.  The
    command-line arguments are prefixed with `--quick --batch` and `--wrap` as
    necessary.

    Args:
      ctx (ctx): rule context
      arguments (list of strings or Args objects): command-line arguments
      inputs (list or depset of File objects): input files
      outputs (list of File objects): output files
      mnemonic (str): one-word action mnemonic
      progress_message (str): progress message
      manifest_basename (str): base name of the manifest file without extension
      manifest_sibling (File or None): file to use as sibling for the manifest
      manifest_load_path: (list of strings or None): additional load path for
          manifest with directories relative to the execution root
    """
    toolchain = ctx.toolchains["@phst_rules_elisp//elisp:toolchain_type"]
    emacs = toolchain.emacs
    arguments = ["--quick", "--batch"] + arguments
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
                tags = ctx.attr.tags,
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
    )

CcDefaultInfo = provider(
    doc = "Internal provider for default C++ flags",
    fields = {
        "features": "Default features",
        "copts": "Default compiler flags",
        "linkopts": "Default linker flags",
    },
)

def _cc_defaults_impl(ctx):
    return CcDefaultInfo(
        features = ctx.attr.features,
        copts = ctx.attr.copts,
        linkopts = ctx.attr.linkopts,
    )

cc_defaults = rule(
    implementation = _cc_defaults_impl,
    attrs = {
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
            src.path,
            out.path,
        ],
        inputs = depset([src, compile]),
        outputs = [out],
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
        "_compile": attr.label(allow_single_file = [".el"], default = "//elisp:compile.el"),
    },
    doc = "Primitive version of `elisp_library` used for bootstrapping",
    toolchains = ["@phst_rules_elisp//elisp:toolchain_type"],
    incompatible_use_toolchain_transition = True,
)
