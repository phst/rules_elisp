# Copyright 2021, 2022, 2023, 2024 Google LLC
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
load("@phst_rules_elisp_deps//:defs.bzl", "CHR", "ORD")

visibility(["//docs", "//elisp", "//elisp/proto", "//emacs"])

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

def cc_launcher(ctx, cc_toolchain, src, deps, *, defines):
    """Builds a launcher executable that starts Emacs.

    You can use `find_cpp_toolchain` to construct an appropriate value for
    `cc_toolchain`.

    Args:
      ctx (ctx): rule context
      cc_toolchain (Provider): the C++ toolchain to use to compile the launcher
      src (File): C++ source file to compile; should contain a `main` function
      deps (list of Targets): `cc_library` targets to add as dependencies
      defines (list of strings): additional preprocessor definitions for
          compiling `src`

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
        # On Windows, Bazel generates incorrectly-escaped parameter files.
        unsupported_features = ctx.disabled_features + ["compiler_param_file"],
    )
    _, objs = cc_common.compile(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        srcs = [src],
        compilation_contexts = [info.compilation_context for info in infos],
        local_defines = defaults.defines + defines,
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

    # Interpret the string as UTF-8.  That’s not really correct,
    # cf. https://bazel.build/concepts/build-files.  However, we assume that in
    # practice all BUILD files do in fact use UTF-8 instead of Latin-1.  Due to
    # the implementation of Starlark strings, the string will actually be a
    # sequence of UTF-8 code units (and not code points), so we have to decode
    # it first.
    string = "".join([_cpp_char(c) for c in _decode_utf8(string)])
    return 'PHST_RULES_ELISP_NATIVE_LITERAL("' + string + '")'

def _cpp_char(point):
    """Returns a C++ representation of a Unicode code point.

    The return value can be used in character and string literals.

    Args:
      point (int): a Unicode code point

    Returns:
      a C++ string literal representation of `point`
    """
    if point == 0:
        fail("can’t have embedded null characters in C++ literals")

    # See https://en.cppreference.com/w/cpp/language/escape.
    esc = _CPP_ESCAPES.get(point)
    if esc != None:  # special treatment
        return esc
    if 0x20 <= point and point <= 0x7F:  # ASCII, no need to escape
        return CHR[point]
    if point <= 0xFFFF:  # BMP character
        return "\\u" + _hex(point, pad = 4)
    if point <= 0x10FFFF:  # Non-BMP character
        return "\\U" + _hex(point, pad = 8)
    fail("invalid code point U+%X" % point)

_CPP_ESCAPES = {
    ORD["\\"]: "\\\\",
    ORD["\n"]: "\\n",
    ORD["\r"]: "\\r",
    ORD["\t"]: "\\t",
    ORD["?"]: "\\?",
    ORD["'"]: "\\'",
    ORD['"']: '\\"',
}

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
    arguments = [
        ctx.actions.args().add("--quick").add("--batch").add("--no-build-details"),
    ] + arguments
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
        arguments = [
            ctx.actions.args().add(manifest, format = "--manifest=%s").add("--"),
        ] + arguments
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

FEATURES = select({
    # We can’t use treat_warnings_as_errors on macOS yet because it tries to
    # pass a flag -fatal-warnings to the linker, but the macOS linker accepts
    # -fatal_warnings instead.
    # TODO: File bug against Bazel.
    "@platforms//os:macos": [],
    "//conditions:default": ["treat_warnings_as_errors"],
})

# Shared C++ compilation options.
COPTS = select({
    Label("//private:msvc-cl"): [
        "/WX",
        "/W4",
        "/utf-8",
        "/permissive-",
        "/Zc:__cplusplus",
    ],
    Label("//private:gcc_or_clang"): [
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
    Label("//private:gcc"): [
        # GCC appears to treat some moves as redundant that are in fact
        # necessary.
        "-Wno-redundant-move",
    ],
    Label("//private:clang"): [],
})

CONLYOPTS = select({
    Label("//private:msvc-cl"): [],
    Label("//private:gcc_or_clang"): ["-Wvla"],
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
    args = ctx.actions.args()
    args.add(compile, format = "--load=%s")
    args.add("--fatal-warnings")
    args.add("--funcall=elisp/compile-batch-and-exit")
    args.add(ctx.label.workspace_name)
    args.add(src)
    args.add(out)
    run_emacs(
        ctx,
        arguments = [args],
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
    toolchains = [Label("//elisp:toolchain_type")],
)

def _executable_only_impl(ctx):
    info = ctx.attr.src[DefaultInfo]
    files_to_run = info.files_to_run
    if not files_to_run:
        fail("missing files_to_run")
    executable = info.files_to_run.executable
    if not executable:
        fail("missing executable")
    return DefaultInfo(
        files = depset([executable]),
        runfiles = info.default_runfiles,
    )

executable_only = rule(
    implementation = _executable_only_impl,
    attrs = {"src": attr.label(mandatory = True)},
    doc = """Strip non-executable output files from `src`.

Use this rule to wrap a `py_binary` target for use with `$(rlocationpath …)`
etc.  This is necessary because `py_binary` also returns the main source file as
additional file to build.
""",
)

def _merged_manual_impl(ctx):
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [ctx.attr._generate])
    orgs = []
    for bin in ctx.files.includes:
        org = ctx.actions.declare_file(paths.replace_extension(bin.basename, ".org"))
        ctx.actions.run(
            outputs = [org],
            inputs = depset(direct = [bin], transitive = [tool_inputs]),
            executable = ctx.executable._generate,
            arguments = [ctx.actions.args().add("--").add(bin).add(org)],
            mnemonic = "GenOrg",
            progress_message = "Generating Org file {}".format(org.short_path),
            input_manifests = input_manifests,
            toolchain = None,
        )
        orgs.append(org)

    args = ctx.actions.args()
    args.add(ctx.attr.exclude_tag)
    args.add(ctx.outputs.out)
    args.add(ctx.file.main)
    args.add_all(orgs, expand_directories = False)
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [ctx.attr._merge])
    ctx.actions.run(
        outputs = [ctx.outputs.out],
        inputs = depset(direct = [ctx.file.main] + orgs, transitive = [tool_inputs]),
        executable = ctx.executable._merge,
        arguments = [args],
        mnemonic = "MergeManual",
        progress_message = "Generating merged manual {}".format(ctx.outputs.out.short_path),
        input_manifests = input_manifests,
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
        "exclude_tag": attr.string(mandatory = True),
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

def _decode_utf8(string):
    """Decodes an UTF-8 string into a list of Unicode codepoints.

    Args:
      string: a string that is assumed to be a valid UTF-8 string, i.e., each
          character in the string should be a UTF-8 code unit

    Returns:
      a list of Unicode code points (integers)
    """
    ret = []
    skip = 0
    for i in range(len(string)):
        # Starlark doesn’t allow us to modify the loop variable here (to
        # guarantee termination), so we skip iterations as necessary instead.
        if skip == 0:
            n, c = _decode_utf8_seq(string, i)
            ret.append(c)
            skip = n - 1
        else:
            skip -= 1
    return ret

def _decode_utf8_seq(string, index):
    """Decodes an UTF-8 code unit sequence into a Unicode codepoints.

    Args:
      string: a string that is assumed to be a valid UTF-8 string, i.e., each
          character in the string should be a UTF-8 code unit
      index: zero-based starting position of the UTF-8 code unit sequence

    Returns:
      a tuple (length, point) of two integers, where `length` is the length of
      the code unit sequence and `point` is the corresponding Unicode code point
    """

    # See the Unicode standard, chapter 3, clause D92, especially the tables 3-6
    # and 3-7.
    a = _utf8_code_unit(string, index)
    if 0x00 <= a and a <= 0x7F:  # one byte
        return 1, a
    trail = lambda off, min = 0x80, max = 0xBF: _utf8_trailing_code_unit(string, index + off, min, max)
    if 0xC2 <= a and a <= 0xDF:  # two bytes
        b = trail(1)
        return 2, ((a & 0x1F) << 6) | (b & 0x3F)
    if 0xE0 <= a and a <= 0xEF:  # three bytes
        b = trail(1, 0xA0 if a == 0xE0 else 0x80, 0x9F if a == 0xED else 0xBF)
        c = trail(2)
        return 3, ((a & 0x0F) << 12) | ((b & 0x3F) << 6) | (c & 0x3F)
    if 0xF0 <= a and a <= 0xF4:  # four bytes
        b = trail(1, 0x90 if a == 0xF0 else 0x80, 0x8F if a == 0xF4 else 0xBF)
        c = trail(2)
        d = trail(3)
        return 4, ((a & 0x07) << 18) | ((b & 0x3F) << 12) | ((c & 0x3F) << 6) | (d & 0x3F)
    fail("invalid leading UTF-8 code unit 0x%X at position %d in string %r" % (a, index, string))

def _utf8_code_unit(string, index):
    """Returns a single UTF-8 code unit in a string.

    Args:
      string: a string that is assumed to be a valid UTF-8 string, i.e., each
          character in the string should be a UTF-8 code unit
      index: zero-based position of the UTF-8 code unit to retrieve

    Returns:
      the UTF-8 code unit as an integer
    """
    c = string[index]
    u = ORD.get(c)
    if u == None or u < 0x00 or u > 0xFF:
        fail("invalid UTF-8 code unit %r at position %d in string %r" % (c, index, string))
    return u

def _utf8_trailing_code_unit(string, index, min, max):
    """Returns a single trailing UTF-8 code unit in a string.

    Checks that the code unit is in a valid range.

    Args:
      string: a string that is assumed to be a valid UTF-8 string, i.e., each
          character in the string should be a UTF-8 code unit
      index: zero-based position of the UTF-8 code unit to retrieve
      min: lowest allowed code unit, inclusive
      max: highest allowed code unit, inclusive

    Returns:
      the UTF-8 code unit as an integer
    """
    if index >= len(string):
        fail("incomplete UTF-8 code unit sequence in string %r" % string)
    u = _utf8_code_unit(string, index)
    if u < min or u > max:
        fail("invalid UTF-8 code unit 0x%X at position %d in string %r" % (u, index, string))
    return u

def _hex(num, *, pad):
    """Converts a number to a hexadecimal string with padding.

    Args:
      num: a nonnegative integer
      pad: minimum number of digits to return

    Returns:
      a string that’s at least `pad` digits long
    """
    if num < 0:
        fail("can’t convert negative number %d to hexadecimal" % num)
    ret = "%X" % num
    if len(ret) < pad:
        ret = (pad - len(ret)) * "0" + ret
    return ret
