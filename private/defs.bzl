# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

visibility([
    # keep sorted
    "//",
    "//dev",
    "//docs",
    "//elisp",
    "//elisp/ert",
    "//elisp/private",
    "//elisp/private/tools",
    "//elisp/proto",
    "//elisp/runfiles",
    "//elisp/toolchains",
    "//emacs",
    "//examples",
    "//gazelle",
    "//tests",
    "//tests/pkg",
])

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
    if not (filename == "." or filename[0].isalpha() or filename[0] in "_+"):
        fail("filename {} has to start with a letter, underscore, or plus sign".format(filename))
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
    # unconditionally.  However, for files in external repositories,
    # “short_path” will start with “../〈repository〉/…”, so the logic here is
    # correct.  “check_relative_filename” not only ensures that the filename is
    # relative, but also canonicalizes it.  Also see
    # https://bazel.build/extending/rules#runfiles_location.
    return check_relative_filename(
        paths.join(ctx.workspace_name, file.short_path),
    )

def repository_relative_filename(file):
    """Return the filename of the given file relative to its repository root.

    Within an action, the file doesn’t necessarily exist at that location since
    it could be generated; use `file.path` instead to obtain a location that’s
    guaranteed to exist.  Within the runfiles tree, the file will be placed
    under the repository directory for its owning target.  Since files can be
    present in multiple repositories, the resulting name isn’t necessarily
    globally unique.

    Args:
      file (File): any file object

    Returns:
      a string representing the filename of the file relative to its repository
      root
    """
    name = file.short_path
    if name.startswith("../"):
        # If the file is from another repository, its short_path is of the form
        # “../REPOSITORY/PACKAGE/FILE.el”.  Strip off the leading
        # “../REPOSITORY” part.
        name = name[3:]
        ws, sep, name = name.partition("/")
        if not ws or not sep:
            fail("invalid name {}".format(file.short_path))
    return name

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
    if toolchain.wrap:
        manifest = ctx.actions.declare_file(
            manifest_basename + ".manifest.json",
            sibling = manifest_sibling,
        )
        ctx.actions.write(
            output = manifest,
            content = json.encode(struct(
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
            )),
        )
        arguments = [
            ctx.actions.args().add(manifest, format = "--manifest=%s").add("--"),
        ] + arguments
        manifests = depset([manifest])
    else:
        manifests = depset()
    ctx.actions.run(
        outputs = outputs,
        # Add manifest after the actual inputs so that the progress message can
        # use %{input} as usual.
        inputs = depset(transitive = [inputs, manifests], order = "preorder"),
        executable = emacs.files_to_run,
        arguments = arguments,
        mnemonic = mnemonic,
        progress_message = progress_message,
        use_default_shell_env = toolchain.use_default_shell_env,
        execution_requirements = toolchain.execution_requirements,
        toolchain = Label("//elisp:toolchain_type"),
    )

# Features for all packages.  These may not contain select expressions.
# FIXME: Once we drop support for Bazel 7.0, move these features to the
# REPO.bazel files, and remove them from BUILD files.
PACKAGE_FEATURES = [
    "no_copts_tokenization",
    "layering_check",
    "parse_headers",
    "external_include_paths",
    # On Windows, Bazel generates incorrectly-escaped parameter files.  See
    # https://github.com/bazelbuild/bazel/issues/21029.
    "-compiler_param_file",
    "-macos_default_link_flags",
]

FEATURES = select({
    Label(":treat_warnings_as_errors_enabled"): ["treat_warnings_as_errors"],
    Label("//conditions:default"): [],
})

LAUNCHER_FEATURES = FEATURES

# Shared C++ compilation options.
COPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [
        "/W4",
        "/utf-8",
        "/permissive-",
        "/Zc:__cplusplus",
        "/external:W2",  # TODO: shouldn’t be needed; file bug against rules_cc
    ],
    Label("//private:gcc_or_clang"): [
        "-finput-charset=utf-8",
        "-fexec-charset=utf-8",
        "-Wall",
        "-Wextra",
        "-Wconversion",
        "-Wsign-conversion",
        "-pedantic",
    ],
}) + select({
    Label("@rules_cc//cc/compiler:clang"): [
        # Work around https://github.com/llvm/llvm-project/issues/121984.
        "--system-header-prefix=absl/",
        "--system-header-prefix=google/",
        "--system-header-prefix=tools/",
        "--system-header-prefix=upb/",
    ],
    Label("//conditions:default"): [],
})

CXXOPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [],
    Label("//private:gcc"): [
        # GCC appears to treat some moves as redundant that are in fact
        # necessary.
        "-Wno-redundant-move",
    ],
    Label("@rules_cc//cc/compiler:clang"): [],
})

CONLYOPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [],
    Label("//private:gcc_or_clang"): ["-Wvla"],
})

LAUNCHER_COPTS = COPTS + CXXOPTS

DEFINES = [
    # https://pubs.opengroup.org/onlinepubs/9699919799/functions/V2_chap02.html#tag_15_02_01
    "_POSIX_C_SOURCE=200809L",
    "_XOPEN_SOURCE=700",
] + select({
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

LAUNCHER_DEFINES = DEFINES

LINKOPTS = []

LAUNCHER_LINKOPTS = LINKOPTS + select({
    Label("@rules_cc//cc/compiler:msvc-cl"): ["/SUBSYSTEM:CONSOLE"],
    Label("//private:gcc_or_clang"): [],
})

CcDefaultInfo = provider(
    doc = "Internal provider for default C++ flags",
    fields = {
        "features": "Default features",
        "disabled_features": "Features to disable",
        "defines": "Local preprocessor definitions",
        "copts": "Default compiler flags",
        "linkopts": "Default linker flags",
    },
)

def _cc_defaults_impl(ctx):
    features, disabled_features = parse_features(ctx.attr.features)
    return CcDefaultInfo(
        features = features,
        disabled_features = disabled_features,
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

ModuleConfigInfo = provider(
    doc = "Internal provider for system-specific Emacs module configuration",
    fields = {
        "suffix": "Filename suffix for Emacs modules",
        "additional_linker_inputs": "Additional inputs for the linker to build Emacs modules",
    },
)

def _executable_only_impl(ctx):
    info = ctx.attr.src[DefaultInfo]
    files_to_run = info.files_to_run or fail("missing files_to_run")
    executable = files_to_run.executable or fail("missing executable")
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

LAUNCHER_ATTRS = {
    "_launcher_srcs": attr.label_list(
        default = [Label("//elisp/private/tools:launcher.cc")],
        allow_files = [".cc"],
    ),
    "_launcher_defaults": attr.label(
        default = Label("//elisp:launcher_defaults"),
        providers = [CcDefaultInfo],
    ),
}

LAUNCHER_DEPS = [
    Label("//elisp/private/tools:platform"),
    Label("@abseil-cpp//absl/container:fixed_array"),
    Label("@abseil-cpp//absl/log"),
    Label("@abseil-cpp//absl/meta:type_traits"),
    Label("@abseil-cpp//absl/status"),
    Label("@abseil-cpp//absl/status:statusor"),
    Label("@abseil-cpp//absl/types:span"),
]

# FIXME: This restriction is arbitrary; elisp_binary rules should accept any
# number of input files if necessary.
MAX_MANUAL_ADDITIONAL_INPUTS = 10

def parse_features(features):
    """Parse a list of feature strings.

    Args:
      features (list of str): feature strings from some `features` attribute

    Returns:
      a pair (features, disabled_features) of lists of strings
    """
    return (
        [f for f in features if not f.startswith("-")],
        [f.removeprefix("-") for f in features if f.startswith("-")],
    )
