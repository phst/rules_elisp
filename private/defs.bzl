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
    "//elisp/common",
    "//elisp/extensions",
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

FEATURES = select({
    Label(":treat_warnings_as_errors_enabled"): ["treat_warnings_as_errors"],
    Label("//conditions:default"): [],
})

# Shared C++ compilation options.
COPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [
        "/W4",
        "/utf-8",
        "/permissive-",
        "/Zc:__cplusplus",
        "/external:W2",  # TODO: shouldn’t be needed; file bug against rules_cc
    ],
    Label("//elisp/private:gcc_or_clang"): [
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
    Label("@rules_cc//cc/compiler:gcc"): [
        # GCC appears to treat some moves as redundant that are in fact
        # necessary.
        "-Wno-redundant-move",
    ],
    Label("@rules_cc//cc/compiler:clang"): [],
})

CONLYOPTS = select({
    Label("@rules_cc//cc/compiler:msvc-cl"): [],
    Label("//elisp/private:gcc_or_clang"): ["-Wvla"],
})

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

LINKOPTS = []

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
