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

"""Contains various utility functions for Emacs Lisp rules.

These functions are internal and subject to change without notice."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

def check_relative_filename(filename):
    """Returns `filename`, checking whether it is relative.

    The file name must be relative and represent either the current directory
    or an entry within the current directory or any of its subdirectories.  In
    other words, it may not point above the current directory.  To specify the
    current directory, pass an empty string or a single dot (`.`).  This
    function also checks whether the filename contains special characters.  If
    the filename is invalid in any way (absolute, containing special
    characters, or pointing above the current directory), this function calls
    `fail` with a descriptive error message.  Otherwise, it returns the
    normalized version of `filename`, using purely lexical simplifications (not
    resolving symbolic links).

    Args:
      filename (string): the filename to check

    Returns:
      the normalized version of the `filename` argument
    """
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

def configure_cc_toolchain(ctx):
    """
    Configures the C/C++ toolchain for use with the current rule.

    Args:
      ctx (ctx): the current rule context

    Returns:
      a pair (toolchain, config) of a `CcToolchainInfo` provider and a
      `FeatureConfiguration` object appropriate for C and C++ compilation
      actions
    """
    toolchain = find_cpp_toolchain(ctx)
    config = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    return toolchain, config

def cc_wrapper(ctx, cc_toolchain, feature_configuration, driver):
    """Builds a wrapper executable that starts Emacs.

    You can use `configure_cc_toolchain` to construct appropriate values for
    `cc_toolchain` and `feature_configuration`.

    Args:
      ctx (ctx): rule context
      cc_toolchain (Provider): the C++ toolchain to use to compile the wrapper
      feature_configuration (FeatureConfiguration): the features to use to
          compile the wrapper
      driver (File): C++ driver file to compile

    Returns:
      a File representing the executable that starts Emacs
    """
    exec = ctx.attr._exec[CcInfo]
    _, objs = cc_common.compile(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        srcs = [driver],
        compilation_contexts = [exec.compilation_context],
        user_compile_flags = COPTS,
    )
    bin = cc_common.link(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = objs,
        linking_contexts = [exec.linking_context],
    )
    return bin.executable

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

    # Use raw strings and choose a delimiter that’s extremely unlikely to occur
    # in real-world code.
    delim = "#*?&"
    open = 'R"' + delim + "("
    close = ")" + delim + '"'
    if close in string:
        fail("String {} can’t be transferred to C++".format(string))
    return open + string + close

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

# Shared C++ compilation options.
COPTS = [
    "-fno-exceptions",
    "-Werror",
    "-Wall",
    "-Wextra",
    "-Wconversion",
    "-Wsign-conversion",
    "-pedantic-errors",
    # GCC appears to treat some moves as redundant that are in fact necessary.
    "-Wno-redundant-move",
    "-D_GNU_SOURCE",
]
