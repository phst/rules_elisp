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

"""Defines the internal `cc_launcher` function."""

load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain")
load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load(":cc_default_info.bzl", "CcDefaultInfo")
load(":filenames.bzl", "check_relative_filename")
load(":generated.bzl", "CHR", "ORD")

visibility(["//elisp/toolchains"])

def cc_launcher(ctx, *, header, args, native = True):
    """Builds a launcher executable that starts Emacs.

    The current rule must provide the following attributes:
    - `_launcher_srcs`: a list of C++ source files to be compiled
    - `_launcher_deps`: a list of `cc_library` targets that the launcher depends
      on
    - `_launcher_defaults`: a `cc_defaults` rule to provide default settings

    Args:
      ctx (ctx): rule context
      header (string): header file to include, relative to the repository root
      args (list of strings): additional arguments for the function
      native (bool): whether the arguments should be wrapped in
        `RULES_ELISP_NATIVE_LITERAL`

    Returns:
      a pair `(executable, runfiles)` where `executable` is a `File` object
      representing the executable that starts Emacs and `runfiles` is a
      `runfiles` object for the runfiles that the executable will need
    """
    cc_toolchain = find_cc_toolchain(ctx)
    deps = ctx.attr._launcher_deps
    infos = [dep[CcInfo] for dep in deps]
    defaults = ctx.attr._launcher_defaults[CcDefaultInfo]
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
        srcs = ctx.files._launcher_srcs,
        compilation_contexts = [info.compilation_context for info in infos],
        local_defines = defaults.defines + [
            'RULES_ELISP_HEADER="' + check_relative_filename(header) + '"',
            "RULES_ELISP_LAUNCHER_ARGS=" + _cpp_strings(args, native = native),
        ],
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
    runfiles = ctx.runfiles().merge_all([dep[DefaultInfo].default_runfiles for dep in deps])
    return bin.executable, runfiles

def _cpp_strings(strings, *, native):
    """Formats the given string list as C++ initializer list.

    This function makes an effort to support strings with special characters.

    Args:
      strings (list of string): strings to be formatted
      native (bool): whether the strings should be wrapped in
        `RULES_ELISP_NATIVE_LITERAL`

    Returns:
      a string containing C++ code representing the given string list
    """
    return ", ".join([_cpp_string(s, native = native) for s in strings])

def _cpp_string(string, *, native):
    """Formats the given string as C++ string literal.

    This function makes an effort to support strings with special characters.

    Args:
      string: any string
      native (bool): whether the string should be wrapped in
        `RULES_ELISP_NATIVE_LITERAL`

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
    string = '"' + string + '"'
    if native:
        string = "RULES_ELISP_NATIVE_LITERAL(" + string + ")"
    return string

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

    def trail(off, min = 0x80, max = 0xBF):
        i = index + off
        if i >= len(string):
            fail("incomplete UTF-8 code unit sequence in string %r" % string)
        u = _utf8_code_unit(string, i)
        if u < min or u > max:
            fail("invalid UTF-8 code unit 0x%X at position %d in string %r" % (u, i, string))
        return u

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
