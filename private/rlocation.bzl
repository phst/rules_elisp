# Copyright 2026 Philipp Stephani
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

"""Contains helper functions to properly quote runfile locations."""

load("@bazel_skylib//lib:shell.bzl", "shell")

visibility([
    # keep sorted
    "//elisp/private/tools",
    "//gazelle/elisp",
    "//tests/integration",
    "//tests/integration/wrap",
    "//tests/proto/integration",
    "//tests/runfiles",
    "//tests/tools",
])

def rlocation(format, label):
    """Splices a runfile location into the format.

    <var>format</var> must contain exactly one %s sequence, which is replaced by
    $(rlocationpath <var>label</var>).  The output is properly quoted so it can
    be used in `args` or `defines` attributes.

    Args:
      format: a format string, must contain exactly one %s sequence
      label: a Blaze label denoting a runfile

    Returns:
      a quoted version of
      <code><var>format % "$(rlocationpath <var>label</var>)"</code>
    """
    before, sep, after = format.partition("%s")
    if not sep or "%" in before or "%" in after:
        fail("Invalid format string %r" % format)

    # Note that rlocation already quotes its result (see
    # https://github.com/bazelbuild/bazel/issues/6531), so we must not quote it
    # again.
    return shell.quote(before) + "$(rlocationpath %s)" % label + shell.quote(after)

def runfileflag(label):
    """Returns a flag string that can be used with testutil.RunfileFlag.

    Args:
      label: a Blaze label denoting a runfile

    Returns:
      a quoted string of the form
      <code>--<var>label</var>=$(rlocationpath <var>label</var>)</code>
    """
    return rlocation("--%s=%%s" % label, label)
