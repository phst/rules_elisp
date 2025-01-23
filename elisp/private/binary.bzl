# Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Shared implementation details for the `bazel_binary` and `bazel_test`
rules."""

load("@bazel_skylib//lib:collections.bzl", "collections")
load(
    "//private:defs.bzl",
    "cc_launcher",
    "check_relative_filename",
    "runfile_location",
)
load(":compile.bzl", "compile")

visibility("//elisp")

def binary(ctx, *, srcs, tags, args):
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
    result = compile(
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
    # https://github.com/bazelbuild/bazel/blob/7.4.1/src/main/java/com/google/devtools/build/lib/bazel/coverage/CoverageReportActionBuilder.java.
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
        header = "elisp/private/tools/binary.h",
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
