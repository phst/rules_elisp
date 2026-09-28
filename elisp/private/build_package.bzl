# Copyright 2025 Google LLC
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

"""Private utility functions to build Emacs Lisp packages."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load(":run_emacs.bzl", "run_emacs")

visibility(["//elisp"])

def build_package(ctx, srcs, data):
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
