# Copyright 2020-2025 Google LLC
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

"""Defines the rule `elisp_emacs_binary`, which compiles Emacs for use in
Bazel."""

load("@rules_cc//cc:action_names.bzl", "CPP_LINK_EXECUTABLE_ACTION_NAME", "C_COMPILE_ACTION_NAME")
load("@rules_cc//cc:find_cc_toolchain.bzl", "CC_TOOLCHAIN_ATTRS", "use_cc_toolchain")
load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load("//elisp/private:cc_default_info.bzl", "CcDefaultInfo")
load("//elisp/private:cc_launcher.bzl", "cc_launcher")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_ATTRS", "LAUNCHER_DEPS")
load("//elisp/private:filenames.bzl", "runfile_location")

visibility("public")

def _elisp_emacs_binary_impl(ctx):
    """Rule implementation of the “elisp_emacs_binary” rule."""
    srcs = ctx.files.srcs
    if len(srcs) == 1:
        (archive,) = srcs
        readme = None
    else:
        # It’s not possible to refer to a directory as a label, so we refer to a
        # known file (README in the source root) instead.
        archive = None
        readme = ctx.file.readme

    mode = ctx.attr.mode
    if mode == "source":
        shell_toolchain = ctx.toolchains[Label("@rules_shell//shell:toolchain_type")]
        emacs_cc_toolchain = ctx.attr._emacs_cc_toolchain[cc_common.CcToolchainInfo]
        install = _install(ctx, shell_toolchain, emacs_cc_toolchain, archive = archive, strip_prefix = ctx.attr.strip_prefix, readme = readme)
    elif mode == "release":
        if not archive:
            fail("release mode requires a source archive")
        install = _unpack(ctx, archive, strip_prefix = ctx.attr.strip_prefix)
    else:
        fail("invalid build mode {}".format(mode))

    executable, runfiles = cc_launcher(
        ctx,
        header = "elisp/private/tools/emacs.h",
        args = [
            mode,
            runfile_location(ctx, install),
        ],
        native = False,
    )
    return [
        DefaultInfo(
            executable = executable,
            files = depset(direct = [executable]),
            runfiles = ctx.runfiles(files = [install]).merge(runfiles),
        ),
    ]

elisp_emacs_binary = rule(
    # FIXME: Remove CC_TOOLCHAIN_ATTRS once
    # https://github.com/bazelbuild/bazel/issues/7260 is fixed.
    # @unsorted-dict-items
    attrs = CC_TOOLCHAIN_ATTRS | LAUNCHER_ATTRS | {
        "mode": attr.string(
            doc = """How to build and install Emacs.  Possible values are:
- `source`: Build Emacs from sources using `configure` and `make install`.
  `srcs` must refer to an unpacked Emacs source archive.
- `release`: Run Emacs directly without building or installing.
  `srcs` must refer to an unpacked Emacs release archive containing pre-built
  binaries for the correct target operating system and architecture.
  Currently this only works on Windows.""",
            values = ["source", "release"],
            default = "source",
        ),
        "srcs": attr.label_list(
            allow_files = True,
            allow_empty = False,
            mandatory = True,
            doc = """Either a single Emacs source archive, or all Emacs source
files from an already-unpacked archive.""",
        ),
        "strip_prefix": attr.string(
            doc = """Prefix to strip from the source archive.
Ignored if `srcs` doesn’t refer to an archive.""",
        ),
        "readme": attr.label(
            allow_single_file = True,
            doc = """The README file in the root of the Emacs repository.
This is necessary to determine the source root directory if `srcs` refers
to unpacked Emacs sources.  If `srcs` refers to a source archive,
`readme` is ignored.""",
        ),
        "module_header": attr.output(
            doc = """Label for a file target that will receive the
`emacs-module.h` header.  If not provided, don’t install the header.""",
        ),
        "builtin_features": attr.output(
            doc = """Label for a file into which to write the list
of builtin features.  If not provided, don’t write such a file.
This is used by Gazelle.""",
        ),
        "dump_mode": attr.string(
            default = "portable",
            values = ["portable"],
            doc = "Deprecated; must always be set to `portable`.",
        ),
        "_build": attr.label(
            default = Label("//elisp/private/tools:build_emacs"),
            executable = True,
            cfg = "exec",
        ),
        "_emacs_cc_toolchain": attr.label(
            default = Label("//elisp/private:emacs_cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_launcher_deps": attr.label_list(
            default = LAUNCHER_DEPS + [Label("//elisp/private/tools:emacs")],
            providers = [CcInfo],
        ),
        "_emacs_defaults": attr.label(
            default = Label("//elisp/private:emacs_defaults"),
            providers = [CcDefaultInfo],
        ),
    },
    doc = """Builds Emacs from a source repository.
The resulting executable can be used to run the compiled Emacs.""",
    executable = True,
    fragments = ["cpp"],
    host_fragments = ["cpp"],
    toolchains = use_cc_toolchain() + [Label("@rules_shell//shell:toolchain_type")],
    implementation = _elisp_emacs_binary_impl,
)

def _install(ctx, shell_toolchain, cc_toolchain, *, archive, strip_prefix, readme):
    """Builds and install Emacs.

    Args:
      ctx (ctx): rule context
      shell_toolchain (Provider): the shell toolchain to use on Windows; must
          be an MSYS2 toolchain
      cc_toolchain (Provider): the C toolchain to use to compile Emacs
      archive (File): Emacs source archive to build from, or `None` if building
          from an unpacked source tree
      strip_prefix (str): prefix to strip from the files in `archive`; ignored
          if building from an unpacked source tree
      readme (File): location of the README file in the Emacs source tree; only
          used if `archive` is `None`

    Returns:
      a File representing the Emacs installation directory
    """
    defaults = ctx.attr._emacs_defaults[CcDefaultInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = defaults.features + ctx.features,
        unsupported_features = defaults.disabled_features + ctx.disabled_features,
    )
    fragment = ctx.fragments.cpp
    vars = cc_common.create_compile_variables(
        cc_toolchain = cc_toolchain,
        feature_configuration = feature_configuration,
        preprocessor_defines = depset(defaults.defines),
    )
    cc = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    )
    cflags = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
        variables = vars,
    ) + fragment.copts + fragment.conlyopts + defaults.copts
    ldflags = cc_common.get_memory_inefficient_command_line(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_EXECUTABLE_ACTION_NAME,
        variables = vars,
    ) + fragment.linkopts + defaults.linkopts

    # Override the toolchain’s “-undefined dynamic_lookup” option so that the
    # configure script doesn’t incorrectly detect absent functions as present.
    remove = []
    for i, s in enumerate(ldflags[:-1]):
        if s == "-undefined" and ldflags[i + 1] == "dynamic_lookup":
            remove += [i, i + 1]
    for i in reversed(remove):
        ldflags.pop(i)

    # The build process needs to find some common binaries like “make” or the
    # GNU coreutils.
    env = {"PATH": "/usr/bin:/bin"}
    for action in (C_COMPILE_ACTION_NAME, CPP_LINK_EXECUTABLE_ACTION_NAME):
        env |= cc_common.get_environment_variables(
            feature_configuration = feature_configuration,
            action_name = action,
            variables = vars,
        )
    install = ctx.actions.declare_directory("_" + ctx.label.name)
    args = ctx.actions.args()
    if archive:
        args.add(archive, format = "--archive=%s")
        args.add(strip_prefix, format = "--strip-prefix=%s")
    if readme:
        args.add(readme, format = "--readme=%s")
    args.add(install.path, format = "--install=%s")
    args.add(shell_toolchain.path, format = "--bash=%s")
    args.add(cc, format = "--cc=%s")
    args.add_joined(
        cflags,
        join_with = " ",
        map_each = _munge_msvc_flag,
        format_joined = "--cflags=%s",
        omit_if_empty = False,
        expand_directories = False,
    )
    args.add_joined(
        ldflags,
        join_with = " ",
        format_joined = "--ldflags=%s",
        omit_if_empty = False,
        expand_directories = False,
    )
    secondary_outs = []
    if ctx.outputs.module_header:
        args.add(ctx.outputs.module_header, format = "--module-header=%s")
        secondary_outs.append(ctx.outputs.module_header)
    if ctx.outputs.builtin_features:
        args.add(ctx.outputs.builtin_features, format = "--builtin-features=%s")
        secondary_outs.append(ctx.outputs.builtin_features)
    ctx.actions.run(
        outputs = [install] + secondary_outs,
        inputs = depset(
            direct = ctx.files.srcs,
            transitive = [cc_toolchain.all_files],
        ),
        executable = ctx.executable._build,
        arguments = [args],
        mnemonic = "EmacsInstall",
        progress_message = "Installing Emacs into %{output}",
        env = env,
        toolchain = None,
    )
    return install

def _unpack(ctx, archive, *, strip_prefix = ""):
    """Unpacks Emacs from a release archive.

    Args:
      ctx (ctx): rule context
      archive (File): location of the Emacs release archive to unpack
      strip_prefix (str): prefix to strip from the files in the archive

    Returns:
      a File representing the Emacs installation directory
    """
    install = ctx.actions.declare_directory("_" + ctx.label.name)
    args = ctx.actions.args()
    args.add("--release")
    args.add(archive, format = "--archive=%s")
    args.add(strip_prefix, format = "--strip-prefix=%s")
    args.add(install.path, format = "--install=%s")
    secondary_outs = []
    if ctx.outputs.module_header:
        args.add(ctx.outputs.module_header, format = "--module-header=%s")
        secondary_outs.append(ctx.outputs.module_header)
    if ctx.outputs.builtin_features:
        args.add(ctx.outputs.builtin_features, format = "--builtin-features=%s")
        secondary_outs.append(ctx.outputs.builtin_features)
    ctx.actions.run(
        outputs = [install] + secondary_outs,
        inputs = ctx.files.srcs,
        executable = ctx.executable._build,
        arguments = [args],
        mnemonic = "EmacsInstall",
        progress_message = "Unpacking Emacs into %{output}",
        toolchain = None,
    )
    return install

def _munge_msvc_flag(s):
    # Crude way to work around specifying Visual C++ options in .bazelrc.
    return None if s.startswith("/external:") else s.replace("/std:c", "-std=gnu")
