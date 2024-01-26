# Copyright 2020, 2021, 2022, 2023, 2024 Google LLC
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

"""Defines the rule `emacs_binary`, which compiles Emacs for use in Bazel."""

load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_LINK_EXECUTABLE_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "use_cpp_toolchain")
load(
    "//private:defs.bzl",
    "CcDefaultInfo",
    "cc_launcher",
    "cpp_string",
    "runfile_location",
)

visibility("public")

def _emacs_binary_impl(ctx):
    """Rule implementation of the “emacs_binary” rule."""
    emacs_cc_toolchain = ctx.attr._emacs_cc_toolchain[cc_common.CcToolchainInfo]

    # It’s not possible to refer to a directory as a label, so we refer to a
    # known file (README in the source root) instead.
    install = _install(ctx, emacs_cc_toolchain, ctx.file.readme)
    executable, runfiles = cc_launcher(
        ctx,
        defines = [
            "RULES_ELISP_ARGS=" + cpp_string(
                "--install=" + runfile_location(ctx, install),
            ),
        ],
    )
    return [
        DefaultInfo(
            executable = executable,
            files = depset(direct = [executable]),
            runfiles = ctx.runfiles(files = [install]).merge(runfiles),
        ),
    ]

emacs_binary = rule(
    attrs = {
        "srcs": attr.label_list(
            allow_files = True,
            allow_empty = False,
            mandatory = True,
            doc = "All Emacs source files.",
        ),
        "readme": attr.label(
            allow_single_file = True,
            mandatory = True,
            doc = """The README file in the root of the Emacs repository.
This is necessary to determine the source root directory.""",
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
            default = Label("//emacs:build"),
            executable = True,
            cfg = "exec",
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_emacs_cc_toolchain": attr.label(
            default = Label("//emacs:cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_launcher_deps": attr.label_list(
            default = [Label("//elisp:emacs")],
            providers = [CcInfo],
        ),
        "_launcher_srcs": attr.label_list(
            default = [Label("//emacs:launcher.cc")],
            allow_files = [".cc"],
        ),
        "_launcher_defaults": attr.label(
            default = Label("//elisp:launcher_defaults"),
            providers = [CcDefaultInfo],
        ),
        "_emacs_defaults": attr.label(
            default = Label("//emacs:defaults"),
            providers = [CcDefaultInfo],
        ),
    },
    doc = """Builds Emacs from a source repository.
The resulting executable can be used to run the compiled Emacs.""",
    executable = True,
    fragments = ["cpp"],
    toolchains = use_cpp_toolchain(),
    implementation = _emacs_binary_impl,
)

def _install(ctx, cc_toolchain, readme):
    """Builds and install Emacs.

    Args:
      ctx (ctx): rule context
      cc_toolchain (Provider): the C toolchain to use to compile Emacs
      readme (File): location of the README file in the Emacs source tree

    Returns:
      a File representing the Emacs installation directory
    """
    defaults = ctx.attr._emacs_defaults[CcDefaultInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = defaults.features + ctx.features,
        unsupported_features = ctx.disabled_features + [
            # Never instrument Emacs itself for coverage collection.  It doesn’t
            # hurt, but leads to needless reinstall actions when switching
            # between “bazel test” and “bazel coverage”.
            "coverage",
            # Don’t link against the C++ standard library, as Emacs is pure C
            # code.
            "default_link_libs",
        ],
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

    # The build process needs to find some common binaries like “make” or the
    # GNU coreutils.
    env = {"PATH": "/usr/bin:/bin"}
    for action in (C_COMPILE_ACTION_NAME, CPP_LINK_EXECUTABLE_ACTION_NAME):
        env.update(cc_common.get_environment_variables(
            feature_configuration = feature_configuration,
            action_name = action,
            variables = vars,
        ))
    install = ctx.actions.declare_directory("_" + ctx.label.name)
    args = ctx.actions.args()
    args.add(readme, format = "--readme=%s")
    args.add(install.path, format = "--install=%s")
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
    outs = [install]
    if ctx.outputs.module_header:
        args.add(ctx.outputs.module_header, format = "--module-header=%s")
        outs.append(ctx.outputs.module_header)
    if ctx.outputs.builtin_features:
        args.add(ctx.outputs.builtin_features, format = "--builtin-features=%s")
        outs.append(ctx.outputs.builtin_features)
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [ctx.attr._build])
    ctx.actions.run(
        outputs = outs,
        inputs = depset(
            direct = ctx.files.srcs,
            transitive = [cc_toolchain.all_files, tool_inputs],
        ),
        executable = ctx.executable._build,
        arguments = [args],
        mnemonic = "EmacsInstall",
        progress_message = (
            "Installing Emacs into {}".format(install.short_path)
        ),
        env = env,
        input_manifests = input_manifests,
        toolchain = None,
    )
    return install

def _munge_msvc_flag(s):
    # Crude way to work around specifying Visual C++ options in .bazelrc.
    return s.replace("/std:c", "-std=gnu")
