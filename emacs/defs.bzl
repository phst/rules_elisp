# Copyright 2020, 2021, 2022 Google LLC
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
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load(
    "//private:defs.bzl",
    "CcDefaultInfo",
    "cc_launcher",
    "cpp_strings",
    "runfile_location",
)

def _emacs_binary_impl(ctx):
    """Rule implementation of the “emacs_binary” rule."""
    cc_toolchain = find_cpp_toolchain(ctx)
    emacs_cc_toolchain = ctx.attr._emacs_cc_toolchain[cc_common.CcToolchainInfo]

    # It’s not possible to refer to a directory as a label, so we refer to a
    # known file (README in the source root) instead.
    readme = ctx.file.readme
    source = "./{}/{}".format(readme.root.path, readme.dirname)
    install = _install(ctx, emacs_cc_toolchain, source)
    launcher_src = ctx.actions.declare_file("_" + ctx.label.name + ".cc")
    ctx.actions.expand_template(
        template = ctx.file._template,
        output = launcher_src,
        substitutions = {
            "[[args]]": cpp_strings([
                "--install=" + runfile_location(ctx, install),
            ]),
        },
        is_executable = True,
    )
    executable, runfiles = cc_launcher(
        ctx,
        cc_toolchain,
        launcher_src,
        ctx.attr._emacs_libs,
    )
    return [DefaultInfo(
        executable = executable,
        files = depset(direct = [executable]),
        runfiles = ctx.runfiles(files = [install]).merge(runfiles),
    )]

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
            default = "//emacs:build",
            executable = True,
            cfg = "exec",
        ),
        "_cc_toolchain": attr.label(
            default = "@bazel_tools//tools/cpp:current_cc_toolchain",
            providers = [cc_common.CcToolchainInfo],
        ),
        "_emacs_cc_toolchain": attr.label(
            default = "@phst_rules_elisp_toolchains//:emacs_cc_toolchain",
            providers = [cc_common.CcToolchainInfo],
        ),
        "_grep_includes": attr.label(
            allow_single_file = True,
            executable = True,
            cfg = "exec",
            default = Label("@bazel_tools//tools/cpp:grep-includes"),
        ),
        "_emacs_libs": attr.label_list(
            default = ["//elisp:emacs"],
            providers = [CcInfo],
        ),
        "_template": attr.label(
            default = "//emacs:launcher.template",
            allow_single_file = [".template"],
        ),
        "_launcher_defaults": attr.label(
            default = "//elisp:launcher_defaults",
            providers = [CcDefaultInfo],
        ),
        "_emacs_defaults": attr.label(
            default = ":defaults",
            providers = [CcDefaultInfo],
        ),
    },
    doc = """Builds Emacs from a source repository.
The resulting executable can be used to run the compiled Emacs.""",
    executable = True,
    fragments = ["cpp"],
    toolchains = ["@bazel_tools//tools/cpp:toolchain_type"],
    incompatible_use_toolchain_transition = True,
    implementation = _emacs_binary_impl,
)

def _install(ctx, cc_toolchain, source):
    """Builds and install Emacs.

    Args:
      ctx (ctx): rule context
      cc_toolchain (Provider): the C toolchain to use to compile Emacs
      source (File): the directory containing the Emacs source tree

    Returns:
      a File representing the Emacs installation directory
    """
    defaults = ctx.attr._emacs_defaults[CcDefaultInfo]
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = defaults.features + ctx.features,
        # Never instrument Emacs itself for coverage collection.  It doesn’t
        # hurt, but leads to needless reinstall actions when switching between
        # “bazel test” and “bazel coverage”.
        unsupported_features = ctx.disabled_features + ["coverage"],
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
    install = ctx.actions.declare_directory(
        "_{}_install".format(ctx.label.name),
    )
    args = [
        "--source=" + source,
        "--install=" + install.path,
        "--cc=" + cc,
        "--cflags=" + " ".join(cflags),
        "--ldflags=" + " ".join(ldflags),
    ]
    outs = [install]
    if ctx.outputs.module_header:
        args.append("--module-header=" + ctx.outputs.module_header.path)
        outs.append(ctx.outputs.module_header)
    if ctx.outputs.builtin_features:
        args.append("--builtin-features=" + ctx.outputs.builtin_features.path)
        outs.append(ctx.outputs.builtin_features)
    tool_inputs, input_manifests = ctx.resolve_tools(tools = [ctx.attr._build])
    ctx.actions.run(
        outputs = outs,
        inputs = depset(
            direct = ctx.files.srcs,
            transitive = [cc_toolchain.all_files, tool_inputs],
        ),
        executable = ctx.executable._build,
        arguments = args,
        mnemonic = "EmacsInstall",
        progress_message = (
            "Installing Emacs into {}".format(install.short_path)
        ),
        env = env,
        input_manifests = input_manifests,
    )
    return install
