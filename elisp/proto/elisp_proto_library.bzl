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

"""Defines the `elisp_proto_library` rule."""

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@protobuf//bazel/common:proto_common.bzl", "proto_common")
load("@protobuf//bazel/common:proto_info.bzl", "ProtoInfo")
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")
load("//elisp/private:compile.bzl", "compile")
load(
    "//private:defs.bzl",
    "check_relative_filename",
    "repository_relative_filename",
)

visibility("public")

def _elisp_proto_aspect_impl(target, ctx):
    """Aspect implementation for the “elisp_proto_aspect” aspect."""
    info = target[ProtoInfo]
    srcs = proto_common.declare_generated_files(ctx.actions, info, ".proto.el")
    proto_common.compile(
        actions = ctx.actions,
        proto_info = info,
        proto_lang_toolchain_info = ctx.attr._proto_toolchain[proto_common.ProtoLangToolchainInfo],
        generated_files = srcs,
    )

    # TODO: We probably shouldn’t generate this bundle, but right now it’s
    # needed for compatibility.
    bundle = ctx.actions.declare_file(ctx.label.name + ".el")
    args = ctx.actions.args()
    args.add(bundle)
    args.add(str(ctx.label))
    args.add(_elisp_proto_feature(bundle))
    args.add_all(srcs, map_each = _elisp_proto_feature, uniquify = True, expand_directories = False)
    ctx.actions.run(
        outputs = [bundle],
        executable = ctx.executable._generate_bundle,
        arguments = [args],
        mnemonic = "GenElispProtoBundle",
        progress_message = "Generating Emacs Lisp protocol buffer bundle %{output}",
        toolchain = None,
    )

    load_dir = info.proto_source_root.removeprefix(ctx.bin_dir.path + "/")
    if load_dir.startswith("external/"):
        _, _, load_dir = load_dir.partition("/")
        _, _, load_dir = load_dir.partition("/")
    load_dir = "/" + check_relative_filename(load_dir)
    load_path = [load_dir]
    if ctx.label.package == "src/google/protobuf":
        # See the comment in _elisp_proto_feature why we’re doing this.
        load_path.append(".")
    result = compile(
        ctx = ctx,
        srcs = srcs + [bundle],
        deps = [ctx.attr._protobuf_lib] + ctx.rule.attr.deps,
        load_path = load_path,
        data = None,
        tags = ctx.rule.attr.tags,
        fatal_warnings = True,
    )
    return [
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps"],
            extensions = ["el"],
        ),
        EmacsLispInfo(
            source_files = srcs + [bundle],
            compiled_files = result.outs,
            load_path = result.load_path,
            data_files = ctx.rule.files.data,
            transitive_source_files = result.transitive_srcs,
            transitive_compiled_files = result.transitive_outs,
            transitive_load_path = result.transitive_load_path,
        ),
    ]

def _elisp_proto_feature(file):
    """Returns the Emacs feature name for a protocol buffer library."""
    stem, ext = paths.split_extension(repository_relative_filename(file))
    if ext != ".el":
        fail("invalid extension {}".format(ext))

    # Strip “virtual import” prefix.
    _, _, after = stem.partition("/_virtual_imports/")
    if after:
        _, _, stem = after.partition("/")

    # Work around https://github.com/bazelbuild/bazel/issues/11044 for built-in
    # types.
    # FIXME: It would probably better to keep the “google/protobuf/” prefix and
    # fix users instead.
    return stem.removeprefix("src/google/protobuf/")

def _elisp_proto_library_impl(ctx):
    """Rule implementation for the “elisp_proto_library” rule."""
    deps = ctx.attr.deps
    if len(deps) != 1:
        fail("exactly one proto_library in ‘deps’ required, got {}".format(len(deps)))
    dep = deps[0]

    # All work is done by the ‘elisp_proto_aspect’ aspect.
    info = dep[EmacsLispInfo]
    return [
        DefaultInfo(files = depset(info.source_files)),
        EmacsLispInfo(
            transitive_source_files = info.transitive_source_files,
            transitive_compiled_files = info.transitive_compiled_files,
            transitive_load_path = info.transitive_load_path,
        ),
    ]

# The protocol buffer aspect is private for now.
_elisp_proto_aspect = aspect(
    doc = "An aspect to generate protocol buffer libraries for Emacs Lisp.",
    attr_aspects = ["deps"],
    attrs = {
        "_compile": attr.label(
            default = Label("//elisp/private/tools:compile.elc"),
            allow_single_file = [".elc"],
        ),
        "_generate_bundle": attr.label(
            default = Label("//elisp/private/tools:gen_proto_bundle"),
            executable = True,
            cfg = "exec",
        ),
        "_protobuf_lib": attr.label(
            default = Label("//elisp/proto"),
            providers = [EmacsLispInfo],
        ),
        "_proto_toolchain": attr.label(
            default = Label("//elisp/proto:toolchain"),
            providers = [proto_common.ProtoLangToolchainInfo],
        ),
    },
    required_providers = [ProtoInfo],
    provides = [EmacsLispInfo],
    toolchains = [Label("//elisp:toolchain_type")],
    implementation = _elisp_proto_aspect_impl,
)

elisp_proto_library = rule(
    attrs = {
        "deps": attr.label_list(
            doc = "List of exactly one `proto_library` rule.",
            mandatory = True,
            allow_empty = False,
            providers = [ProtoInfo],
            aspects = [_elisp_proto_aspect],
        ),
    },
    doc = r"""Generates Emacs bindings for a protocol buffer library.
By convention, for a `proto_library` rule named
<code><var>prefix</var>\_proto</code> there should be a corresponding
`elisp_proto_library` rule named <code><var>prefix</var>\_elisp\_proto</code>.
Other `elisp_library`, `elisp_binary`, and `elisp_test` rules can then depend
on this rule.  This rule generates and byte-compiles Emacs Lisp representations
of the protocol buffer definitions listed in the `deps` attribute and all their
direct and indirect dependencies.  The feature symbol for `require` is
<code><var>package</var>/<var>name</var></code>, where
<code>//<var>package</var>:<var>name</var></code> is the label of the
corresponding `proto_library` rule.""",
    provides = [EmacsLispInfo],
    toolchains = [Label("//elisp:toolchain_type")],
    implementation = _elisp_proto_library_impl,
)
