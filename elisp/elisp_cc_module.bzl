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

"""Defines the `elisp_cc_module` rule."""

load("@rules_cc//cc:find_cc_toolchain.bzl", "find_cc_toolchain", "use_cc_toolchain")
load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")
load("//elisp/private:load_path.bzl", "resolve_load_path")
load("//elisp/private:module_config_info.bzl", "ModuleConfigInfo")
load(
    "//private:defs.bzl",
    "CcDefaultInfo",
)

visibility("public")

def _elisp_cc_module_impl(ctx):
    """Implementation of the `elisp_cc_module` rule."""
    cc_toolchain = find_cc_toolchain(ctx)
    deps = ctx.attr.deps + [ctx.attr._module_header]
    infos = [dep[CcInfo] for dep in deps]
    defaults = ctx.attr._module_config[CcDefaultInfo]
    config = ctx.attr._module_config[ModuleConfigInfo]
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
        srcs = ctx.files.srcs,
        compilation_contexts = [info.compilation_context for info in infos],
        local_defines = defaults.defines + ctx.attr.local_defines,
        user_compile_flags = defaults.copts + ctx.attr.copts,
    )
    out = cc_common.link(
        name = ctx.label.name,
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = objs,
        linking_contexts = [info.linking_context for info in infos],
        output_type = "dynamic_library",
        user_link_flags = defaults.linkopts + ctx.attr.linkopts,
        additional_inputs = config.additional_linker_inputs,
    )
    if not (out.library_to_link and out.library_to_link.dynamic_library):
        fail("linking Emacs module didn’t produce a dynamic library")

    # Ensure that the library file has the expected name.
    filename = ctx.label.name + config.suffix
    if out.library_to_link.dynamic_library.basename == filename:
        lib = out.library_to_link.dynamic_library
    else:
        lib = ctx.actions.declare_file(filename)
        ctx.actions.symlink(
            output = lib,
            target_file = out.library_to_link.dynamic_library,
            progress_message = "Creating symbolic link " + lib.short_path,
        )

    # Replicate some implementation details of cc_binary to make coverage work,
    # at least with llvm-cov.  See
    # https://github.com/bazelbuild/bazel/issues/15974.
    metadata_files = []
    if ctx.configuration.coverage_enabled and ctx.coverage_instrumented():
        # @bazel_tools//tools/test:collect_cc_coverage.sh requires a file whose
        # name ends in “runtime_objects_list.txt”.
        objects_list = ctx.actions.declare_file(ctx.label.name + ".runtime_objects_list.txt")
        ctx.actions.write(objects_list, lib.path + "\n")
        metadata_files += [lib, objects_list]

    load_path = [resolve_load_path(ctx, "")]
    return [
        DefaultInfo(
            files = depset([lib]),
            runfiles = ctx.runfiles(ctx.files.data).merge_all([
                dep[DefaultInfo].default_runfiles
                for dep in ctx.attr.deps
            ]),
        ),
        EmacsLispInfo(
            source_files = [lib],
            compiled_files = [lib],
            load_path = load_path,
            data_files = ctx.files.data,
            transitive_source_files = depset([lib]),
            transitive_compiled_files = depset([lib]),
            transitive_load_path = depset(load_path),
        ),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps"],
            metadata_files = metadata_files,
        ),
    ]

elisp_cc_module = rule(
    doc = """Builds an Emacs dynamic module from C and C++ source files.
For background on Emacs modules, see
[Emacs Dynamic Modules](<info:elisp#Dynamic Modules>), and see
[Writing Dynamically-Loaded Modules](<info:elisp#Writing Dynamic Modules>).
The `emacs_module` rule is similar to
[`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library),
but it always builds a dynamic library (shared object) that can be loaded
into Emacs.  The module can also be used directly as dependency
for `elisp_library`, `elisp_binary`, and `elisp_test` rules.
The dynamic library will only export the symbols necessary for Emacs modules,
`plugin_is_GPL_compatible` and `emacs_module_init`; see
[Module Initialization Code](<info:elisp#Module Initialization>).
The file name of the dynamic library will be
<code><var>name</var>.<var>extension</var></code>, where <var>name</var>
is the rule name as specified in the `name` attribute, and <var>extension</var>
is the correct extension for dynamic libraries for the target operating system.
The file name will not be prefixed with `lib`.

The library code in `srcs` can include a recent version of `emacs-module.h`
using

```c
#include "emacs-module.h"
```

to implement module functions.""",
    attrs = {
        "srcs": attr.label_list(
            doc = """C and C++ source files for the module.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.srcs).""",
            allow_files = [".cc", ".c"],
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/7.4.1/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L74.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        "deps": attr.label_list(
            doc = """`cc_library` targets that the module depends on.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.deps).""",
            providers = [CcInfo],
        ),
        "data": attr.label_list(
            doc = """Data dependencies to make available at runtime.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.data).""",
            allow_files = True,
        ),
        "copts": attr.string_list(
            doc = """Additional options to pass to the C/C++ compiler.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.copts).""",
        ),
        "linkopts": attr.string_list(
            doc = """Additional options to pass to the C/C++ linker.
See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.linkopts).""",
        ),
        "local_defines": attr.string_list(
            doc = """Additional preprocessor definitions to pass to the
C/C++ compiler.  See the [corresponding attribute for
`cc_library`](https://bazel.build/reference/be/c-cpp#cc_library.local_defines).""",
        ),
        # FIXME: Remove once https://github.com/bazelbuild/bazel/issues/7260 is
        # fixed.
        "_cc_toolchain": attr.label(
            default = Label("@rules_cc//cc:current_cc_toolchain"),
            providers = [cc_common.CcToolchainInfo],
        ),
        "_module_header": attr.label(
            default = Label("//emacs:module_header"),
            providers = [CcInfo],
        ),
        "_module_config": attr.label(
            default = Label("//elisp:module_config"),
            providers = [CcDefaultInfo, ModuleConfigInfo],
        ),
    },
    provides = [EmacsLispInfo],
    fragments = ["cpp"],
    toolchains = use_cc_toolchain(),
    implementation = _elisp_cc_module_impl,
)
