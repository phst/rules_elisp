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

"""Defines the `elisp_test` rule."""

load("@rules_cc//cc:find_cc_toolchain.bzl", "CC_TOOLCHAIN_ATTRS", "use_cc_toolchain")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")
load("//elisp/private:binary.bzl", "binary")
load("//elisp/private:cc_launcher_config.bzl", "LAUNCHER_DEPS")
load("//elisp/private:compile.bzl", "COMPILE_ATTRS")

visibility("public")

def _elisp_test_impl(ctx):
    """Rule implementation for the “elisp_test” rule."""
    toolchain = ctx.toolchains[Label("//elisp:toolchain_type")]

    args = [
        "--skip-test=" + test
        for test in ctx.attr.skip_tests
    ] + [
        "--skip-tag=" + tag
        for tag in ctx.attr.skip_tags
    ]
    if ctx.var["COMPILATION_MODE"] != "opt":
        args.append("--module-assertions")
    executable, runfiles = binary(
        ctx,
        srcs = ctx.files.srcs,
        # “local = 1” is equivalent to adding a “local” tag,
        # cf. https://bazel.build/reference/be/common-definitions#test.local.
        tags = ["local"] if ctx.attr.local else [],
        args = args,
    )

    # We include the original source files in the runfiles so that error
    # messages in tests can link back to them.
    runfiles = runfiles.merge(ctx.runfiles(files = ctx.files.srcs))

    test_env = {}
    if ctx.configuration.coverage_enabled:
        # Bazel’s coverage runner
        # (https://github.com/bazelbuild/bazel/blob/7.4.1/tools/test/collect_coverage.sh)
        # needs a binary called “lcov_merge.”  Its location is passed in the
        # LCOV_MERGER environmental variable.  For builtin rules, this variable
        # is set automatically based on a magic “$lcov_merger” or
        # “:lcov_merger” attribute, but it’s not possible to create such
        # attributes in Starlark.  Therefore we specify the variable ourselves.
        # Note that the coverage runner runs in the runfiles root instead of
        # the execution root, therefore we use “path” instead of “short_path.”
        runfiles = runfiles.merge(
            ctx.attr._lcov_merger[DefaultInfo].default_runfiles,
        )
        test_env["LCOV_MERGER"] = ctx.executable._lcov_merger.path

        # C/C++ coverage instrumentation needs another binary that wraps gcov;
        # see
        # https://github.com/bazelbuild/bazel/blob/7.4.1/tools/test/collect_coverage.sh#L183.
        # This is normally set from a hidden “$collect_cc_coverage” attribute;
        # see
        # https://github.com/bazelbuild/bazel/blob/7.4.1/src/main/java/com/google/devtools/build/lib/analysis/test/TestActionBuilder.java#L259-L263.
        # We also need to inject its location here, like above.
        runfiles = runfiles.merge(
            ctx.attr._collect_cc_coverage[DefaultInfo].default_runfiles,
        )
        test_env["CC_CODE_COVERAGE_SCRIPT"] = ctx.executable._collect_cc_coverage.path

    # The InstrumentedFilesInfo provider needs to be added here as well as in
    # the “elisp_library” rule for coverage collection to work.
    return [
        DefaultInfo(
            executable = executable,
            runfiles = runfiles,
        ),
        testing.ExecutionInfo(toolchain.execution_requirements),
        testing.TestEnvironment(test_env),
        coverage_common.instrumented_files_info(
            ctx,
            source_attributes = ["srcs"],
            dependency_attributes = ["deps", "srcs"],
        ),
    ]

elisp_test = rule(
    # FIXME: Remove CC_TOOLCHAIN_ATTRS once
    # https://github.com/bazelbuild/bazel/issues/7260 is fixed.
    # @unsorted-dict-items
    attrs = CC_TOOLCHAIN_ATTRS | COMPILE_ATTRS | {
        "srcs": attr.label_list(
            allow_empty = False,
            doc = "List of source files to load.",
            allow_files = [".el"],
            mandatory = True,
            # Undocumented flag to make these rules work with
            # “bazel build --compile_one_dependency”.  See
            # https://github.com/bazelbuild/bazel/blob/7.4.1/src/test/java/com/google/devtools/build/lib/pkgcache/CompileOneDependencyTransformerTest.java#L74.
            flags = ["DIRECT_COMPILE_TIME_INPUT"],
        ),
        "_launcher_deps": attr.label_list(
            default = LAUNCHER_DEPS + [Label("//elisp/private/tools:test")],
            providers = [CcInfo],
        ),
        # Magic coverage attributes.  This is only partially documented
        # (https://bazel.build/rules/lib/coverage#output_generator), but we can
        # take over the values from
        # https://github.com/bazelbuild/rules_python/blob/0.39.0/python/private/py_test_rule.bzl.
        "_lcov_merger": attr.label(
            default = configuration_field("coverage", "output_generator"),
            executable = True,
            cfg = "exec",
        ),
        "_collect_cc_coverage": attr.label(
            default = Label("@bazel_tools//tools/test:collect_cc_coverage"),
            executable = True,
            cfg = "exec",
        ),
        "data": attr.label_list(
            doc = "List of files to be made available at runtime.",
            allow_files = True,
        ),
        "deps": attr.label_list(
            doc = "List of `elisp_library` dependencies.",
            providers = [EmacsLispInfo],
        ),
        "skip_tests": attr.string_list(
            doc = """List of tests to skip.  This attribute contains a list of
ERT test symbols; when running the test rule, these tests are skipped.

Most of the time, you should use the `skip-unless` macro instead; see
[Tests and Their Environment](<info:ert#Tests and Their Environment>).
The `skip_tests` attribute is mainly useful for third-party code that
you don’t control.""",
        ),
        "skip_tags": attr.string_list(
            doc = """List of test tags to skip.  This attribute contains a list
of tag names; if a test is tagged with one of the tags from this list, it is
skipped.  This can be useful to e.g. skip tests that are flaky or only work in
interactive mode.  Use the `:tags` keyword argument to `ert-deftest` to tag
tests.""",
        ),
    },
    doc = """Runs ERT tests that are defined in the source files.
The given source files should contain ERT tests defined with `ert-deftest`.
For details, see [How to Write Tests](<info:ert#How to Write Tests>).
The generated test binary loads all source files and executes all
tests like `ert-run-tests-batch-and-exit`.

You can restrict the tests to be run using the `--test_filter` option.  If set,
the value of `--test_filter` must be a Lisp expression usable as an
ERT test selector; see [Test Selectors](<info:ert#Test Selectors>).
You can also restrict the tests to be run using the `skip_tests` and
`skip_tags` rule attributes.  These restrictions are additive, i.e., a test
only runs if it’s not suppressed by either facility.

In coverage mode (i.e., when run under `bazel coverage`), all tests tagged with
the `:nocover` tag are also skipped.  You can use this tag to skip tests that
normally pass, but don’t work under coverage for some reason.

Test files can access additional command-line arguments passed via the `args`
attribute or the `--test_arg` Bazel option via the `command-line-args-left`
variable.  After processing known arguments, test files must remove them from
`command-line-args-left` so that it’s empty after all test files are loaded.
Emacs will not automatically process these arguments using
`command-switch-alist` or `command-line-functions`.""",
    fragments = ["cpp"],
    host_fragments = ["cpp"],
    test = True,
    toolchains = use_cc_toolchain() + [Label("//elisp:toolchain_type")],
    implementation = _elisp_test_impl,
)
