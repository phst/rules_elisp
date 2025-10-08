# Copyright 2020-2023, 2025 Google LLC
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

"""Unit tests for //elisp/common:elisp_info.bzl."""

load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts", "unittest")
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo")

visibility("private")

# Test for the EmacsLispInfo provider.

def _elisp_info_unit_test(ctx):
    env = unittest.begin(ctx)
    srcs = _files(ctx.actions, suffix = "el", count = 3)
    outs = _files(ctx.actions, suffix = "elc", count = 3)
    data = _files(ctx.actions, suffix = "dat", count = 2)
    dir = struct(for_actions = ".", for_runfiles = "_main")
    info = EmacsLispInfo(
        source_files = srcs,
        compiled_files = outs,
        load_path = [dir],
        data_files = data,
        transitive_source_files = depset(srcs),
        transitive_compiled_files = depset(outs),
        transitive_load_path = depset([dir]),
    )
    asserts.equals(env, info.source_files, srcs)
    asserts.equals(env, info.compiled_files, outs)
    asserts.equals(env, info.load_path, [dir])
    asserts.equals(env, info.data_files, data)
    asserts.equals(env, sorted(info.transitive_source_files.to_list()), sorted(srcs))
    asserts.equals(env, sorted(info.transitive_compiled_files.to_list()), sorted(outs))
    asserts.equals(env, info.transitive_load_path.to_list(), [dir])
    return unittest.end(env)

elisp_info_unit_test = unittest.make(_elisp_info_unit_test)

def _provider_test_impl(ctx):
    env = analysistest.begin(ctx)
    target_under_test = analysistest.target_under_test(env)
    info = target_under_test[EmacsLispInfo]
    asserts.equals(
        env,
        actual = [f.path for f in info.source_files],
        expected = ["tests/provider-test.el"],
    )
    asserts.equals(
        env,
        actual = [(f.root, f.short_path) for f in info.compiled_files],
        expected = [(ctx.bin_dir, "tests/provider-test.elc")],
    )
    asserts.equals(
        env,
        actual = info.load_path,
        expected = [struct(
            for_actions = ctx.bin_dir.path,
            for_runfiles = ctx.workspace_name,
        )],
    )
    asserts.equals(env, actual = info.data_files, expected = [])
    asserts.equals(
        env,
        actual = [f.path for f in info.transitive_source_files.to_list()],
        expected = ["tests/provider-test.el"],
    )
    asserts.equals(
        env,
        actual = [
            (f.root, f.short_path)
            for f in info.transitive_compiled_files.to_list()
        ],
        expected = [(ctx.bin_dir, "tests/provider-test.elc")],
    )
    asserts.equals(
        env,
        actual = info.transitive_load_path.to_list(),
        expected = [struct(
            for_actions = ctx.bin_dir.path,
            for_runfiles = ctx.workspace_name,
        )],
    )
    return analysistest.end(env)

provider_test = analysistest.make(_provider_test_impl)

def _files(actions, *, suffix, count):
    files = []
    for i in range(count):
        file = actions.declare_file("%d.%s" % (i, suffix))
        actions.write(file, "", is_executable = False)
        files.append(file)
    return files
