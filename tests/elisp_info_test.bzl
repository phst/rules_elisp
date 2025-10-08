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
load("//elisp/common:elisp_info.bzl", "EmacsLispInfo", "merge_elisp_infos")

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
    asserts.equals(env, actual = info.source_files, expected = srcs, msg = "incorrect source_files")
    asserts.equals(env, actual = info.compiled_files, expected = outs, msg = "incorrect compiled_files")
    asserts.equals(env, actual = info.load_path, expected = [dir], msg = "incorrect load_path")
    asserts.equals(env, actual = info.data_files, expected = data, msg = "incorrect data_files")
    asserts.equals(env, actual = sorted(info.transitive_source_files.to_list()), expected = sorted(srcs), msg = "incorrect transitive_source_files")
    asserts.equals(env, actual = sorted(info.transitive_compiled_files.to_list()), expected = sorted(outs), msg = "incorrect transitive_compiled_files")
    asserts.equals(env, actual = info.transitive_load_path.to_list(), expected = [dir], msg = "incorrect transitive_load_path")
    return unittest.end(env)

elisp_info_unit_test = unittest.make(_elisp_info_unit_test)

def _merge_elisp_infos_direct_test(ctx):
    env = unittest.begin(ctx)
    s1, s2, s3 = _files(ctx.actions, suffix = "el", count = 3)
    o1, o2, o3 = _files(ctx.actions, suffix = "elc", count = 3)
    d1, d2 = _files(ctx.actions, suffix = "dat", count = 2)
    dir = struct(for_actions = ".", for_runfiles = "_main")
    a = EmacsLispInfo(
        source_files = [s1, s2],
        compiled_files = [o1, o2],
        load_path = [dir],
        data_files = [d1],
        transitive_source_files = depset([s1, s2]),
        transitive_compiled_files = depset([o1, o2]),
        transitive_load_path = depset([dir]),
    )
    b = EmacsLispInfo(
        source_files = [s3, s1],
        compiled_files = [o3, o1],
        load_path = [dir],
        data_files = [d2],
        transitive_source_files = depset([s3, s1]),
        transitive_compiled_files = depset([o3, o1]),
        transitive_load_path = depset([dir]),
    )
    m = merge_elisp_infos(direct = [b, b, a])
    asserts.equals(env, actual = sorted(m.source_files), expected = [s1, s2, s3], msg = "incorrect source_files")
    asserts.equals(env, actual = sorted(m.compiled_files), expected = [o1, o2, o3], msg = "incorrect compiled_files")
    asserts.equals(env, actual = m.load_path, expected = [dir], msg = "incorrect load_path")
    asserts.equals(env, actual = sorted(m.data_files), expected = [d1, d2], msg = "incorrect data_files")
    asserts.equals(env, actual = sorted(m.transitive_source_files.to_list()), expected = [s1, s2, s3], msg = "incorrect transitive_source_files")
    asserts.equals(env, actual = sorted(m.transitive_compiled_files.to_list()), expected = [o1, o2, o3], msg = "incorrect transitive_compiled_files")
    asserts.equals(env, actual = m.transitive_load_path.to_list(), expected = [dir], msg = "incorrect transitive_load_path")
    return unittest.end(env)

merge_elisp_infos_direct_test = unittest.make(_merge_elisp_infos_direct_test)

def _merge_elisp_infos_transitive_test(ctx):
    env = unittest.begin(ctx)
    s1, s2, s3 = _files(ctx.actions, suffix = "el", count = 3)
    o1, o2, o3 = _files(ctx.actions, suffix = "elc", count = 3)
    d1, d2 = _files(ctx.actions, suffix = "dat", count = 2)
    dir = struct(for_actions = ".", for_runfiles = "_main")
    a = EmacsLispInfo(
        source_files = [s1, s2],
        compiled_files = [o1, o2],
        load_path = [dir],
        data_files = [d1],
        transitive_source_files = depset([s1, s2]),
        transitive_compiled_files = depset([o1, o2]),
        transitive_load_path = depset([dir]),
    )
    b = EmacsLispInfo(
        source_files = [s3, s1],
        compiled_files = [o3, o1],
        load_path = [dir],
        data_files = [d2],
        transitive_source_files = depset([s3, s1]),
        transitive_compiled_files = depset([o3, o1]),
        transitive_load_path = depset([dir]),
    )
    m = merge_elisp_infos(transitive = [a, b, a])
    asserts.equals(env, actual = m.source_files, expected = [], msg = "incorrect source_files")
    asserts.equals(env, actual = m.compiled_files, expected = [], msg = "incorrect compiled_files")
    asserts.equals(env, actual = m.load_path, expected = [], msg = "incorrect load_path")
    asserts.equals(env, actual = m.data_files, expected = [], msg = "incorrect data_files")
    asserts.equals(env, actual = sorted(m.transitive_source_files.to_list()), expected = [s1, s2, s3], msg = "incorrect transitive_source_files")
    asserts.equals(env, actual = sorted(m.transitive_compiled_files.to_list()), expected = [o1, o2, o3], msg = "incorrect transitive_compiled_files")
    asserts.equals(env, actual = m.transitive_load_path.to_list(), expected = [dir], msg = "incorrect transitive_load_path")
    return unittest.end(env)

merge_elisp_infos_transitive_test = unittest.make(_merge_elisp_infos_transitive_test)

def _merge_elisp_infos_mixed_test(ctx):
    env = unittest.begin(ctx)
    s1, s2, s3 = _files(ctx.actions, suffix = "el", count = 3)
    o1, o2, o3 = _files(ctx.actions, suffix = "elc", count = 3)
    d1, d2 = _files(ctx.actions, suffix = "dat", count = 2)
    dir = struct(for_actions = ".", for_runfiles = "_main")
    a = EmacsLispInfo(
        source_files = [s1, s2],
        compiled_files = [o1, o2],
        load_path = [dir],
        data_files = [d1],
        transitive_source_files = depset([s1, s2]),
        transitive_compiled_files = depset([o1, o2]),
        transitive_load_path = depset([dir]),
    )
    b = EmacsLispInfo(
        source_files = [s3, s1],
        compiled_files = [o3, o1],
        load_path = [dir],
        data_files = [d2],
        transitive_source_files = depset([s3, s1]),
        transitive_compiled_files = depset([o3, o1]),
        transitive_load_path = depset([dir]),
    )
    m = merge_elisp_infos(direct = [b], transitive = [a])
    asserts.equals(env, actual = sorted(m.source_files), expected = [s1, s3], msg = "incorrect source_files")
    asserts.equals(env, actual = sorted(m.compiled_files), expected = [o1, o3], msg = "incorrect compiled_files")
    asserts.equals(env, actual = m.load_path, expected = [dir], msg = "incorrect load_path")
    asserts.equals(env, actual = sorted(m.data_files), expected = [d2], msg = "incorrect data_files")
    asserts.equals(env, actual = sorted(m.transitive_source_files.to_list()), expected = [s1, s2, s3], msg = "incorrect transitive_source_files")
    asserts.equals(env, actual = sorted(m.transitive_compiled_files.to_list()), expected = [o1, o2, o3], msg = "incorrect transitive_compiled_files")
    asserts.equals(env, actual = m.transitive_load_path.to_list(), expected = [dir], msg = "incorrect transitive_load_path")
    return unittest.end(env)

merge_elisp_infos_mixed_test = unittest.make(_merge_elisp_infos_mixed_test)

def _provider_test_impl(ctx):
    env = analysistest.begin(ctx)
    target_under_test = analysistest.target_under_test(env)
    info = target_under_test[EmacsLispInfo]
    asserts.equals(
        env,
        actual = [f.path for f in info.source_files],
        expected = ["tests/provider-test.el"],
        msg = "incorrect source_files",
    )
    asserts.equals(
        env,
        actual = [(f.root, f.short_path) for f in info.compiled_files],
        expected = [(ctx.bin_dir, "tests/provider-test.elc")],
        msg = "incorrect compiled_files",
    )
    asserts.equals(
        env,
        actual = info.load_path,
        expected = [struct(
            for_actions = ctx.bin_dir.path,
            for_runfiles = ctx.workspace_name,
        )],
        msg = "incorrect load_path",
    )
    asserts.equals(env, actual = info.data_files, expected = [], msg = "incorrect data_files")
    asserts.equals(
        env,
        actual = [f.path for f in info.transitive_source_files.to_list()],
        expected = ["tests/provider-test.el"],
        msg = "incorrect transitive_source_files",
    )
    asserts.equals(
        env,
        actual = [
            (f.root, f.short_path)
            for f in info.transitive_compiled_files.to_list()
        ],
        expected = [(ctx.bin_dir, "tests/provider-test.elc")],
        msg = "incorrect transitive_compiled_files",
    )
    asserts.equals(
        env,
        actual = info.transitive_load_path.to_list(),
        expected = [struct(
            for_actions = ctx.bin_dir.path,
            for_runfiles = ctx.workspace_name,
        )],
        msg = "incorrect transitive_load_path",
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
