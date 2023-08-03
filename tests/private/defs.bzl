# Copyright 2020, 2021, 2022, 2023 Google LLC
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

"""Unit tests for defs.bzl."""

load("@bazel_skylib//lib:unittest.bzl", "analysistest", "asserts")
load("//elisp:defs.bzl", "EmacsLispInfo", "elisp_library")

# Test for the EmacsLispInfo provider.

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
            for_runfiles = "phst_rules_elisp",
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
            for_runfiles = "phst_rules_elisp",
        )],
    )
    return analysistest.end(env)

provider_test = analysistest.make(_provider_test_impl)

def _test_provider():
    elisp_library(
        name = "provider_test_subject",
        srcs = ["provider-test.el"],
        tags = ["manual"],
    )
    provider_test(
        name = "provider_test",
        target_under_test = ":provider_test_subject",
        size = "small",
    )

def elisp_test_suite(*, name):
    _test_provider()
    native.test_suite(
        name = name,
        tests = [":provider_test"],
    )
