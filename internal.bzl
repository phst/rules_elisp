# Copyright 2021 Google LLC
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

"""Internal-only rules."""

def _requirements_txt_impl(repository_ctx):
    """Implementation of the “copy_requirements_txt” repository rule."""
    prefixes = {
        "linux": "linux",
        "mac os x": "macos",
        "windows server 2019": "windows",
    }
    prefix = prefixes.get(repository_ctx.os.name, None)
    if not prefix:
        fail("Unsupported operating system “{}”".format(repository_ctx.os.name))
    repository_ctx.symlink(
        Label("@//:{}-requirements.txt".format(prefix)),
        "requirements.txt",
    )
    repository_ctx.file(
        "BUILD",
        'exports_files(["requirements.txt"])',
        executable = False,
    )

requirements_txt = repository_rule(
    implementation = _requirements_txt_impl,
    local = True,
    doc = "Generates requirements.txt for the current platform.",
)

def _pylint_impl(target, ctx):
    if "nolint" in ctx.rule.attr.tags:
        return []
    info = target[PyInfo]
    stem = "_{}.pylint".format(target.label.name)
    params_file = ctx.actions.declare_file(stem + ".json")
    output_file = ctx.actions.declare_file(stem + ".hash")
    _write_params(ctx.actions, params_file, output_file, info)
    rcfile = ctx.file._rcfile
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = [params_file, rcfile],
            transitive = [info.transitive_sources],
        ),
        executable = ctx.executable._pylint,
        arguments = [
            "--rcfile=" + rcfile.path,
            "--params=" + params_file.path,
        ],
        mnemonic = "Pylint",
        progress_message = "linting target {}".format(target.label),
    )
    return [OutputGroupInfo(pylint = depset([output_file]))]

def _pytype_impl(target, ctx):
    if "notype" in ctx.rule.attr.tags:
        return []
    info = target[PyInfo]
    stem = "_{}.pytype".format(target.label.name)
    params_file = ctx.actions.declare_file(stem + ".json")
    output_file = ctx.actions.declare_file(stem + ".hash")
    _write_params(ctx.actions, params_file, output_file, info)
    ctx.actions.run(
        outputs = [output_file],
        inputs = depset(
            direct = [params_file],
            transitive = [info.transitive_sources],
        ),
        executable = ctx.executable._pytype,
        arguments = ["--params=" + params_file.path],
        mnemonic = "Pytype",
        progress_message = "type-checking target {}".format(target.label),
    )
    return [OutputGroupInfo(pytype = depset([output_file]))]

pylint = aspect(
    implementation = _pylint_impl,
    attrs = {
        "_pylint": attr.label(
            default = "//:run_pylint",
            executable = True,
            cfg = "exec",
        ),
        "_rcfile": attr.label(
            default = "//:.pylintrc",
            allow_single_file = True,
        ),
    },
    required_providers = [PyInfo],
)

pytype = aspect(
    implementation = _pytype_impl,
    attrs = {
        "_pytype": attr.label(
            default = "//:run_pytype",
            executable = True,
            cfg = "exec",
        ),
    },
    required_providers = [PyInfo],
)

def _write_params(actions, params_file, output_file, info):
    params = struct(
        out = output_file.path,
        srcs = [
            struct(
                rel = file.short_path,
                src = file.path,
                ext = bool(file.owner.workspace_name),
            )
            for file in info.transitive_sources.to_list()
        ],
        path = info.imports.to_list(),
    )
    actions.write(params_file, json.encode(params))
