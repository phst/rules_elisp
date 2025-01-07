# Copyright 2023, 2024, 2025 Google LLC
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

"""Non-module dependencies."""

load("@bazel_skylib//lib:modules.bzl", "modules")

visibility("private")

def _non_module_dev_deps_impl(ctx):
    ctx.download_and_extract(
        sha256 = "ba809d0fedfb392cc604ad38aff7db7d750b77eaf5fed977a51360fa4a6dffdf",
        url = [
            "https://github.com/windyroad/JUnit-Schema/archive/refs/tags/1.0.0.tar.gz",  # 2022-04-09
        ],
        stripPrefix = "JUnit-Schema-1.0.0/",
    )
    ctx.template(
        "BUILD.bazel",
        Label("//:junit_xsd.BUILD"),
        {
            '"[tests_pkg]"': repr(str(Label("//tests:__pkg__"))),
        },
        executable = False,
    )

_non_module_dev_deps = repository_rule(
    doc = """Installs development dependencies that are not available as modules.""",
    implementation = _non_module_dev_deps_impl,
)

def _emacs_repos(*, version, integrity, windows_integrity):
    major, _, _ = version.partition(".")
    _emacs_repository(
        name = "gnu_emacs_" + version,
        path = "/emacs/emacs-{}.tar.xz".format(version),
        integrity = integrity,
        output = "emacs.tar.xz",
        strip_prefix = "emacs-{}".format(version),
        mode = "source",
    )
    _emacs_repository(
        name = "gnu_emacs_windows_" + version,
        path = "/emacs/windows/emacs-{}/emacs-{}.zip".format(major, version),
        integrity = windows_integrity,
        output = "emacs.zip",
        strip_prefix = "emacs-{}".format(version) if major == "28" else "",
        mode = "release",
        target_compatible_with = [
            Label("@platforms//os:windows"),
            Label("@platforms//cpu:x86_64"),
        ],
    )

def _emacs_repository_impl(ctx):
    path = ctx.attr.path
    output = ctx.attr.output
    ctx.download(
        integrity = ctx.attr.integrity,
        url = [
            "https://ftpmirror.gnu.org" + path,
            "https://ftp.gnu.org/gnu" + path,
        ],
        output = output,
    )
    ctx.template(
        "BUILD.bazel",
        Label("//:emacs.BUILD"),
        {
            '"[defs_bzl]"': repr(str(Label("//emacs:defs.bzl"))),
            '"[emacs_pkg]"': repr(str(Label("//emacs:__pkg__"))),
            '"[src]"': repr(output),
            '"[strip_prefix]"': repr(ctx.attr.strip_prefix),
            '"[mode]"': repr(ctx.attr.mode),
            "[[compatible_with]]": repr([str(label) for label in ctx.attr.target_compatible_with]),
        },
        executable = False,
    )

_emacs_repository = repository_rule(
    attrs = {
        "path": attr.string(mandatory = True),
        "integrity": attr.string(mandatory = True),
        "output": attr.string(mandatory = True),
        "strip_prefix": attr.string(),
        "mode": attr.string(mandatory = True, values = ["source", "release"]),
        "target_compatible_with": attr.label_list(),
    },
    implementation = _emacs_repository_impl,
)

def _local_emacs_repo_impl(ctx):
    windows = ctx.os.name.startswith("windows")
    emacs = ctx.getenv("EMACS", "emacs")
    if windows and not emacs.lower().endswith(".exe"):
        emacs += ".exe"
    sep = "\\" if windows else "/"
    if sep not in emacs:
        emacs = ctx.which(emacs)

    # Don’t fail during the loading phase if Emacs isn’t locally installed, only
    # when Emacs is actually needed.
    if emacs:
        ctx.symlink(emacs, "source.exe")
    ctx.template(
        "BUILD.bazel",
        Label("//:local.BUILD.template"),
        {
            '"[native_binary.bzl]"': repr(str(Label("@bazel_skylib//rules:native_binary.bzl"))),
            '"[elisp_pkg]"': repr(str(Label("//elisp:__pkg__"))),
        },
        executable = False,
    )

_local_emacs_repo = repository_rule(
    implementation = _local_emacs_repo_impl,
    local = True,
)

_local_emacs = tag_class(
    attrs = {
        "name": attr.string(mandatory = True),
    },
)

_emacs = tag_class(
    attrs = {
        "version": attr.string(mandatory = True),
        "integrity": attr.string(mandatory = True),
        "windows_integrity": attr.string(mandatory = True),
    },
)

def _deps_impl(ctx):
    for module in ctx.modules:
        for emacs in module.tags.emacs:
            _emacs_repos(
                version = emacs.version,
                integrity = emacs.integrity,
                windows_integrity = emacs.windows_integrity,
            )
        for local_emacs in module.tags.local_emacs:
            _local_emacs_repo(name = local_emacs.name)

def _dev_deps_impl():
    _non_module_dev_deps(name = "phst_rules_elisp_dev_deps")

deps = module_extension(
    tag_classes = {
        "emacs": _emacs,
        "local_emacs": _local_emacs,
    },
    implementation = _deps_impl,
)
dev_deps = modules.as_extension(_dev_deps_impl)
