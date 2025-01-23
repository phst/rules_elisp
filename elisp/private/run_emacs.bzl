# Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

"""Defines the internal `run_emacs` function."""

visibility("private")

def run_emacs(
        ctx,
        *,
        arguments,
        inputs,
        outputs,
        tags,
        mnemonic,
        progress_message,
        manifest_basename,
        manifest_sibling = None,
        manifest_load_path = None):
    """Runs Emacs with the default toolchain, wrapping it if necessary.

    Most parameters are mostly passed directly to ctx.actions.run.  The
    command-line arguments are prefixed with
    `--quick --batch --no-build-details` and `--wrap` as necessary.

    Args:
      ctx (ctx): rule context
      arguments (list of strings or Args objects): command-line arguments
      inputs (depset of File objects): input files
      outputs (list of File objects): output files
      tags (list of strings): list of rule tags to write into the manifest
      mnemonic (str): one-word action mnemonic
      progress_message (str): progress message
      manifest_basename (str): base name of the manifest file without extension
      manifest_sibling (File or None): file to use as sibling for the manifest
      manifest_load_path: (list of strings or None): additional load path for
          manifest with directories relative to the execution root
    """
    toolchain = ctx.toolchains[Label("//elisp:toolchain_type")]
    emacs = toolchain.emacs
    arguments = [
        ctx.actions.args().add("--quick").add("--batch").add("--no-build-details"),
    ] + arguments
    if toolchain.wrap:
        manifest = ctx.actions.declare_file(
            manifest_basename + ".manifest.json",
            sibling = manifest_sibling,
        )
        ctx.actions.write(
            output = manifest,
            content = json.encode(struct(
                root = "EXECUTION_ROOT",
                loadPath = manifest_load_path,
                inputFiles = [
                    f.path
                    for f in inputs.to_list()
                    # Exclude middlemen, which don’t exist in the filesystem.
                    if not f.short_path.startswith("_middlemen/")
                ],
                outputFiles = [f.path for f in outputs],
                tags = tags,
            )),
        )
        arguments = [
            ctx.actions.args().add(manifest, format = "--manifest=%s").add("--"),
        ] + arguments
        manifests = depset([manifest])
    else:
        manifests = depset()
    ctx.actions.run(
        outputs = outputs,
        # Add manifest after the actual inputs so that the progress message can
        # use %{input} as usual.
        inputs = depset(transitive = [inputs, manifests], order = "preorder"),
        executable = emacs.files_to_run,
        arguments = arguments,
        mnemonic = mnemonic,
        progress_message = progress_message,
        use_default_shell_env = toolchain.use_default_shell_env,
        execution_requirements = toolchain.execution_requirements,
        toolchain = Label("//elisp:toolchain_type"),
    )
