# Copyright 2020-2025 Google LLC
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

"""Defines the `EmacsLispInfo` provider."""

load("@bazel_skylib//lib:types.bzl", "types")

visibility("public")

def _init_elisp_info(*, source_files, compiled_files, load_path, data_files, transitive_source_files, transitive_compiled_files, transitive_load_path):
    if not types.is_list(source_files):
        fail("source_files must be a list")
    if not types.is_list(compiled_files):
        fail("compiled_files must be a list")
    if not types.is_list(load_path):
        fail("load_path must be a list")
    if not types.is_list(data_files):
        fail("data_files must be a list")
    if not types.is_depset(transitive_source_files):
        fail("transitive_source_files must be a list")
    if not types.is_depset(transitive_compiled_files):
        fail("transitive_compiled_files must be a list")
    if not types.is_depset(transitive_load_path):
        fail("transitive_load_path must be a list")

    # @unsorted-dict-items
    return {
        "source_files": source_files,
        "compiled_files": compiled_files,
        "load_path": load_path,
        "data_files": data_files,
        "transitive_source_files": transitive_source_files,
        "transitive_compiled_files": transitive_compiled_files,
        "transitive_load_path": transitive_load_path,
    }

EmacsLispInfo, _ = provider(
    doc = """Provider for Emacs Lisp libraries.
The `elisp_library`, `elisp_proto_library`, and `elisp_cc_module` rules
produce this provider.

Load path directory entries are structures with the following fields:
- `for_actions` is a string specifying the load directory to use for actions,
  relative to the execution root.
- `for_runfiles` is a string specifying the load directory to use at runtime,
  relative to the runfiles root.""",
    # @unsorted-dict-items
    fields = {
        "source_files": """A list of `File` objects containing
the Emacs Lisp source files of this library.""",
        "compiled_files": """A list of `File` objects containing
the byte-compiled Emacs Lisp files and module objects of this library.""",
        "load_path": """A list containing necessary load path
additions for this library.  The list elements are structures as
described in the provider documentation.""",
        "data_files": """A list of `File` objects that this library requires
at runtime.""",
        "transitive_source_files": """A `depset` of `File` objects containing
the Emacs Lisp source files of this library
and all its transitive dependencies.""",
        "transitive_compiled_files": """A `depset` of `File` objects containing
the byte-compiled Emacs Lisp files and module objects of this library
and all its transitive dependencies.""",
        "transitive_load_path": """A `depset` containing necessary load path
additions for this library and all its transitive dependencies.
The `depset` uses preorder traversal: entries for libraries closer to the root
of the dependency graph come first.  The `depset` elements are structures as
described in the provider documentation.""",
    },
    init = _init_elisp_info,
)
