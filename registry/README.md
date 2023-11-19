This directory contains a [Bazel index registry][] used for local development
and testing.  This allows the main `phst_rules_elisp` workspace and the external
example workspace in the `examples/ext` subdirectory to depend on each other via
custom `--registry` flags in the respective `.bazelrc` files.  Starting with
Bazel 7, this should no longer be necessary because Bazel 7 includes [commit
78cb7d5][] that will allow `local_path_override` in non-root modules.  While we
still support Bazel 5 and 6, we need to keep this registry around.

[Bazel index registry]: https://bazel.build/external/registry#index_registry
[commit 78cb7d5]: https://github.com/bazelbuild/bazel/commit/78cb7d5b652ee155a0d1ad5cef3a3131e9705152
