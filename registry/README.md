This directory contains a [Bazel index registry][] used for local development
and testing.  This allows the main `phst_rules_elisp` workspace and the external
example workspace in the `examples/ext` subdirectory to depend on each other via
custom `--registry` flags in the respective `.bazelrc` files.  Starting with
Bazel 6.3, this should no longer be necessary because Bazel 7 includes [PR
#18388][] that will allow `local_path_override` in non-root modules.  While we
still support Bazel 6.2 and below, we need to keep this registry around.

[Bazel index registry]: https://bazel.build/external/registry#index_registry
[PR #18388]: https://github.com/bazelbuild/bazel/pull/18388
