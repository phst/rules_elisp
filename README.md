# Bazel rules for Emacs Lisp

This repository provides a [Bazel][] integration for [Emacs Lisp][].  It is
modeled after the rules definitions for other languages, like the [C++
rules][].

This is not an officially supported Google product.

[Bazel]: https://bazel.build/
[Emacs Lisp]: https://www.gnu.org/software/emacs/manual/html_node/elisp/
[C++ rules]: https://docs.bazel.build/versions/3.0.0/be/c-cpp.html

## Usage

Add a snippet like the following to your Bazel `WORKSPACE` file:

```python
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "phst_rules_elisp",
    urls = ["https://github.com/phst/rules_elisp/archive/e422123d39398d7561745de72c6f07c15bb0477e.zip"],
    sha256 = "07693ff0e276948008812c077d3cfedfe0398c2e607f021120bedc49723b51ec",
    strip_prefix = "rules_elisp-e422123d39398d7561745de72c6f07c15bb0477e",
)

load("@phst_rules_elisp//elisp:repositories.bzl", "rules_elisp_dependencies", "rules_elisp_toolchains")

rules_elisp_dependencies()

rules_elisp_toolchains()
```

Then you can use the `elisp_library`, `elisp_binary`, and `elisp_test` rules.
See the [generated documentation][] and the examples in the `examples`
directory for details.

[generated documentation]: documentation/elisp_defs.md

## Load path management

The Emacs Lisp rules by default only add the workspace root directories to the
[load path][].  However, many Emacs Lisp libraries assume that their immediate
parent directory is present in the load path.  To support such libraries, the
`elisp_library` rule supports an optional `load_path` attribute.  You can
specify additional load path directories using this attribute; the directories
are relative to the Bazel package directory.  A typical use case is to specify
`load_path = ["."]` to add the current package to the load path.

[load path]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html

## Runfiles

This repository also includes a library to access [runfiles][].  To use it, add
a build dependency on `@phst_rules_elisp//elisp/runfiles`.  See the header
comments in `runfiles.el` for further usage hints.

[runfiles]: https://docs.bazel.build/versions/3.0.0/skylark/rules.html#runfiles
