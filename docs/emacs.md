<!-- Generated with Stardoc: http://skydoc.bazel.build -->

<a id="#emacs_binary"></a>

## emacs_binary

<pre>
emacs_binary(<a href="#emacs_binary-name">name</a>, <a href="#emacs_binary-dump_mode">dump_mode</a>, <a href="#emacs_binary-module_header">module_header</a>, <a href="#emacs_binary-readme">readme</a>, <a href="#emacs_binary-srcs">srcs</a>)
</pre>

Builds Emacs from a source repository.
The resulting executable can be used to run the compiled Emacs.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="emacs_binary-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| <a id="emacs_binary-dump_mode"></a>dump_mode |  Dumping mode that Emacs will use.  This can be either  <code>portable</code> to use the portable dumper introduced in Emacs 27, or <code>unexec</code> to use the legacy “unexec” dumper.  Starting with Emacs 27, <code>portable</code> is strongly recommended.   | String | optional | "portable" |
| <a id="emacs_binary-module_header"></a>module_header |  Label for a file target that will receive the <code>emacs-module.h</code> header.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |
| <a id="emacs_binary-readme"></a>readme |  The README file in the root of the Emacs repository. This is necessary to determine the source root directory.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |
| <a id="emacs_binary-srcs"></a>srcs |  All Emacs source files.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | required |  |


