<!-- Generated with Stardoc: http://skydoc.bazel.build -->

<a name="#emacs_binary"></a>

## emacs_binary

<pre>
emacs_binary(<a href="#emacs_binary-name">name</a>, <a href="#emacs_binary-readme">readme</a>, <a href="#emacs_binary-srcs">srcs</a>)
</pre>

Builds Emacs from a source repository.
The resulting executable can be used to run the compiled Emacs.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :-------------: | :-------------: | :-------------: | :-------------: | :-------------: |
| name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| readme |  The README file in the root of the Emacs repository. This is necessary to determine the source root directory.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |
| srcs |  All Emacs source files.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | required |  |


