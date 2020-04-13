<!-- Generated with Stardoc: http://skydoc.bazel.build -->

<a name="#elisp_binary"></a>

## elisp_binary

<pre>
elisp_binary(<a href="#elisp_binary-name">name</a>, <a href="#elisp_binary-data">data</a>, <a href="#elisp_binary-deps">deps</a>, <a href="#elisp_binary-src">src</a>)
</pre>

Binary rule that loads a single Emacs Lisp file.
The source file is byte-compiled.  At runtime, the compiled version is loaded in batch mode.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :-------------: | :-------------: | :-------------: | :-------------: | :-------------: |
| name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| data |  List of files to be made available at runtime.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| deps |  List of <code>elisp_library</code> dependencies.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| src |  Source file to load.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |


<a name="#elisp_library"></a>

## elisp_library

<pre>
elisp_library(<a href="#elisp_library-name">name</a>, <a href="#elisp_library-data">data</a>, <a href="#elisp_library-deps">deps</a>, <a href="#elisp_library-load_path">load_path</a>, <a href="#elisp_library-srcs">srcs</a>)
</pre>

Byte-compiles Emacs Lisp source files and makes the compiled output available to dependencies.
All sources are byte-compiled.  `elisp_library`, `elisp_binary`, and `elisp_test` rules depending on this binary
can then use `load` or `require` to load them.

By default, libraries need to be loaded using a filename relative to the workspace root, i.e.,
<var>package</var>/<var>file</var>.
If you want to add further elements to the load path, use the `load_path` attribute.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :-------------: | :-------------: | :-------------: | :-------------: | :-------------: |
| name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| data |  List of files to be made available at runtime.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| deps |  List of <code>elisp_library</code> dependencies.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| load_path |  List of additional load path elements. The elements are directory names relative to the current package. To add a load path entry for the current package, specify <code>.</code> here.   | List of strings | optional | [] |
| srcs |  List of source files.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | required |  |


<a name="#elisp_test"></a>

## elisp_test

<pre>
elisp_test(<a href="#elisp_test-name">name</a>, <a href="#elisp_test-data">data</a>, <a href="#elisp_test-deps">deps</a>, <a href="#elisp_test-srcs">srcs</a>)
</pre>

Runs ERT tests that are defined in the source files.
The given source files should contain ERT tests defined with `ert_test`.
The generated test binary loads all source files and executes all tests like `ert-run-tests-batch-and-exit`.
You can restrict the tests to be run using the `--test_filter` option.  If set, the value of
`--test_filter` must be a Lisp expression usable as an
[ERT test selector](https://www.gnu.org/software/emacs/manual/html_node/ert/Test-Selectors.html).

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :-------------: | :-------------: | :-------------: | :-------------: | :-------------: |
| name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| data |  List of files to be made available at runtime.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| deps |  List of <code>elisp_library</code> dependencies.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| srcs |  List of source files to load.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | required |  |


<a name="#elisp_toolchain"></a>

## elisp_toolchain

<pre>
elisp_toolchain(<a href="#elisp_toolchain-name">name</a>, <a href="#elisp_toolchain-emacs">emacs</a>, <a href="#elisp_toolchain-use_default_shell_env">use_default_shell_env</a>)
</pre>

Toolchain rule for Emacs Lisp.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :-------------: | :-------------: | :-------------: | :-------------: | :-------------: |
| name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| emacs |  An executable file that behaves like the Emacs binary.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |
| use_default_shell_env |  Whether actions should inherit the external shell environment.   | Boolean | optional | False |


<a name="#EmacsLispInfo"></a>

## EmacsLispInfo

<pre>
EmacsLispInfo(<a href="#EmacsLispInfo-transitive_compiled_files">transitive_compiled_files</a>, <a href="#EmacsLispInfo-transitive_load_path">transitive_load_path</a>)
</pre>

Provider for Emacs Lisp libraries.  The `elisp_library` rule produces this provider.

**FIELDS**


| Name  | Description |
| :-------------: | :-------------: |
| transitive_compiled_files |  A <code>depset</code> of <code>File</code> objects containing the byte-compiled Emacs Lisp files of this library and all its transitive dependencies.    |
| transitive_load_path |  A <code>depset</code> containing necessary load path additions for this library and all its transitive dependencies. The <code>depset</code> uses preorder traversal: entries for libraries closer to the root of the dependency graph come first. The <code>depset</code> elements are structures with the following fields:<br><br>- <code>for_actions</code> is a string specifying the load directory to use for actions, relative to the execution root.<br><br>- <code>for_runfiles</code> is a string specifying the load directory to use at runtime, relative to the runfiles root.    |


