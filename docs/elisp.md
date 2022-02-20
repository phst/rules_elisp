<!-- Generated with Stardoc: http://skydoc.bazel.build -->

Defines rules to work with Emacs Lisp files in Bazel.

<a id="#elisp_binary"></a>

## elisp_binary

<pre>
elisp_binary(<a href="#elisp_binary-name">name</a>, <a href="#elisp_binary-data">data</a>, <a href="#elisp_binary-deps">deps</a>, <a href="#elisp_binary-fatal_warnings">fatal_warnings</a>, <a href="#elisp_binary-input_args">input_args</a>, <a href="#elisp_binary-interactive">interactive</a>, <a href="#elisp_binary-output_args">output_args</a>, <a href="#elisp_binary-src">src</a>)
</pre>

Binary rule that loads a single Emacs Lisp file.
The source file is byte-compiled.  At runtime, the compiled version is loaded
in batch mode unless `interactive` is `True`.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="elisp_binary-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| <a id="elisp_binary-data"></a>data |  List of files to be made available at runtime.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| <a id="elisp_binary-deps"></a>deps |  List of <code>elisp_library</code> dependencies.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| <a id="elisp_binary-fatal_warnings"></a>fatal_warnings |  If <code>True</code> (the default), then byte compile warnings should be treated as errors.  If <code>False</code>, they still show up in the output, but don’t cause the compilation to fail.  Most targets should leave this attribute as <code>True</code>, because otherwise important issues might remain undetected.  Set this attribute to <code>False</code> only for integrating third-party libraries that don’t compile cleanly and that you don’t control.   | Boolean | optional | True |
| <a id="elisp_binary-input_args"></a>input_args |  Indices of command-line arguments that represent input filenames.  These number specify indices into the <code>argv</code> array.  Negative indices are interpreted as counting from the end of the array.  For example, the index <code>2</code> stands for <code>argv[2]</code>, and the index <code>-2</code> stands for <code>argv[argc - 2]</code>.  When passing arguments to an <code>emacs_binary</code> program on the command line, the corresponding arguments are treated as filenames for input files and added to the <code>inputFiles</code> field of the manifest.  This only has an effect for toolchains that specify <code>wrap = True</code>.   | List of integers | optional | [] |
| <a id="elisp_binary-interactive"></a>interactive |  Run Emacs in interactive instead of batch mode.   | Boolean | optional | False |
| <a id="elisp_binary-output_args"></a>output_args |  Indices of command-line arguments that represent output filenames.  These number specify indices into the <code>argv</code> array.  Negative indices are interpreted as counting from the end of the array.  For example, the index <code>2</code> stands for <code>argv[2]</code>, and the index <code>-2</code> stands for <code>argv[argc - 2]</code>.  When passing arguments to an <code>emacs_binary</code> program on the command line, the corresponding arguments are treated as filenames for output files and added to the <code>outputFiles</code> field of the manifest.  This only has an effect for toolchains that specify <code>wrap = True</code>.   | List of integers | optional | [] |
| <a id="elisp_binary-src"></a>src |  Source file to load.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |


<a id="#elisp_library"></a>

## elisp_library

<pre>
elisp_library(<a href="#elisp_library-name">name</a>, <a href="#elisp_library-data">data</a>, <a href="#elisp_library-deps">deps</a>, <a href="#elisp_library-fatal_warnings">fatal_warnings</a>, <a href="#elisp_library-load_path">load_path</a>, <a href="#elisp_library-outs">outs</a>, <a href="#elisp_library-srcs">srcs</a>)
</pre>

Byte-compiles Emacs Lisp source files and makes the compiled output
available to dependencies. All sources are byte-compiled.
`elisp_library`, `elisp_binary`, and `elisp_test` rules depending on this binary
can then use `load` or `require` to load them.

By default, libraries need to be loaded using a filename relative to the
workspace root, i.e., <var>package</var>/<var>file</var>.  If you want to add
further elements to the load path, use the `load_path` attribute.

If there are multiple source files specified in `srcs`, these source files can
also load each other.  However, it’s often preferable to only have one
`elisp_library` target per source file to make dependencies more obvious and
ensure that files get only loaded in their byte-compiled form.

The source files in `srcs` can also list shared objects.  The rule treats them
as Emacs modules and doesn’t try to byte-compile them.  You can use
e.g. `cc_binary` with `linkshared = True` to create shared objects.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="elisp_library-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| <a id="elisp_library-data"></a>data |  List of files to be made available at runtime.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| <a id="elisp_library-deps"></a>deps |  List of <code>elisp_library</code> dependencies.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| <a id="elisp_library-fatal_warnings"></a>fatal_warnings |  If <code>True</code> (the default), then byte compile warnings should be treated as errors.  If <code>False</code>, they still show up in the output, but don’t cause the compilation to fail.  Most targets should leave this attribute as <code>True</code>, because otherwise important issues might remain undetected.  Set this attribute to <code>False</code> only for integrating third-party libraries that don’t compile cleanly and that you don’t control.   | Boolean | optional | True |
| <a id="elisp_library-load_path"></a>load_path |  List of additional load path elements. The elements are directory names, which can be either relative or absolute. Relative names are relative to the current package. Absolute names are relative to the workspace root. To add a load path entry for the current package, specify <code>.</code> here.   | List of strings | optional | [] |
| <a id="elisp_library-outs"></a>outs |  List of byte-compiled Emacs Lisp files to be made available as targets.   | List of labels | optional |  |
| <a id="elisp_library-srcs"></a>srcs |  List of source files.  These must either be Emacs Lisp files ending in <code>.el</code>, or module objects ending in <code>.so</code>, <code>.dylib</code>, or <code>.dll</code>.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | required |  |


<a id="#elisp_manual"></a>

## elisp_manual

<pre>
elisp_manual(<a href="#elisp_manual-name">name</a>, <a href="#elisp_manual-out">out</a>, <a href="#elisp_manual-src">src</a>)
</pre>

Generates a [GNU_Texinfo](https://www.gnu.org/software/texinfo/)
manual from an [Org Mode file](https://orgmode.org/) using
[Org’s exporting functionality](https://orgmode.org/manual/Exporting.html).
You can then use
[`texi2any`](https://www.gnu.org/software/texinfo/manual/texinfo/html_node/Generic-Translator-texi2any.html)
to generate other document formats from the output file.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="elisp_manual-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| <a id="elisp_manual-out"></a>out |  Texinfo manual file to write; must end in <code>.texi</code>.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |
| <a id="elisp_manual-src"></a>src |  Org-mode file to use as manual source; must end in <code>.org</code>.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |


<a id="#elisp_test"></a>

## elisp_test

<pre>
elisp_test(<a href="#elisp_test-name">name</a>, <a href="#elisp_test-data">data</a>, <a href="#elisp_test-deps">deps</a>, <a href="#elisp_test-fatal_warnings">fatal_warnings</a>, <a href="#elisp_test-skip_tags">skip_tags</a>, <a href="#elisp_test-skip_tests">skip_tests</a>, <a href="#elisp_test-srcs">srcs</a>)
</pre>

Runs ERT tests that are defined in the source files.
The given source files should contain ERT tests defined with `ert-deftest`.
See the [ERT
manual](https://www.gnu.org/software/emacs/manual/html_node/ert/How-to-Write-Tests.html)
for details.  The generated test binary loads all source files and executes all
tests like `ert-run-tests-batch-and-exit`.

You can restrict the tests to be run using the `--test_filter` option.  If set,
the value of `--test_filter` must be a Lisp expression usable as an [ERT test
selector](https://www.gnu.org/software/emacs/manual/html_node/ert/Test-Selectors.html).
You can also restrict the tests to be run using the `skip_tests` and
`skip_tags` rule attributes.  These restrictions are additive, i.e., a test
only runs if it’s not suppressed by either facility.

In coverage mode (i.e., when run under `bazel coverage`), all tests tagged with
the `:nocover` tag are also skipped.  You can use this tag to skip tests that
normally pass, but don’t work under coverage for some reason.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="elisp_test-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| <a id="elisp_test-data"></a>data |  List of files to be made available at runtime.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| <a id="elisp_test-deps"></a>deps |  List of <code>elisp_library</code> dependencies.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | optional | [] |
| <a id="elisp_test-fatal_warnings"></a>fatal_warnings |  If <code>True</code> (the default), then byte compile warnings should be treated as errors.  If <code>False</code>, they still show up in the output, but don’t cause the compilation to fail.  Most targets should leave this attribute as <code>True</code>, because otherwise important issues might remain undetected.  Set this attribute to <code>False</code> only for integrating third-party libraries that don’t compile cleanly and that you don’t control.   | Boolean | optional | True |
| <a id="elisp_test-skip_tags"></a>skip_tags |  List of test tags to skip.  This attribute contains a list of tag names; if a test is tagged with one of the tags from this list, it is skipped.  This can be useful to e.g. skip tests that are flaky or only work in interactive mode.  Use the <code>:tags</code> keyword argument to <code>ert-deftest</code> to tag tests.   | List of strings | optional | [] |
| <a id="elisp_test-skip_tests"></a>skip_tests |  List of tests to skip.  This attribute contains a list of ERT test symbols; when running the test rule, these tests are skipped.<br><br>Most of the time, you should use [the <code>skip-unless</code> macro](https://www.gnu.org/software/emacs/manual/html_node/ert/Tests-and-Their-Environment.html) instead.  The <code>skip_tests</code> attribute is mainly useful for third-party code that you don’t control.   | List of strings | optional | [] |
| <a id="elisp_test-srcs"></a>srcs |  List of source files to load.   | <a href="https://bazel.build/docs/build-ref.html#labels">List of labels</a> | required |  |


<a id="#elisp_toolchain"></a>

## elisp_toolchain

<pre>
elisp_toolchain(<a href="#elisp_toolchain-name">name</a>, <a href="#elisp_toolchain-emacs">emacs</a>, <a href="#elisp_toolchain-execution_requirements">execution_requirements</a>, <a href="#elisp_toolchain-use_default_shell_env">use_default_shell_env</a>, <a href="#elisp_toolchain-wrap">wrap</a>)
</pre>

Toolchain rule for Emacs Lisp.
This toolchain configures how to run Emacs.
The executable passed to the `emacs` attribute must be a binary
that behaves like Emacs.
If `wrap` is `False`, Bazel calls it as is, passing arguments
that a normal Emacs binary would accept.
If `wrap` is `True`, Bazel calls the binary with a special `--manifest` option.
The value of the option is the filename of a JSON file containing a manifest.
The manifest specifies which files should be readable and/or writable by Emacs.
Toolchains can use this to sandbox Emacs, if desired.

If `wrap` is `True`, the format of the command line is as follows:

```bash
emacs --manifest=MANIFEST -- ARGS…
```

That is, the original arguments for Emacs are separated by a double hyphen
(`--`) so that argument parsers can distinguish between the `--manifest` option
and Emacs arguments.

The manifest is a JSON object with the following keys:
- `root` can be either `EXECUTION_ROOT` or `RUNFILES_ROOT` and specifies
  the root directory for relative file names.
- `loadPath` is a list of directory names making up the load path.
- `inputFiles` is a list of files that should be readable.
- `outputFiles` is a list of files that should be writable.
- `tags` is the list of tags for the current rule.

When executing an action, file names are relative to the execution root.
Otherwise, file names are relative to the runfiles root.  File names in
`inputFiles` or `outputFiles` can also be absolute; in this case they specify
temporary files that are deleted after the action completes, or files passed on
the command line interpreted according to the `input_args` and `output_args`
attributes of the `elisp_binary` rule.

**ATTRIBUTES**


| Name  | Description | Type | Mandatory | Default |
| :------------- | :------------- | :------------- | :------------- | :------------- |
| <a id="elisp_toolchain-name"></a>name |  A unique name for this target.   | <a href="https://bazel.build/docs/build-ref.html#name">Name</a> | required |  |
| <a id="elisp_toolchain-emacs"></a>emacs |  An executable file that behaves like the Emacs binary. Depending on whether <code>wrap</code> is <code>True</code>, Bazel invokes this executable with a command line like <code>emacs --manifest=MANIFEST -- ARGS…</code> or <code>emacs ARGS…</code>. The <code>--manifest</code> flag is only present if <code>wrap</code> is <code>True</code>. See the rule documentation for details.   | <a href="https://bazel.build/docs/build-ref.html#labels">Label</a> | required |  |
| <a id="elisp_toolchain-execution_requirements"></a>execution_requirements |  Execution requirements for compilation and test actions.   | <a href="https://bazel.build/docs/skylark/lib/dict.html">Dictionary: String -> String</a> | optional | {} |
| <a id="elisp_toolchain-use_default_shell_env"></a>use_default_shell_env |  Whether actions should inherit the external shell environment.   | Boolean | optional | False |
| <a id="elisp_toolchain-wrap"></a>wrap |  Whether the binary given in the <code>emacs</code> attribute is a wrapper around Emacs proper. If <code>True</code>, Bazel passes a manifest file using the <code>--manifest</code> option. See the rule documentation for details.   | Boolean | optional | False |


<a id="#EmacsLispInfo"></a>

## EmacsLispInfo

<pre>
EmacsLispInfo(<a href="#EmacsLispInfo-source_files">source_files</a>, <a href="#EmacsLispInfo-compiled_files">compiled_files</a>, <a href="#EmacsLispInfo-load_path">load_path</a>, <a href="#EmacsLispInfo-data_files">data_files</a>, <a href="#EmacsLispInfo-transitive_source_files">transitive_source_files</a>,
              <a href="#EmacsLispInfo-transitive_compiled_files">transitive_compiled_files</a>, <a href="#EmacsLispInfo-transitive_load_path">transitive_load_path</a>)
</pre>

Provider for Emacs Lisp libraries.
The `elisp_library` rule produces this provider.

Load path directory entries are structures with the following fields:
- `for_actions` is a string specifying the load directory to use for actions,
  relative to the execution root.
- `for_runfiles` is a string specifying the load directory to use at runtime,
  relative to the runfiles root.

**FIELDS**


| Name  | Description |
| :------------- | :------------- |
| <a id="EmacsLispInfo-source_files"></a>source_files |  A list of <code>File</code> objects containing the Emacs Lisp source files of this library.    |
| <a id="EmacsLispInfo-compiled_files"></a>compiled_files |  A list of <code>File</code> objects containing the byte-compiled Emacs Lisp files and module objects of this library.    |
| <a id="EmacsLispInfo-load_path"></a>load_path |  A list containing necessary load path additions for this library.  The list elements are structures as described in the provider documentation.    |
| <a id="EmacsLispInfo-data_files"></a>data_files |  A list of <code>File</code> objects that this library requires at runtime.    |
| <a id="EmacsLispInfo-transitive_source_files"></a>transitive_source_files |  A <code>depset</code> of <code>File</code> objects containing the Emacs Lisp source files of this library and all its transitive dependencies.    |
| <a id="EmacsLispInfo-transitive_compiled_files"></a>transitive_compiled_files |  A <code>depset</code> of <code>File</code> objects containing the byte-compiled Emacs Lisp files and module objects of this library and all its transitive dependencies.    |
| <a id="EmacsLispInfo-transitive_load_path"></a>transitive_load_path |  A <code>depset</code> containing necessary load path additions for this library and all its transitive dependencies. The <code>depset</code> uses preorder traversal: entries for libraries closer to the root of the dependency graph come first.  The <code>depset</code> elements are structures as described in the provider documentation.    |


