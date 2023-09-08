;;; runner.el --- run ERT tests with Bazel      -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022, 2023 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This library runs ERT tests under Bazel.  It provides support for the
;; --test_filter flag, as described in
;; https://bazel.build/reference/test-encyclopedia#initial-conditions.

;;; Code:

(require 'backtrace)
(require 'bytecomp)
(require 'cl-lib)
(require 'cl-macs)
(require 'debug)
(require 'edebug)
(require 'ert)
(require 'ert-x)
(require 'format)
(require 'nadvice)
(require 'pp)
(require 'rx)
(require 'subr-x)
(require 'trampver)  ; load eagerly to work around https://debbugs.gnu.org/11218
(require 'warnings)
(require 'xml)

(add-to-list 'command-switch-alist
             (cons "--test-source" #'elisp/ert/test-source))
(add-to-list 'command-switch-alist (cons "--skip-test" #'elisp/ert/skip-test))
(add-to-list 'command-switch-alist (cons "--skip-tag" #'elisp/ert/skip-tag))

(defvar elisp/ert/test--sources ()
  "Test source files to be loaded.
This list is populated by --test-source command-line options.")

(defvar elisp/ert/testing--in-progress nil
  "Whether tests are currently running.
Used to detect recursive invocation of
‘elisp/ert/run-batch-and-exit’.")

(defun elisp/ert/run-batch-and-exit ()
  "Run ERT tests in batch mode.
This is similar to ‘ert-run-tests-batch-and-exit’, but uses the
TESTBRIDGE_TEST_ONLY environmental variable as test selector."
  (unless noninteractive
    (error "This function works only in batch mode"))
  (when elisp/ert/testing--in-progress
    (error "Recursive invocation of ‘elisp/ert/run-batch-and-exit’"))
  (let* ((elisp/ert/testing--in-progress t)
         (attempt-stack-overflow-recovery nil)
         (attempt-orderly-shutdown-on-fatal-signal nil)
         (edebug-initial-mode 'Go-nonstop)  ; ‘step’ doesn’t work in batch mode
         ;; We perform our own coverage instrumentation.
         (edebug-behavior-alist (cons '(elisp/ert/coverage
                                        elisp/ert/edebug--enter
                                        elisp/ert/edebug--before
                                        elisp/ert/edebug--after)
                                      (bound-and-true-p edebug-behavior-alist)))
         (warning-fill-column 1000)  ; https://debbugs.gnu.org/52281
         (source-dir (getenv "TEST_SRCDIR"))
         (temp-dir (getenv "TEST_TMPDIR"))
         (temporary-file-directory
          (file-name-as-directory (concat "/:" temp-dir)))
         ;; We could get the workspace name from the TEST_WORKSPACE environment
         ;; variable, but that one’s optional
         ;; (cf. https://bazel.build/reference/test-encyclopedia#initial-conditions).
         (workspace-name (file-name-nondirectory
                          (directory-file-name default-directory)))
         (runfiles-handler-installed
          (rassq #'elisp/runfiles/file-handler file-name-handler-alist))
         ;; If the runfiles filename handler is installed, use that.  It’s more
         ;; correct and should also work on Windows.
         (resource-root (file-name-as-directory
                         (if runfiles-handler-installed
                             (concat "/bazel-runfile:" workspace-name)
                           default-directory)))
         ;; Best-effort support for ‘ert-resource-directory’ and
         ;; ‘ert-resource-file’.  The directory returned by
         ;; ‘ert-resource-directory’ will typically be in the execution root and
         ;; no longer be valid when the test runs.  Therefore, strip out
         ;; everything up to the workspace directory in the execution root
         ;; (cf. https://bazel.build/remote/output-directories#layout-diagram),
         ;; and replace it with the default directory.  Robust tests should use
         ;; the ‘elisp/runfiles/runfiles’ library to find their data files.
         (ert-resource-directory-trim-left-regexp
          (rx-to-string `(seq (* nonl) ?/ ,workspace-name ?/) :no-group))
         (ert-resource-directory-format
          (concat (replace-regexp-in-string (rx ?%) "%%" resource-root
                                            :fixedcase :literal)
                  "%s-resources/"))
         (report-file (getenv "XML_OUTPUT_FILE"))
         (fail-fast (equal (getenv "TESTBRIDGE_TEST_RUNNER_FAIL_FAST") "1"))
         (random-seed (or (getenv "TEST_RANDOM_SEED") ""))
         (shard-count (string-to-number (or (getenv "TEST_TOTAL_SHARDS") "1")))
         (shard-index (string-to-number (or (getenv "TEST_SHARD_INDEX") "0")))
         (shard-status-file (getenv "TEST_SHARD_STATUS_FILE"))
         (coverage-enabled (equal (getenv "COVERAGE") "1"))
         (coverage-manifest (getenv "COVERAGE_MANIFEST"))
         (coverage-dir (getenv "COVERAGE_DIR"))
         (verbose-coverage (not (member (getenv "VERBOSE_COVERAGE") '(nil ""))))
         (selector (elisp/ert/make--selector
                    (and coverage-enabled '(:nocover))))
         (original-load-suffixes load-suffixes)
         ;; If coverage is enabled, check for a file with a well-known
         ;; extension first.  The Bazel runfiles machinery is expected to
         ;; generate these files for source files that should be instrumented.
         ;; See the commentary in //elisp:defs.bzl for details.
         (load-suffixes (if coverage-enabled
                            (cons ".el.instrument" load-suffixes)
                          load-suffixes))
         (load-buffers ()))
    ;; TEST_SRCDIR and TEST_TMPDIR are required,
    ;; cf. https://bazel.build/reference/test-encyclopedia#initial-conditions.
    (and (member source-dir '(nil "")) (error "TEST_SRCDIR not set"))
    (and (member temp-dir '(nil "")) (error "TEST_TMPDIR not set"))
    (and coverage-enabled (member coverage-manifest '(nil ""))
         (error "Coverage requested but COVERAGE_MANIFEST not set"))
    (and coverage-enabled (member coverage-dir '(nil ""))
         (error "Coverage requested but COVERAGE_DIR not set"))
    (unless (and (natnump shard-count) (natnump shard-index)
                 (< shard-index shard-count))
      (error "Invalid SHARD_COUNT (%s) or SHARD_INDEX (%s)"
             shard-count shard-index))
    (when coverage-enabled
      (when verbose-coverage
        (message "Reading coverage manifest %s" coverage-manifest))
      (let ((format-alist nil)
            (after-insert-file-functions nil)
            ;; The coverage manifest uses ISO-8859-1, see
            ;; https://github.com/bazelbuild/bazel/blob/3.1.0/src/main/java/com/google/devtools/build/lib/analysis/test/InstrumentedFileManifestAction.java#L68.
            (coding-system-for-read 'iso-8859-1-unix)
            (instrumented-files ()))
        (with-temp-buffer
          (insert-file-contents (concat "/:" coverage-manifest))
          (while (not (eobp))
            ;; The filenames in the coverage manifest are typically relative to
            ;; the current directory, so expand them here.
            (push (expand-file-name
                   (buffer-substring-no-properties (point) (line-end-position)))
                  instrumented-files)
            (forward-line)))
        (when verbose-coverage
          (message "Found %d files in coverage manifest"
                   (length instrumented-files)))
        ;; We don’t bother removing the advises since we are going to kill
        ;; Emacs anyway.
        (add-function
         :before-until load-source-file-function
         (lambda (fullname file _noerror _nomessage)
           ;; If we got a magic filename that tells us to instrument a file,
           ;; then instrument the corresponding source file if that exists.
           ;; See the commentary in //elisp:defs.bzl for details.  In all other
           ;; cases, we defer to the normal ‘load-source-file-function’, which
           ;; is also responsible for raising errors if desired.
           (when (string-suffix-p ".el.instrument" fullname)
             (cl-callf2 string-remove-suffix ".instrument" fullname)
             (cl-callf2 string-remove-suffix ".instrument" file)
             (when (and (file-readable-p fullname)
                        ;; We still need to check whether Bazel wants us to
                        ;; instrument the file.
                        (cl-find fullname instrumented-files
                                 :test #'elisp/ert/file--equal-p))
               (push (elisp/ert/load--instrument fullname file) load-buffers)
               t))))
        ;; Work around another Edebug specification issue fixed with Emacs
        ;; commit aeb25f9d3d12a18ef3881e23b32a34615355d4d0.
        (when (< emacs-major-version 29)
          (put #'cl-define-compiler-macro 'edebug-form-spec
               `(&define name
                         ([&optional "&whole" arg]
                          ,@(car
                             (or (get 'cl-macro-list 'edebug-elem-spec)
                                 (edebug-get-spec 'cl-macro-list))))
                         cl-declarations-or-string def-body)))
        ;; Work around another Edebug specification issue fixed with Emacs
        ;; commit c799ad42f705f64975771e181dee29e1d0ebe97a.
        (when (eql emacs-major-version 29)
          (put #'cl-define-compiler-macro 'edebug-form-spec
               `(&define [&name symbolp "@cl-compiler-macro"]
                         cl-macro-list
                         cl-declarations-or-string def-body)))))
    (random random-seed)
    (when shard-status-file
      (write-region "" nil (concat "/:" shard-status-file) :append))
    (mapc #'load (reverse elisp/ert/test--sources))
    (let ((tests (ert-select-tests selector t))
          (total 0)
          (unexpected 0)
          (errors 0)
          (failures 0)
          (skipped 0)
          (test-reports ())
          (start-time (current-time)))
      (or tests (error "Selector %S doesn’t match any tests" selector))
      (when (> shard-count 1)
        (setq tests (cl-loop for test in tests
                             for i from 0
                             when (eql (mod i shard-count) shard-index)
                             collect test))
        (or tests (message "Empty shard with index %d" shard-index)))
      (message "Running %d tests" (length tests))
      (while tests
        (message "Running test %s" (ert-test-name (car tests)))
        (let* ((test (pop tests))
               (name (ert-test-name test))
               (start-time (current-time))
               (result (ert-run-test test))
               (duration (time-subtract nil start-time))
               (expected (ert-test-result-expected-p test result))
               (failed
                (and (not expected)
                     ;; A test that passed unexpectedly should count as failed
                     ;; for the XML report.
                     (ert-test-result-type-p result '(or :passed :failed))))
               (status (ert-string-for-test-result result expected))
               (tag nil)
               (failure-message nil)
               (type nil)
               (description nil))
          (message "Test %s %s and took %d ms" name status
                   (* (float-time duration) 1000))
          (cl-incf total)
          (unless expected
            (cl-incf unexpected)
            ;; Print a nice error message that should point back to the source
            ;; file in a compilation buffer.  We don’t want to find the
            ;; “.el.instrument” files when printing the error message, so bind
            ;; ‘load-suffixes’ temporarily to its original value.
            (let ((load-suffixes original-load-suffixes))
              (when-let ((prefix (elisp/ert/message--prefix name)))
                (message "%s: Test %s %s" prefix name status)))
            (and fail-fast (setq tests nil)))
          (and failed (cl-incf failures))
          (and (not expected) (not failed) (cl-incf errors))
          (when (ert-test-skipped-p result)
            (cl-incf skipped)
            (setq tag 'skipped))
          (and (not expected) (ert-test-passed-p result)
               ;; Fake an error so that the test is marked as failed in the XML
               ;; report.
               (setq tag 'failure
                     failure-message "Test passed unexpectedly"
                     type 'error))
          (when (ert-test-result-with-condition-p result)
            (let ((message (elisp/ert/failure--message name result))
                  (condition (ert-test-result-with-condition-condition result)))
              (message "%s" message)
              (unless (symbolp (car condition))
                ;; This shouldn’t normally happen, but happens due to a bug in
                ;; ERT for forms such as (should (integerp (ert-fail "Boo"))).
                (push 'ert-test-failed condition))
              (setq failure-message (error-message-string condition)
                    description message)
              (unless expected
                (setq tag (if failed 'failure 'error)
                      type (car condition)))))
          (let ((report (and tag
                             `((,tag
                                ((message . ,failure-message)
                                 ,@(and type `((type . ,(symbol-name type)))))
                                ,@(and description `(,description)))))))
            (push `(testcase ((name . ,(symbol-name name))
                              ;; classname is required, but we don’t have test
                              ;; classes, so fill in a dummy value.
                              (classname . "ERT")
                              (time . ,(format-time-string "%s.%N" duration)))
                             ,@report)
                  test-reports))))
      (message "Running %d tests finished, %d results unexpected"
               total unexpected)
      (unless (member report-file '(nil ""))
        (with-temp-buffer
          ;; The expected format of the XML output file isn’t well-documented.
          ;; https://bazel.build/reference/test-encyclopedia#initial-conditions
          ;; only states that the XML file is “based on the JUnit test result
          ;; schema”, referring to
          ;; https://windyroad.com.au/dl/Open%20Source/JUnit.xsd.
          ;; https://llg.cubic.org/docs/junit/ and
          ;; https://help.catchsoftware.com/display/ET/JUnit+Format contain a
          ;; bit of documentation.
          (xml-print
           (elisp/ert/sanitize--xml
            `((testsuite
               ((name . "ERT")  ; required
                (hostname . "localhost")  ; required
                (tests . ,(number-to-string total))
                (errors . ,(number-to-string errors))
                (failures . ,(number-to-string failures))
                (skipped . ,(number-to-string skipped))
                (time . ,(format-time-string "%s.%N"
                                             (time-subtract nil start-time)))
                ;; No timezone or fractional seconds allowed.
                (timestamp . ,(format-time-string "%FT%T" start-time)))
               (properties () (property ((name . "emacs-version")
                                         (value . ,emacs-version))))
               ,@(nreverse test-reports)
               (system-out) (system-err)))))
          (let ((coding-system-for-write 'utf-8-unix)
                (write-region-annotate-functions nil)
                (write-region-post-annotation-function nil))
            (write-region nil nil (concat "/:" report-file)))))
      (when coverage-enabled
        (when verbose-coverage
          (message "Writing coverage report into directory %s" coverage-dir))
        (elisp/ert/write--coverage-report (concat "/:" coverage-dir)
                                          load-buffers))
      (kill-emacs (min unexpected 1)))))

(defvar elisp/ert/skip--tests nil
  "Test symbols to be skipped.
This list is populated by --skip-test command-line options.")

(defvar elisp/ert/skip--tags nil
  "Test tags to be skipped.
This list is populated by --skip-tag command-line options.")

(defun elisp/ert/test-source (_arg)
  "Handle the --test-source command-line argument."
  (let ((file (elisp/ert/pop--argument 'test-source)))
    (push file elisp/ert/test--sources)))

(defun elisp/ert/skip-test (_arg)
  "Handle the --skip-test command-line argument."
  (let ((test (elisp/ert/pop--argument 'skip-test)))
    (push (intern test) elisp/ert/skip--tests)))

(defun elisp/ert/skip-tag (_arg)
  "Handle the --skip-tag command-line argument."
  (let ((tag (elisp/ert/pop--argument 'skip-tag)))
    (push (intern tag) elisp/ert/skip--tags)))

(defun elisp/ert/pop--argument (option)
  "Pop argument for OPTION from the command line."
  (cl-check-type option symbol)
  (let ((argument (pop command-line-args-left)))
    (or argument (error "Missing value for --%s" option))
    (if (memq system-type '(ms-dos windows-nt cygwin))
        (decode-coding-string (url-unhex-string argument :allow-newlines)
                              'utf-8-unix)
      argument)))

(defun elisp/ert/make--selector (skip-tags)
  "Build an ERT selector from environment and command line.
SKIP-TAGS is a list of additional tags to skip."
  (cl-check-type skip-tags list)
  (cl-callf append skip-tags (reverse elisp/ert/skip--tags))
  ;; We optimize the test selector somewhat.  It’s displayed to the user if no
  ;; test matches, and then we’d like to avoid empty branches such as ‘(and)’.
  (cl-flet ((combine (op def elts)
              (cond ((null elts) def)
                    ((cdr elts) `(,op ,@elts))
                    (t (car elts))))
            (invert (sel)
              (if sel `(not ,sel) t)))
    (let* ((test-filter (getenv "TESTBRIDGE_TEST_ONLY"))
           (filter (if (member test-filter '(nil "")) t (read test-filter)))
           (skip-tags-sel
            (invert
             (combine 'or nil (nreverse (mapcar (lambda (tag) `(tag ,tag))
                                                skip-tags)))))
           (skip-tests
            (invert (combine 'member nil (reverse elisp/ert/skip--tests)))))
      (combine 'and t (delq t (list filter skip-tags-sel skip-tests))))))

(defun elisp/ert/failure--message (name result)
  "Return a failure message for the RESULT of a failing test.
NAME is the name of the test."
  (cl-check-type name symbol)
  (cl-check-type result ert-test-result-with-condition)
  (with-temp-buffer
    (let ((print-escape-newlines t)
          (pp-escape-newlines t)
          (print-circle t)
          (print-gensym t)
          (print-level 8)
          (print-length 50)
          (backtrace (ert-test-result-with-condition-backtrace result))
          (infos (ert-test-result-with-condition-infos result)))
      (insert (backtrace-to-string backtrace))
      (goto-char (point-min))
      (while (not (eobp))
        (delete-region (min (line-end-position) (+ 120 (point)))
                       (line-end-position))
        ;; ‘backtrace-to-string’ and ‘debugger-insert-backtrace’ already indent
        ;; all lines by two spaces, so we only add two more spaces.
        (insert "  ")
        (forward-line))
      (goto-char (point-min))
      (insert (format-message "\n  Test %s backtrace:\n\n" name))
      (goto-char (point-max))
      (when infos (insert ?\n))
      (dolist (info infos)
        (insert "  " (car info) (cdr info) ?\n))
      (insert (format-message "\n  Test %s condition:\n\n" name))
      (let ((point (point)))
        (pp (ert-test-result-with-condition-condition result)
            (current-buffer))
        (indent-rigidly point (point) 4))
      (insert ?\n)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun elisp/ert/load--instrument (fullname file)
  "Load and instrument the Emacs Lisp file FULLNAME.
FILE is an abbreviated name as described in
‘load-source-file-function’, which see.  Return a live buffer
visiting the file."
  (cl-check-type fullname string)
  (cl-check-type file string)
  ;; Similar to testcover.el, we use Edebug to collect coverage
  ;; information.  The rest of this function is similar to
  ;; ‘load-with-code-conversion’, but we ignore some edge cases.
  (let ((buffer (generate-new-buffer (format "*%s*" file)))
        (reporter (make-progress-reporter
                   (format-message "Loading and instrumenting %s..." file)))
        (load-in-progress t)
        (load-file-name fullname)
        (set-auto-coding-for-load t)
        (inhibit-file-name-operation nil)
        (edebug-all-defs t)
        (edebug-new-definition-function #'elisp/ert/new--definition)
        (edebug-after-instrumentation-function
         #'elisp/ert/after--instrumentation))
    (with-current-buffer buffer
      (insert-file-contents fullname :visit)
      ;; The file buffer needs to be current for Edebug
      ;; instrumentation to work.
      (eval-buffer buffer nil fullname nil :do-allow-print)
      ;; Yuck!  We have to mess with internal Edebug data here.
      ;; Byte-compile all functions to be a bit more realistic.
      (dolist (data edebug-form-data)
        (let ((name (edebug--form-data-name data)))
          ;; Don’t attempt to byte-compile macros due to
          ;; https://debbugs.gnu.org/41618.
          (unless (macrop name) (byte-compile name)))))
    (do-after-load-evaluation fullname)
    (progress-reporter-done reporter)
    buffer))

(defun elisp/ert/new--definition (name)
  "Enable line coverage collection for NAME.
This can be used as ‘edebug-new-definition-function’."
  (cl-check-type name symbol)
  ;; Check for duplicate names, if possible.  Duplicates cause subtle errors
  ;; that are otherwise very hard to debug, cf. https://debbugs.gnu.org/41853.
  (when (get name 'edebug-behavior)
    (error "Symbol ‘%s’ instrumented twice" name))
  (put name 'edebug-behavior 'elisp/ert/coverage)
  (let ((offsets (caddr (get name 'edebug))))
    ;; Unlike Edebug, we initialize the coverage data to nil and only set the
    ;; “before” entry to a non-nil value so that we can easily distinguish
    ;; between “before” and “after” positions later.
    (put name 'elisp/ert/coverage (make-vector (length offsets) nil))))

(defun elisp/ert/after--instrumentation (form)
  "Instrument FORM to collect line and branch coverage information.
This can be used as ‘edebug-after-instrumentation-function’.
Return FORM."
  (let ((seen (make-hash-table :test #'eq)))
    (elisp/ert/instrument--form seen nil form))
  form)

(cl-defstruct (elisp/ert/coverage--data
               (:constructor elisp/ert/make--coverage-data)
               (:copier nil))
  "Coverage data for a specific form.
The ‘edebug-after-instrumentation-function’ initializes the
‘elisp/ert/coverage’ property of each instrumented symbol to a
vector.  Some elements of the vector will be objects of this
type."
  (hits
   0
   :type natnum
   :documentation "Number of times this form was hit.")
  (branches
   nil
   :type (or null vector)
   :documentation "Nil if the underlying form doesn’t have
multiple branches.  Otherwise, a vector of per-branch hit counts.
If a branch can’t be instrumented, the corresponding element is
nil.")
  (parent-branches
   nil
   :type (or null vector)
   :documentation "Nil if the underlying form is neither a
branching condition nor a child form of a branching form.
Otherwise, a reference to the ‘branches’ property of the coverage
data of the corresponding branching form.")
  (branch-index
   nil
   :type (or null natnum)
   :documentation "Index into the ‘parent-branches’ vector
specifying the element that should be incremented whenever the
underlying form is hit.  Nil if no branch element should be
incremented.")
  (then-index
   nil
   :type (or null natnum)
   :documentation "Index into the ‘parent-branches’ vector
specifying the element that should be incremented whenever the
underlying form finishes successfully with a non-nil value.  Nil
if no branch element should be incremented.")
  (else-index
   nil
   :type (or null natnum)
   :documentation "Index into the ‘parent-branches’ vector
specifying the element that should be incremented whenever the
underlying form finishes successfully with a non-nil value.  Nil
if no branch element should be incremented."))

(defun elisp/ert/instrument--form (seen vector form)
  "Instrument FORM to collect line coverage information.
SEEN is a hashtable used to prevent infinite recursion.
VECTOR is either nil (for a toplevel definition) or a vector of
optional ‘elisp/ert/coverage--data’ objects with the same length
as the offset vector.  The vector is attached to the
‘elisp/ert/coverage’ property of the symbol being defined."
  (cl-check-type seen hash-table)
  (cl-check-type vector (or null vector))
  (unless (gethash form seen)
    (puthash form t seen)
    (pcase form
      (`(edebug-enter ',func ,_args ,body)
       (let ((vector (get func 'elisp/ert/coverage)))
         (cl-check-type vector vector)  ; set by ‘elisp/ert/new--definition’
         (elisp/ert/instrument--form seen vector body)))
      ((or `(edebug-after (edebug-before ,index) ,_after-index ,form)
           `(edebug-after ,_ ,index ,form))
       (cl-check-type vector vector)  ; set by ‘elisp/ert/new--definition’
       (cl-check-type index natnum)
       (cl-check-type (aref vector index) null)  ; not yet prepared
       ;; We prefer setting the “before” entry to a non-nil value so that we can
       ;; easily distinguish between “before” and “after” positions later.  We
       ;; have to do this in ‘edebug-after-instrumentation-function’ because
       ;; otherwise we couldn’t distinguish between forms that aren’t
       ;; instrumented and forms that are instrumented but not executed.  The
       ;; second branch of the ‘or’ above is chosen for forms without a “before”
       ;; entry, in which case we have to fall back to the “after” entry.
       (let ((data (aset vector index (elisp/ert/make--coverage-data))))
         (elisp/ert/instrument--form seen vector form)
         ;; Determine whether this is a branching form.  If so, generate a
         ;; branch frequency vector and attach it to DATA.
         (when-let ((branches (elisp/ert/instrument--branches vector form)))
           (setf (elisp/ert/coverage--data-branches data) branches))))
      ((pred proper-list-p)
       ;; Use ‘dolist’ where possible to avoid deep recursion.
       (dolist (element form)
         (elisp/ert/instrument--form seen vector element)))
      (`(,car . ,cdr)
       (elisp/ert/instrument--form seen vector car)
       (elisp/ert/instrument--form seen vector cdr))
      ((pred vectorp)
       (cl-loop for element across form
                do (elisp/ert/instrument--form seen vector element)))
      ;; Literals that can’t be instrumented.
      ((or (pred symbolp) (pred numberp) (pred stringp)))
      ;; Everything else is unexpected.
      (_ (signal 'elisp/ert/syntax-error (list form))))))

(define-error 'elisp/ert/syntax-error "Syntax error" 'invalid-read-syntax)

(defun elisp/ert/instrument--branches (vector form)
  "Instrument a branching FORM.
VECTOR is the coverage data vector attached to the
‘elisp/ert/coverage’ property of the symbol being defined.
Return nil if FORM doesn’t define a branching construct.
Otherwise, return a new vector containing per-branch
frequencies (hit counts).  If a branch can’t be instrumented, the
corresponding element in the return value will be nil."
  (cl-check-type vector vector)
  (pcase form
    (`(,(or 'if 'when 'unless 'while) ,cond . ,_)
     (let ((branches (vector nil nil)))
       (elisp/ert/instrument--branch vector branches (list cond) nil 0 1)
       branches))
    (`(,(or 'and 'or) . ,(and (pred proper-list-p) conditions))
     ;; The last condition form won’t introduce a new branch.
     (cl-callf butlast conditions)
     ;; Assume that each remaining condition introduces two branches.  This
     ;; isn’t quite correct, but should be good enough.
     (let ((branches (make-vector (* 2 (length conditions)) nil)))
       (cl-loop for form in conditions
                and index from 0 by 2
                do (elisp/ert/instrument--branch vector branches (list form)
                                                 nil index (1+ index)))
       branches))
    (`(,(or 'cond
            'cl-case 'cl-ecase
            'cl-typecase 'cl-etypecase
            'pcase 'pcase-exhaustive)
       . ,(and (pred proper-list-p) clauses))
     (let ((branches (make-vector (length clauses) nil)))
       (cl-loop for clause in clauses
                and index from 0
                when (listp clause)  ; gently skip over syntax errors
                ;; Instrumenting the entire CLAUSE works nicely for the special
                ;; case of a one-element CLAUSE for ‘cond’.  Since Edebug won’t
                ;; add wrappers for non-form elements like the first elements of
                ;; ‘cl-case’ clauses etc., we can just instrument the entire
                ;; CLAUSE in all cases.  Note that this will miss CLAUSES with
                ;; an empty body, but that’s life.
                do (elisp/ert/instrument--branch vector branches clause
                                                 index nil nil))
       branches))
    (`(,(or 'condition-case 'condition-case-unless-debug)
       ,_var ,bodyform . ,(and (pred proper-list-p) clauses))
     ;; Similar to the ‘cond’ case, but here we have one branch for each handler
     ;; as well as one branch for a successful exit.
     (let ((branches (make-vector (1+ (length clauses)) nil)))
       ;; Specifying a THEN-INDEX and ELSE-INDEX instead of a BRANCH-INDEX
       ;; forces evaluation after the form finishes, which is exactly what we
       ;; want here.
       (elisp/ert/instrument--branch vector branches (list bodyform) nil 0 0)
       (cl-loop for clause in clauses
                and index from 1
                when (listp clause)  ; gently skip over syntax errors
                do (elisp/ert/instrument--branch vector branches clause
                                                 index nil nil))
       branches))
    (`(,(or 'if-let 'when-let) (,(pred symbolp) ,expr) . ,_)
     ;; This flavor doesn’t actually work right due to
     ;; https://debbugs.gnu.org/48489.
     (let ((branches (vector nil nil)))
       (elisp/ert/instrument--branch vector branches (list expr) nil 0 1)
       branches))
    (`(,(or 'if-let 'if-let* 'when-let 'when-let* 'and-let*)
       ,(and (pred proper-list-p) spec) . ,_)
     (let ((branches (make-vector (* 2 (length spec)) nil)))
       (cl-loop for binding in spec
                and index from 0 by 2
                do (pcase binding
                     ((or `(,(pred symbolp) ,form)
                          `(,form)
                          ;; We accept a plain symbol, but this isn’t actually
                          ;; instrumented because Edebug doesn’t instrument
                          ;; symbols matched using ‘symbolp’.
                          (and (pred symbolp) form))
                      (elisp/ert/instrument--branch
                       vector branches (list form) nil index (1+ index)))))
       branches))
    (`(cl-loop . ,(and (pred proper-list-p) rest))
     (when-let ((conditions (cl-loop for (keyword form) on rest
                                     when (memq keyword '(if when unless))
                                     collect form)))
       (let ((branches (make-vector (* 2 (length conditions)) nil)))
         (cl-loop for form in conditions
                  and index from 0 by 2
                  do (elisp/ert/instrument--branch
                      vector branches (list form) nil index (1+ index)))
         branches)))))

(defun elisp/ert/instrument--branch
    (vector branches forms branch-index then-index else-index)
  "Instrument a single branch of a branching form.
VECTOR is the coverage data vector attached to the
‘elisp/ert/coverage’ property of the symbol being defined.
BRANCHES is the branch frequency vector for the parent form.
FORMS is the list of forms to be instrumented.  BRANCH-INDEX,
THEN-INDEX, and ELSE-INDEX will be used for the ‘branch-index’,
‘then-index’, and ‘else-index’ properties of the
‘elisp/ert/coverage--data’ object of the newly-instrumented form,
respectively."
  (cl-check-type vector vector)
  (cl-check-type branches vector)
  (cl-check-type branch-index (or natnum null))
  (cl-check-type then-index (or natnum null))
  (cl-check-type else-index (or natnum null))
  (cl-check-type forms list)
  (cl-assert (eq (null branch-index) (not (null then-index))))
  (cl-assert (eq (null then-index) (null else-index)))
  (cl-assert (or (null then-index) (eql (length forms) 1)))
  (cl-dolist (form forms)
    ;; Look for the first form that has any Edebug instrumentation.
    (pcase form
      ;; Prefer the “before” spot, like ‘elisp/ert/instrument--form’.
      ((or `(edebug-after (edebug-before ,form-index) ,_after-index ,_form)
           `(edebug-after ,_ ,form-index ,_form))
       ;; The data object has been prepared by ‘elisp/ert/instrument--form’.
       (let ((data (aref vector form-index)))
         (cl-check-type data elisp/ert/coverage--data)
         (setf (elisp/ert/coverage--data-parent-branches data) branches
               (elisp/ert/coverage--data-branch-index data) branch-index
               (elisp/ert/coverage--data-then-index data) then-index
               (elisp/ert/coverage--data-else-index data) else-index))
       ;; Initialize branch frequency vector.  Some elements might remain nil,
       ;; if Edebug hasn’t generated any instrumentation for the form.
       (dolist (i (list branch-index then-index else-index))
         (when i (aset branches i 0)))
       (cl-return)))))

;; Innermost function being executed, dynamically bound by
;; ‘elisp/ert/edebug--enter’.
(defvar elisp/ert/coverage--function)

;; Current coverage data vector, dynamically bound by ‘elisp/ert/edebug--enter’.
(defvar elisp/ert/coverage--vector)

(defun elisp/ert/edebug--enter (func args body)
  "Implementation of ‘edebug-enter’ for ERT coverage instrumentation.
See ‘edebug-enter’ for the meaning of FUNC, ARGS, and BODY."
  (cl-check-type func symbol)
  (cl-check-type args list)
  (cl-check-type body function)
  (let ((elisp/ert/coverage--function func)
        (elisp/ert/coverage--vector (get func 'elisp/ert/coverage)))
    (funcall body)))

(defun elisp/ert/edebug--before (before-index)
  "Implementation of ‘edebug-before’ for ERT coverage instrumentation.
BEFORE-INDEX is the index into ‘elisp/ert/frequency--vector’ for
the beginning of the form.  Return (before . BEFORE-INDEX)."
  (cl-check-type before-index natnum)
  (let ((data (elisp/ert/coverage--data before-index)))
    ;; Increment hit count.  We prefer doing that here because the beginning of
    ;; a form tends to be more interesting and the end, and we’d like to
    ;; increment the hit count for the first line of a form instead of the last.
    ;; See ‘elisp/ert/edebug--after’ for a case where this isn’t possible.
    (cl-incf (elisp/ert/coverage--data-hits data))
    ;; If this is a subform of a branching form and we’re supposed to
    ;; unconditionally increment a branch frequency, do so now.
    (when-let ((branches (elisp/ert/coverage--data-parent-branches data))
               (branch-index (elisp/ert/coverage--data-branch-index data)))
      (cl-incf (aref branches branch-index))))
  ;; The return value gets passed to the BEFORE-INDEX argument of
  ;; ‘edebug-after’.  Pick a form that allows it to distinguish this case from
  ;; the case of a plain variable, which doesn’t involve ‘edebug-before’.
  `(before . ,before-index))

(defun elisp/ert/edebug--after (before after-index value)
  "Implementation of ‘edebug-before’ for ERT coverage instrumentation.
BEFORE is normally of the form (before . BEFORE-INDEX).
BEFORE-INDEX and AFTER-INDEX are the indices into
‘elisp/ert/frequency--vector’ for the beginning and end of the
form, respectively.  VALUE is the value of the form.  Return
VALUE."
  (cl-check-type after-index natnum)
  ;; Edebug uses two different forms for instrumentation: For list forms it
  ;; emits (edebug-after (edebug-before BEFORE-INDEX) AFTER-INDEX form), but for
  ;; variables it just emits (edebug-after 0 AFTER-INDEX form).  We prefer
  ;; incrementing the hit count for the beginning of the form, see
  ;; ‘elisp/ert/edebug--before’.  However, where this isn’t possible, increment
  ;; the hit count for the end of them form.  This should only happen for
  ;; variables that rarely span more than one line.
  (let (incrementp form-index)
    (pcase before
      ;; Increment line hit count only if ‘edebug-before’ hasn’t incremented it
      ;; yet.
      (`(before . ,before-index) (setq incrementp nil form-index before-index))
      (_ (setq incrementp t form-index after-index)))
    (let ((data (elisp/ert/coverage--data form-index)))
      (when incrementp
        ;; Increment line hit count, because that hasn’t happened yet in
        ;; ‘edebug-before’.
        (cl-incf (elisp/ert/coverage--data-hits data)))
      ;; Check if this form is a condition of a branching form such as ‘if’.  If
      ;; so, increment the branch hit count for the “then” or “else” branch
      ;; depending on VALUE.  We need to do this in ‘edebug-after’ because only
      ;; then we know the return value of the form.
      (when-let ((branches (elisp/ert/coverage--data-parent-branches data))
                 (branch-index (if value
                                   (elisp/ert/coverage--data-then-index data)
                                 (elisp/ert/coverage--data-else-index data))))
        (cl-incf (aref branches branch-index)))))
  value)

(defun elisp/ert/coverage--data (index)
  "Return INDEX’th element of the current coverage vector.
The return value is of type ‘elisp/ert/coverage--data’."
  (declare (side-effect-free t))
  (cl-check-type index natnum)
  (let ((data (aref elisp/ert/coverage--vector index)))
    (unless data
      ;; This is typically an error in the Edebug specification of a macro being
      ;; expanded, not a bug in this library.  Give the user a more helpful
      ;; error message than “wrong type”.  To check which macro is the culprit,
      ;; look through the body of the ‘edebug/coverage--function’.  The most
      ;; common error is to use ‘form’ where ‘def-form’ would be required; see
      ;; Info node ‘(elisp) Specification List’.
      (signal 'elisp/ert/missing-coverage-data
              (list elisp/ert/coverage--function
                    elisp/ert/coverage--vector index)))
    (cl-the elisp/ert/coverage--data data)))

(define-error 'elisp/ert/missing-coverage-data "Missing coverage data")

(defun elisp/ert/write--coverage-report (coverage-dir buffers)
  "Write a coverage report to a file in COVERAGE-DIR.
BUFFERS is a list of buffers containing Emacs Lisp sources
instrumented using Edebug."
  (cl-check-type coverage-dir string)
  (cl-check-type buffers list)
  (with-temp-buffer
    (let ((coding-system-for-write 'utf-8-unix)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil)
          (test-name (getenv "TEST_TARGET")))
      (unless (member test-name '(nil ""))
        (insert "TN:" (elisp/ert/sanitize--string test-name) ?\n))
      (dolist (buffer buffers)
        (elisp/ert/insert--coverage-report buffer)
        (kill-buffer buffer))
      (write-region nil nil (expand-file-name "emacs-lisp.dat" coverage-dir)
                    :append))))

(eval-when-compile
  (defmacro elisp/ert/hash--get-or-put (key table &rest body)
    "Return the value associated with KEY in TABLE.
If no such value exists, evaluate BODY and put its value into
TABLE."
    (declare (indent 2) (debug t))
    (macroexp-let2* nil ((key key) (table table))
      (let ((value (make-symbol "value"))
            (default (cons nil nil)))  ; unique object
        `(let ((,value (gethash ,key ,table ',default)))
           (if (eq ,value ',default)
               (puthash ,key ,(macroexp-progn body) ,table)
             ,value))))))

(defun elisp/ert/insert--coverage-report (buffer)
  "Insert a coverage report into the current buffer.
BUFFER must be a different buffer visiting an Emacs Lisp source
file that has been instrumented with Edebug."
  (cl-check-type buffer buffer)
  (let ((file-name (elisp/ert/sanitize--string
                    (elisp/ert/file--display-name (buffer-file-name buffer))))
        (functions ())
        (functions-hit 0)
        (lines (make-hash-table :test #'eql))
        ;; BRANCHES maps line numbers to hashtables mapping offsets to branch
        ;; hit count vectors.
        (branches (make-hash-table :test #'eql)))
    (with-current-buffer buffer
      (widen)
      (cl-loop
       for data in edebug-form-data
       ;; Yuck!  More messing around with Edebug internals.
       for name = (edebug--form-data-name data)
       for ours = (eq (get name 'edebug-behavior) 'elisp/ert/coverage)
       for coverage = (get name
                           (if ours 'elisp/ert/coverage 'edebug-freq-count))
       for frequency = (if ours
                           (lambda (cov)
                             (and cov (elisp/ert/coverage--data-hits cov)))
                         #'identity)
       ;; We don’t really know the number of function calls, so assume it’s the
       ;; same as the hit count of the first breakpoint.
       for calls = (cl-loop for cov across coverage
                            for hits = (funcall frequency cov)
                            thereis (and (not (eql hits 0)) hits)
                            finally return 0)
       for (begin _ offsets) = (get name 'edebug)
       do
       (unless (eq (marker-buffer begin) buffer)
         (error "Function %s got redefined in some other file" name))
       (cl-incf functions-hit (min calls 1))
       (cl-assert (eql (length coverage) (length offsets)) :show-args)
       (cl-loop
        for offset across offsets
        and cov across coverage
        for freq = (funcall frequency cov)
        for position = (+ begin offset)
        ;; Edebug adds two elements per form to the frequency and offset tables,
        ;; one for the beginning of the form and one for the end.  The end
        ;; position will typically contain a closing parenthesis or space.  We
        ;; don’t consider this a covered line since it typically only contains
        ;; unimportant pieces of the form.  An exception is a plain variable;
        ;; see the discussion in ‘elisp/ert/edebug--after’.
        for ok = (if ours
                     ;; If we added our own coverage instrumentation, the
                     ;; coverage data is set only for form beginnings and
                     ;; variables.
                     cov
                   ;; Otherwise, check whether we are probably at a form
                   ;; beginning or after a variable.
                   (or (not (memql (char-syntax (char-after position))
                                   '(?\) ?\s)))
                       (memql (char-syntax (char-before position))
                              '(?w ?_))))
        do
        (when ok
          (let ((line (line-number-at-pos position)))
            (cl-callf max (gethash line lines 0) freq)
            (when ours
              ;; Collect branch coverage information if the form has multiple
              ;; branches.
              (when-let ((frequencies
                          (elisp/ert/coverage--data-branches cov)))
                ;; Remove branches that Edebug didn’t instrument.
                (cl-callf2 cl-remove nil frequencies)
                ;; If fewer than two branches are left, we don’t really have any
                ;; meaningful branch coverage data.
                (when (> (length frequencies) 1)
                  (let* ((u (elisp/ert/hash--get-or-put line branches
                              (make-hash-table :test #'eql)))
                         (v (elisp/ert/hash--get-or-put offset u
                              (make-vector (length frequencies) 0))))
                    (cl-loop for f across frequencies
                             and n across-ref v
                             do (cl-callf max n f)))))))))
       (push (list (line-number-at-pos begin)
                   (elisp/ert/sanitize--string (symbol-name name))
                   calls)
             functions)))
    (cl-callf sort functions #'car-less-than-car)
    ;; The expected format is described to some extend in the
    ;; geninfo(1) man page.
    (insert (format "SF:%s\n" file-name))
    (dolist (func functions)
      (insert (format "FN:%d,%s\n" (car func) (cadr func))))
    (dolist (func functions)
      (insert (format "FNDA:%d,%s\n" (caddr func) (cadr func))))
    (insert (format "FNF:%d\n" (length functions)))
    (insert (format "FNH:%d\n" functions-hit))
    ;; Convert branch table into a vector used for BRDA lines.
    (let ((vector (make-vector (hash-table-count branches) nil))
          (branches-hit 0)
          (branches-found 0))
      (cl-loop
       for line hash-keys of branches using (hash-values branches)
       and elem across-ref vector
       do
       (cl-check-type line natnum)
       (cl-check-type branches hash-table)
       ;; Generate one block per branching form for this line.
       (let ((blocks (make-vector (hash-table-count branches) nil)))
         (cl-loop
          for offset hash-keys of branches using (hash-values branches)
          and block across-ref blocks
          for hits = (cl-count 0 branches :test-not #'eql)
          do
          (cl-check-type offset natnum)
          (cl-check-type branches vector)
          (when (eql hits 0) (fillarray branches '-))  ; block not executed
          (setf block (cons offset branches))
          (cl-incf branches-found (length branches))
          (cl-incf branches-hit hits))
         (setf elem (cons line blocks))))
      (cl-loop
       for (line . blocks) across (sort vector #'car-less-than-car)
       do (cl-loop
           ;; Sort block list for stability by offset.
           for (_offset . frequencies) across (sort blocks #'car-less-than-car)
           and block-index from 0
           do (cl-loop
               for frequency across frequencies
               and branch-index from 0
               do (insert (format "BRDA:%d,%d,%d,%s\n"
                                  line block-index branch-index frequency)))))
      ;; Only print branch summary if there were any branches at all.
      (unless (eql branches-found 0)
        (insert (format "BRF:%d\nBRH:%d\n" branches-found branches-hit))))
    ;; Convert line frequency table into a vector used for DA lines.
    (let ((vector (make-vector (hash-table-count lines) nil))
          (lines-hit 0))
      (cl-loop for line hash-keys of lines using (hash-values freq)
               and elem across-ref vector
               do (setf elem (cons line freq)))
      (cl-loop for (line . freq) across (sort vector #'car-less-than-car)
               do
               (cl-incf lines-hit (min freq 1))
               (insert (format "DA:%d,%d\n" line freq)))
      (insert (format "LH:%d\nLF:%d\nend_of_record\n"
                      lines-hit (length vector))))))

(defun elisp/ert/message--prefix (test)
  "Return a message prefix for TEST.
TEST should be an ERT test symbol.  If possible, return a prefix
of the form “FILE:LINE” as described in the GNU Coding Standards;
see Info node ‘(standards) Errors’.  If the file and line can’t
be determined, return nil."
  (cl-check-type test symbol)
  (let ((case-fold-search nil)
        (directory default-directory))
    ;; Yuck!  ‘ert--test’ is an implementation detail.
    (when-let ((file (symbol-file test 'ert--test)))
      ;; The filename typically refers to a compiled file in the execution root.
      ;; Try to resolve it to a source file.  See
      ;; https://bazel.build/remote/output-directories#layout-diagram.
      (when (string-match (rx "/execroot/"
                              (+ (not (any ?/))) ?/ ; workspace
                              (+ (not (any ?/))) ?/ ; bazel-out
                              (+ (not (any ?/))) ?/ ; configuration
                              "bin/"
                              (group (+ nonl)) ".elc" eos)
                          file)
        ;; We can use the filename relative to the Bazel binary directory since
        ;; that corresponds to a source filename relative to some workspace
        ;; root.  ‘find-library-name’ will find the corresponding source file
        ;; because all workspace roots are in the ‘load-path’.
        (cl-callf2 match-string-no-properties 1 file))
      ;; ‘find-library-name’ signals errors if it can’t find the library.  Since
      ;; we’re only attempting to print a log message here, ignore them and move
      ;; on.
      (when-let ((file (ignore-errors (find-library-name file))))
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (format-alist nil)
                (after-insert-file-functions nil))
            (insert-file-contents file))
          (emacs-lisp-mode)
          (when (re-search-forward
                 (rx-to-string `(seq bol (* space) "(ert-deftest" (+ space)
                                     ,(symbol-name test) (or space eol)))
                 nil t)
            ;; Try to print nice and short filenames.  We do this by using the
            ;; filename relative to the test working directory
            ;; (i.e. $TEST_SRCDIR/$TEST_WORKSPACE).
            (format "%s:%d"
                    (elisp/ert/file--display-name file directory)
                    (line-number-at-pos))))))))

(defun elisp/ert/sanitize--string (string)
  "Return a sanitized version of STRING for the coverage file."
  (cl-check-type string string)
  ;; The coverage file is line-based, so the string shouldn’t contain any
  ;; newlines.
  (let ((case-fold-search nil))
    (replace-regexp-in-string (rx (not (any alnum blank punct))) "?" string)))

(defun elisp/ert/file--display-name (filename &optional directory)
  "Return a relative or absolute name for FILENAME, whichever is shorter.
DIRECTORY is the directory that could contain FILENAME."
  (cl-check-type filename string)
  (cl-check-type directory (or null string))
  (unless directory (setq directory default-directory))
  ;; If we got a virtual runfile filename but a physical directory, make an
  ;; educated guess about the location relative to the test working directory.
  (and (string-prefix-p "/bazel-runfile:" filename)
       (not (string-prefix-p "/bazel-runfile:" directory))
       (let ((workspace (file-name-nondirectory
                         (directory-file-name directory))))
         (setq directory (concat "/bazel-runfile:" workspace
                                 (unless (string-empty-p workspace) "/")))))
  ;; Work around https://debbugs.gnu.org/46219.
  (unless (file-remote-p filename)
    (setq filename (file-name-quote (expand-file-name filename))))
  (unless (file-remote-p directory)
    (setq directory (file-name-quote (expand-file-name directory))))
  (let ((relative (file-relative-name filename directory))
        (absolute (abbreviate-file-name (file-name-unquote filename))))
    (if (< (length relative) (length absolute)) relative absolute)))

(defun elisp/ert/file--equal-p (file-1 file-2)
  "Return whether FILE-1 and FILE-2 are probably the same file.
This is more lenient than ‘file-equal-p’ because it also treats
exact copies as equal."
  (cl-check-type file-1 string)
  (cl-check-type file-2 string)
  (or (file-equal-p file-1 file-2)
      (when-let ((attributes-1 (file-attributes file-1))
                 (attributes-2 (file-attributes file-2)))
        (and (equal (file-attribute-type attributes-1)
                    (file-attribute-type attributes-2))
             (eql (file-attribute-size attributes-1)
                  (file-attribute-size attributes-2))
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents-literally file-1)
               (let ((buffer-1 (current-buffer)))
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (insert-file-contents-literally file-2)
                   (let ((buffer-2 (current-buffer))
                         (case-fold-search nil))
                     (eql (compare-buffer-substrings buffer-1 nil nil
                                                     buffer-2 nil nil)
                          0)))))))))

(defun elisp/ert/sanitize--xml (tree)
  "Return a sanitized version of the XML TREE."
  ;; This is necessary because ‘xml-print’ sometimes generates invalid XML,
  ;; cf. https://debbugs.gnu.org/41094.  Use a hashtable to avoid infinite loops
  ;; on cyclic data structures.
  (cl-check-type tree list)
  (let ((map (make-hash-table :test #'eq))
        (marker (cons nil nil)))
    (cl-labels ((walk (obj)
                  (let ((existing (gethash obj map marker)))
                    (if (not (eq existing marker)) existing
                      (let ((new (cl-etypecase obj
                                   (symbol (elisp/ert/check--xml-name obj))
                                   (string (elisp/ert/sanitize--xml-string obj))
                                   ((and list (satisfies proper-list-p))
                                    (mapcar #'walk obj))
                                   (cons
                                    (cons (walk (car obj)) (walk (cdr obj)))))))
                        (puthash obj new map)
                        new)))))
      (walk tree))))

(defun elisp/ert/check--xml-name (symbol)
  "Check that SYMBOL maps to a valid XML name.
Return SYMBOL."
  (cl-check-type symbol symbol)
  (let ((name (symbol-name symbol)))
    ;; Allow only known-safe characters in tags.  Also see
    ;; https://www.w3.org/TR/xml/#sec-common-syn.
    (when (or (string-prefix-p "xml" name :ignore-case)
              (not (string-match-p (rx bos (any "a-z" "A-Z" ?_)
                                       (* (any "a-z" "A-Z" "0-9" ?- ?_)) eos)
                                   name)))
      (error "Invalid XML symbol %s" symbol)))
  symbol)

(defun elisp/ert/sanitize--xml-string (string)
  "Return a sanitized variant of STRING containing only valid XML characters."
  (cl-check-type string string)
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     ;; https://www.w3.org/TR/xml/#charsets
     (rx (not (any #x9 #xA #xD (#x20 . #xD7FF) (#xE000 . #xFFFD)
                   (#x10000 . #x10FFFF))))
     (lambda (s)
       (let ((c (string-to-char s)))
         (format (if (< c #x10000) "\\u%04X" "\\U%08X") c)))
     string :fixedcase :literal)))

(defun elisp/ert/edebug--unique (_cursor spec)
  "Handle the ‘:unique’ Edebug specification.
SPEC is the prefix for ‘gensym’."
  (cl-check-type spec string)
  ;; See ‘edebug-match-colon-name’ for a similar function.
  (let ((suffix (gensym spec)))
    (setq edebug-def-name
          (if edebug-def-name
              (intern (format "%s@%s" edebug-def-name suffix))
            suffix)))
  nil)

(declare-function elisp/runfiles/file-handler "elisp/runfiles/runfiles"
                  (operation &rest args))

(provide 'elisp/ert/runner)
;;; runner.el ends here
