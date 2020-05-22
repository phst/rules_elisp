;;; runner.el --- run ERT tests with Bazel      -*- lexical-binding: t; -*-

;; Copyright 2020 Google LLC
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
;; https://docs.bazel.build/versions/3.0.0/test-encyclopedia.html#initial-conditions.

;;; Code:

(require 'backtrace nil :noerror)  ; only in Emacs 27
(require 'bytecomp)
(require 'cl-lib)
(require 'debug)
(require 'edebug)
(require 'ert)
(require 'format)
(require 'nadvice)
(require 'pp)
(require 'rx)
(require 'subr-x)
(require 'xml)

(add-to-list 'command-switch-alist (cons "--skip-test" #'elisp/ert/skip-test))
(add-to-list 'command-switch-alist (cons "--skip-tag" #'elisp/ert/skip-tag))

(defun elisp/ert/run-batch-and-exit ()
  "Run ERT tests in batch mode.
This is similar to ‘ert-run-tests-batch-and-exit’, but uses the
TESTBRIDGE_TEST_ONLY environmental variable as test selector.
Treat all remaining command-line arguments as names of test
source files and load them."
  (or noninteractive (error "This function works only in batch mode"))
  (let* ((attempt-stack-overflow-recovery nil)
         (attempt-orderly-shutdown-on-fatal-signal nil)
         (edebug-initial-mode 'Go-nonstop)  ; ‘step’ doesn’t work in batch mode
         (source-dir (getenv "TEST_SRCDIR"))
         (temp-dir (getenv "TEST_TMPDIR"))
         (temporary-file-directory (concat "/:" temp-dir))
         (report-file (getenv "XML_OUTPUT_FILE"))
         (random-seed (or (getenv "TEST_RANDOM_SEED") ""))
         (shard-count (string-to-number (or (getenv "TEST_TOTAL_SHARDS") "1")))
         (shard-index (string-to-number (or (getenv "TEST_SHARD_INDEX") "0")))
         (shard-status-file (getenv "TEST_SHARD_STATUS_FILE"))
         (coverage-enabled (equal (getenv "COVERAGE") "1"))
         (coverage-dir (getenv "COVERAGE_DIR"))
         (selector (elisp/ert/make--selector))
         ;; If coverage is enabled, check for a file with a well-known
         ;; extension first.  The Bazel runfiles machinery is expected to
         ;; generate these files for source files that should be instrumented.
         ;; See the commentary in //elisp:defs.bzl for details.
         (load-suffixes (if coverage-enabled
                            (cons ".el.instrument" load-suffixes)
                          load-suffixes))
         (load-buffers ()))
    ;; TEST_SRCDIR and TEST_TMPDIR are required,
    ;; cf. https://docs.bazel.build/versions/3.1.0/test-encyclopedia.html#initial-conditions.
    (and (member source-dir '(nil "")) (error "TEST_SRCDIR not set"))
    (and (member temp-dir '(nil "")) (error "TEST_TMPDIR not set"))
    (and coverage-enabled (member coverage-dir '(nil ""))
         (error "Coverage requested but COVERAGE_DIR not set"))
    (unless (and (natnump shard-count) (natnump shard-index)
                 (< shard-index shard-count))
      (error "Invalid SHARD_COUNT (%s) or SHARD_INDEX (%s)"
             shard-count shard-index))
    (when coverage-enabled
      ;; We don’t bother removing the advice since we are going to kill Emacs
      ;; anyway.
      (add-function
       :before-until load-source-file-function
       (lambda (fullname file _noerror _nomessage)
         ;; If we got a magic filename that tells us to instrument a file, then
         ;; instrument the corresponding source file if that exists.  See the
         ;; commentary in //elisp:defs.bzl for details.  In all other cases, we
         ;; defer to the normal ‘load-source-file-function’, which is also
         ;; responsible for raising errors if desired.
         (when (string-suffix-p ".el.instrument" fullname)
           (cl-callf2 string-remove-suffix ".instrument" fullname)
           (cl-callf2 string-remove-suffix ".instrument" file)
           (when (file-readable-p fullname)
             (push (elisp/ert/load--instrument fullname file) load-buffers)
             t)))))
    (random random-seed)
    (when shard-status-file
      (write-region "" nil (concat "/:" shard-status-file) :append))
    (mapc #'load command-line-args-left)
    (let ((tests (ert-select-tests selector t))
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
      (dolist (test tests)
        (message "Running test %s" (ert-test-name test))
        (let* ((name (ert-test-name test))
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
               (report nil))
          (message "Test %s %s and took %d ms" name status
                   (* (float-time duration) 1000))
          (unless expected
            (cl-incf unexpected)
            ;; Print a nice error message that should point back to the source
            ;; file in a compilation buffer.
            (elisp/ert/log--error name
                                  (format-message "Test %s %s" name status)))
          (and failed (cl-incf failures))
          (and (not expected) (not failed) (cl-incf errors))
          (when (ert-test-skipped-p result)
            (cl-incf skipped)
            (setq report '((skipped))))
          (and (not expected) (ert-test-passed-p result)
               ;; Fake an error so that the test is marked as failed in the XML
               ;; report.
               (setq report '((failure ((message . "Test passed unexpectedly")
                                        (type . "error"))))))
          (when (ert-test-result-with-condition-p result)
            (let ((message (elisp/ert/failure--message name result))
                  (condition (ert-test-result-with-condition-condition result)))
              (message "%s" message)
              (unless expected
                (setq report `((,(if failed 'failure 'error)
                                ((message . ,(error-message-string condition))
                                 (type . ,(symbol-name (car condition))))
                                ,message))))))
          (push `(testcase ((name . ,(symbol-name name))
                            ;; classname is required, but we don’t have test
                            ;; classes, so fill in a dummy value.
                            (classname . "ERT")
                            (time . ,(format-time-string "%s.%N" duration)))
                           ,@report)
                test-reports)))
      (message "Running %d tests finished, %d results unexpected"
               (length tests) unexpected)
      (unless (member report-file '(nil ""))
        (with-temp-buffer
          ;; The expected format of the XML output file isn’t well-documented.
          ;; https://docs.bazel.build/versions/3.0.0/test-encyclopedia.html#initial-conditions
          ;; only states that the XML file is “ANT-like.”
          ;; https://llg.cubic.org/docs/junit/ and
          ;; https://help.catchsoftware.com/display/ET/JUnit+Format contain a
          ;; bit of documentation.
          (xml-print
           (elisp/ert/sanitize--xml
            `((testsuite
               ((name . "ERT")  ; required
                (hostname . "localhost")  ; required
                (tests . ,(number-to-string (length tests)))
                (errors . ,(number-to-string errors))
                (failures . ,(number-to-string failures))
                (skipped . ,(number-to-string skipped))
                (time . ,(format-time-string "%s.%N"
                                             (time-subtract nil start-time)))
                ;; No timezone or fractional seconds allowed.
                (timestamp . ,(format-time-string "%FT%T" start-time)))
               (properties)  ; required
               ,@(nreverse test-reports)
               (system-out) (system-err)))))
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region nil nil (concat "/:" report-file)
                          nil nil nil 'excl))))
      (when coverage-enabled
        (elisp/ert/write--coverage-report (concat "/:" coverage-dir)
                                          load-buffers))
      (kill-emacs (min unexpected 1)))))

(defvar elisp/ert/skip--tests nil
  "Test symbols to be skipped.
This list is populated by --skip-test command-line options.")

(defvar elisp/ert/skip--tags nil
  "Test tags to be skipped.
This list is populated by --skip-tag command-line options.")

(defun elisp/ert/skip-test (_arg)
  "Handle the --skip-test command-line argument."
  (let ((test (pop command-line-args-left)))
    (or test (error "Missing value for --skip-test option"))
    (push (intern test) elisp/ert/skip--tests)))

(defun elisp/ert/skip-tag (_arg)
  "Handle the --skip-tag command-line argument."
  (let ((tag (pop command-line-args-left)))
    (or tag (error "Missing value for --skip-tag option"))
    (push (intern tag) elisp/ert/skip--tags)))

(defun elisp/ert/make--selector ()
  "Build an ERT selector from environment and command line."
  ;; We optimize the test selector somewhat.  It’s displayed to the user if no
  ;; test matches, and then we’d like to avoid empty branches such as ‘(and)’.
  (cl-flet ((combine (op def elts) (cond ((null elts) def)
                                         ((cdr elts) `(,op ,@elts))
                                         (t (car elts))))
            (invert (sel) (if sel `(not ,sel) t)))
    (let* ((test-filter (getenv "TESTBRIDGE_TEST_ONLY"))
           (filter (if (member test-filter '(nil "")) t (read test-filter)))
           (skip-tags
            (invert
             (combine 'or nil (nreverse (mapcar (lambda (tag) `(tag ,tag))
                                                elisp/ert/skip--tags)))))
           (skip-tests
            (invert (combine 'member nil (reverse elisp/ert/skip--tests)))))
      (combine 'and t (delq t (list filter skip-tags skip-tests))))))

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
      (cond ((fboundp 'backtrace-to-string)  ; Emacs 27
             (insert (backtrace-to-string backtrace)))
            ((fboundp 'debugger-insert-backtrace)  ; Emacs 26
             (debugger-insert-backtrace backtrace nil))
            (t (error "Unsupported Emacs version")))
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
        (edebug-all-defs t))
    (with-current-buffer buffer
      (insert-file-contents fullname :visit)
      ;; The file buffer needs to be current for Edebug
      ;; instrumentation to work.
      (eval-buffer buffer nil fullname nil :do-allow-print)
      ;; Yuck!  We have to mess with internal Edebug data here.
      ;; Byte-compile all functions to be a bit more realistic.
      (dolist (data edebug-form-data)
        (byte-compile (edebug--form-data-name data))))
    (do-after-load-evaluation fullname)
    (progress-reporter-done reporter)
    buffer))

(defun elisp/ert/write--coverage-report (coverage-dir buffers)
  "Write a coverage report to a file in COVERAGE-DIR.
BUFFERS is a list of buffers containing Emacs Lisp sources
instrumented using Edebug."
  (cl-check-type coverage-dir string)
  (cl-check-type buffers list)
  (with-temp-buffer
    (let ((coding-system-for-write 'utf-8-unix)
          (root (getenv "TEST_SRCDIR")))
      (dolist (buffer buffers)
        (elisp/ert/insert--coverage-report buffer root)
        (kill-buffer buffer))
      (write-region nil nil (expand-file-name "emacs-lisp.dat" coverage-dir)
                    nil nil nil 'excl))))

(defun elisp/ert/insert--coverage-report (buffer root)
  "Insert a coverage report into the current buffer.
BUFFER must be a different buffer visiting an Emacs Lisp source
file that has been instrumented with Edebug.  ROOT is a directory
to be used as root."
  (cl-check-type buffer buffer-live)
  (cl-check-type root string)
  (let ((file-name (elisp/ert/sanitize--string
                    (file-relative-name (buffer-file-name buffer) root)))
        (functions ())
        (functions-hit 0)
        (lines (make-hash-table :test #'eql)))
    (with-current-buffer buffer
      (widen)
      ;; Yuck!  More messing around with Edebug internals.
      (dolist (data edebug-form-data)
        (let* ((name (edebug--form-data-name data))
               (frequencies (get name 'edebug-freq-count))
               ;; We don’t really know the number of function calls,
               ;; so assume it’s the same as the hit count of the
               ;; first breakpoint.
               (calls (or (cl-find 0 frequencies :test-not #'eq) 0))
               (stuff (get name 'edebug))
               (begin (car stuff))
               (offsets (caddr stuff)))
          (cl-incf functions-hit (min calls 1))
          (cl-assert (eq (marker-buffer begin) buffer))
          (cl-assert (eql (length frequencies) (length offsets)))
          (cl-loop for offset across offsets
                   ;; This can’t be ‘and’ due to
                   ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40727.
                   for freq across frequencies
                   for position = (+ begin offset)
                   for line = (line-number-at-pos position)
                   do (cl-callf max (gethash line lines 0) freq))
          (push (list (elisp/ert/sanitize--string (symbol-name name))
                      (line-number-at-pos begin) calls)
                functions))))
    (cl-callf nreverse functions)
    ;; The expected format is described to some extend in the
    ;; geninfo(1) man page.
    (insert (format "SF:%s\n" file-name))
    (dolist (func functions)
      (insert (format "FN:%d,%s\n" (cadr func) (car func))))
    (dolist (func functions)
      (insert (format "FNDA:%d,%s\n" (caddr func) (car func))))
    (insert (format "FNF:%d\n" (length functions)))
    (insert (format "FNH:%d\n" functions-hit))
    (let ((list ())
          (lines-hit 0))
      (maphash (lambda (line freq) (push (cons line freq) list)) lines)
      (cl-callf sort list #'car-less-than-car)
      (dolist (line list)
        (cl-incf lines-hit (min (cdr line) 1))
        (insert (format "DA:%d,%d\n" (car line) (cdr line))))
      (insert (format "LH:%d\nLF:%d\nend_of_record\n"
                      lines-hit (hash-table-count lines))))))

(defun elisp/ert/log--error (test message)
  "Log an error for TEST.
TEST should be an ERT test symbol.  MESSAGE is the error message.
If possible, format the message according to the
GNU Coding Standards; see Info node ‘(standards) Errors’."
  (cl-check-type message string)
  (cl-check-type test symbol)
  ;; Yuck!  ‘ert--test’ is an implementation detail.
  (when-let ((file (symbol-file test 'ert--test)))
    ;; The filename typically refers to a compiled file in the execution root.
    ;; Try to resolve it to a source file.  See
    ;; https://docs.bazel.build/versions/3.1.0/output_directories.html#layout-diagram.
    (when (string-match (rx "/execroot/"
                            (+ (not (any ?/))) ?/ ; workspace
                            (+ (not (any ?/))) ?/ ; bazel-out
                            (+ (not (any ?/))) ?/ ; configuration
                            "bin/"
                            (group (+ nonl)) ".elc" eos)
                        file)
      ;; We can use the filename relative to the Bazel binary directory since
      ;; that corresponds to a source filename relative to some workspace root.
      ;; ‘find-function-search-for-symbol’ will find the corresponding source
      ;; file because all workspace roots are in the ‘load-path’.
      (cl-callf2 match-string-no-properties 1 file))
    (let ((buffers (buffer-list))
          (directory default-directory)
          ;; Try to print nice and short filenames.  We do this by using the
          ;; filename relative to the test working directory
          ;; (i.e. $TEST_SRCDIR/$TEST_WORKSPACE).  Prevent both ‘find-file’ and
          ;; ‘vc-refresh-state’ from following symbolic links to the original
          ;; source file.
          (find-file-visit-truename nil)
          (vc-handled-backends nil))
      (when-let ((definition
                   ;; ‘find-function-search-for-symbol’ signals errors if it
                   ;; can’t find the library.  Since we’re only attempting to
                   ;; print a log message here, ignore them and move on.
                   (ignore-errors
                     ;; Yuck!  ‘ert--test’ is an implementation detail.
                     (find-function-search-for-symbol test 'ert--test file))))
        (cl-destructuring-bind (buffer . point) definition
          (with-current-buffer buffer
            (message "%s:%d: %s"
                     (file-relative-name buffer-file-name directory)
                     (line-number-at-pos point :absolute)
                     message)
            ;; If ‘find-function-search-for-symbol’ has created a new buffer,
            ;; kill it.
            (unless (memq buffer buffers) (kill-buffer))))))))

(defun elisp/ert/sanitize--string (string)
  "Return a sanitized version of STRING for the coverage file."
  (cl-check-type string string)
  ;; The coverage file is line-based, so the string shouldn’t contain any
  ;; newlines.
  (replace-regexp-in-string (rx (not (any alnum blank punct))) "?" string))

;; This polyfill needs to be defined before using it as type to prevent errors
;; during compilation.
(defalias 'elisp/ert/proper--list-p
  (if (fboundp 'proper-list-p) 'proper-list-p 'format-proper-list-p))

(defun elisp/ert/sanitize--xml (tree)
  "Return a sanitized version of the XML TREE."
  ;; This is necessary because ‘xml-print’ sometimes generates invalid XML,
  ;; cf. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=41094.  Use a hashtable
  ;; to avoid infinite loops on cyclic data structures.
  (cl-check-type tree list)
  (let ((map (make-hash-table :test #'eq))
        (marker (cons nil nil)))
    (cl-labels ((walk
                 (obj)
                 (let ((existing (gethash obj map marker)))
                   (if (not (eq existing marker)) existing
                     (let ((new (cl-etypecase obj
                                  (symbol (elisp/ert/check--xml-name obj))
                                  (string (elisp/ert/sanitize--xml-string obj))
                                  (elisp/ert/proper--list (mapcar #'walk obj))
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
  (replace-regexp-in-string
   ;; https://www.w3.org/TR/xml/#charsets
   (rx (not (any #x9 #xA #xD (#x20 . #xD7FF) (#xE000 . #xFFFD)
                 (#x10000 . #x10FFFF))))
   (lambda (s)
     (let ((c (string-to-char s)))
       (format (if (< c #x10000) "\\u%04X" "\\U%08X") c)))
   string :fixedcase :literal))

(provide 'elisp/ert/runner)
;;; runner.el ends here
