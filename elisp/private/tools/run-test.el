;;; run-test.el --- run ERT tests with Bazel      -*- lexical-binding: t; -*-

;; Copyright 2020-2025 Google LLC
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
(require 'xml)


;;;; Message formatting:

(defun @failure-message (name result)
  "Return a failure message for the RESULT of a failing test.
NAME is the name of the test."
  (declare (ftype (function (symbol ert-test-result-with-condition) string))
           (side-effect-free error-free))
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
      (pcase-dolist (`(,prefix . ,message) infos)
        (insert "  " prefix message ?\n))
      (insert (format-message "\n  Test %s condition:\n\n" name))
      (let ((point (point)))
        (pp (ert-test-result-with-condition-condition result)
            (current-buffer))
        (indent-rigidly point (point) 4))
      (insert ?\n)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun @message-prefix (test)
  "Return a message prefix for TEST.
TEST should be an ERT test symbol.  If possible, return a prefix
of the form “FILE:LINE” as described in the GNU Coding Standards;
see Info node ‘(standards) Errors’.  If the file and line can’t
be determined, return nil."
  (declare (ftype (function (symbol) (or null string))))
  (cl-check-type test symbol)
  (let ((case-fold-search nil)
        (directory default-directory))
    ;; Yuck!  ‘ert--test’ is an implementation detail.
    (when-let ((file (symbol-file test 'ert--test)))
      ;; The filename typically refers to a compiled file in the execution root.
      ;; Try to resolve it to a source file.  See
      ;; https://bazel.build/remote/output-directories#layout-diagram.
      (when (string-match (rx "/execroot/"
                              (+ (not (any ?/))) ?/ ; repository
                              (+ (not (any ?/))) ?/ ; bazel-out
                              (+ (not (any ?/))) ?/ ; configuration
                              "bin/"
                              (group (+ nonl)) ".elc" eos)
                          file)
        ;; We can use the filename relative to the Bazel binary directory since
        ;; that corresponds to a source filename relative to some repository
        ;; root.  ‘find-library-name’ will find the corresponding source file
        ;; because all repository roots are in the ‘load-path’.
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
                 (rx bol (* space) "(ert-deftest" (+ space)
                     (literal (symbol-name test)) (or space eol))
                 nil t)
            ;; Try to print nice and short filenames.  We do this by using the
            ;; filename relative to the test working directory
            ;; (i.e. $TEST_SRCDIR/$TEST_WORKSPACE).
            (format "%s:%d"
                    (@file-display-name file directory)
                    (line-number-at-pos))))))))


;;;; XML report generation:

(defun @write-report (file start-time tests stats failure-messages)
  "Write XML report to FILE.
START-TIME is a time value specifying when the test run started, TESTS
is a list of ‘ert-test’ structures, STATS is an ERT statistics object,
and FAILURE-MESSAGES is a hash table mapping ‘ert-test’ structures to
failure messages."
  (declare (ftype (function (string t list cl-structure-object hash-table) t)))
  (cl-check-type file string)
  (cl-check-type tests list)
  (cl-check-type stats cl-structure-object)
  (cl-check-type failure-messages hash-table)
  (let ((errors 0)
        (failures 0)
        (test-reports ())
        (coding-system-for-write 'utf-8-unix)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (dolist (test tests)
      (when-let ((result (ert-test-most-recent-result test)))
        (let* ((name (ert-test-name test))
               (duration (ert-test-result-duration result))
               (expected (ert-test-result-expected-p test result))
               (failed
                (and (not expected)
                     ;; Only actual failures reported with ‘ert-fail’ count as
                     ;; failures, other signals count as errors.  See the
                     ;; distinction in JUnit.xsd and
                     ;; https://stackoverflow.com/a/3426034.
                     (or (and (ert-test-failed-p result)
                              (eq (car (ert-test-failed-condition result))
                                  'ert-test-failed))
                         ;; A test that passed unexpectedly should count as
                         ;; failed for the XML report.
                         (ert-test-passed-p result))))
               (error (and (not expected) (not failed)))
               (skipped (ert-test-skipped-p result))
               (tag (cond (failed 'failure)
                          (error 'error)
                          (skipped 'skipped)))
               (failure-message nil)
               (type nil)
               (description nil))
          (when failed (cl-incf failures))
          (when error (cl-incf errors))
          (and (not expected) (ert-test-passed-p result)
               ;; Fake an error so that the test is marked as failed in the
               ;; XML report.
               (setq failure-message "Test passed unexpectedly"
                     type 'error))
          (and (not expected) (ert-test-aborted-with-non-local-exit-p result)
               (setq failure-message "Test aborted"
                     type 'error))
          (when (ert-test-result-with-condition-p result)
            (let ((condition (ert-test-result-with-condition-condition result)))
              (setq failure-message (error-message-string condition)
                    description (gethash test failure-messages))
              (unless expected
                (setq type (car condition)))))
          (push `(testcase
                  ((name . ,(symbol-name name))
                   ;; classname is required, but we don’t have test classes, so
                   ;; fill in a dummy value.
                   (classname . "ERT")
                   (time . ,(format-time-string "%s.%3N" duration)))
                  ,@(when tag
                      `((,tag ((message . ,(cl-the string failure-message))
                               ,@(when type `((type . ,(symbol-name type)))))
                              ,@(when description `(,description))))))
                test-reports))))
    (with-temp-file file
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      ;; The expected format of the XML output file isn’t well-documented.
      ;; https://bazel.build/reference/test-encyclopedia#initial-conditions only
      ;; states that the XML file is “based on the JUnit test result schema”,
      ;; referring to https://windyroad.com.au/dl/Open%20Source/JUnit.xsd.
      ;; https://llg.cubic.org/docs/junit/ and
      ;; https://help.catchsoftware.com/display/ET/JUnit+Format contain a bit of
      ;; documentation.  Sanitizing the tree is necessary because ‘xml-print’
      ;; sometimes generates invalid XML, cf. https://debbugs.gnu.org/41094.
      (cl-labels ((walk (obj)
                    (cl-etypecase obj
                      (symbol (@check-xml-name obj))
                      (string (@sanitize-xml-string obj))
                      ((and list (satisfies proper-list-p)) (mapcar #'walk obj))
                      (cons (cons (walk (car obj)) (walk (cdr obj)))))))
        (xml-print
         (walk
          `((testsuite
             ((name . "ERT")            ; required
              (hostname . "localhost")  ; required
              (tests . ,(number-to-string (ert-stats-completed stats)))
              (errors . ,(number-to-string errors))
              (failures . ,(number-to-string failures))
              (skipped . ,(number-to-string (ert-stats-skipped stats)))
              (time . ,(format-time-string "%s.%3N"
                                           (time-subtract nil start-time)))
              ;; No timezone or fractional seconds allowed.
              (timestamp . ,(format-time-string "%FT%T" start-time)))
             (properties () (property ((name . "emacs-version")
                                       (value . ,emacs-version))))
             ,@(nreverse test-reports)
             (system-out) (system-err)))))))))

(defun @check-xml-name (symbol)
  "Check that SYMBOL maps to a valid XML name.
Return SYMBOL."
  (declare (ftype (function (symbol) symbol))
           (side-effect-free t))
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

(defun @sanitize-xml-string (string)
  "Return a sanitized variant of STRING containing only valid XML characters."
  (declare (ftype (function (string) string))
           (side-effect-free error-free))
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


;;;; Coverage support:

(defun @load-instrument (fullname file)
  "Load and instrument the Emacs Lisp file FULLNAME.
FILE is an abbreviated name as described in ‘load-source-file-function’,
which see.  Return a live buffer containing the file contents."
  (declare (ftype (function (string string) buffer)))
  (cl-check-type fullname string)
  (cl-check-type file string)
  ;; Similar to testcover.el, we use Edebug to collect coverage information.
  (let ((reporter (make-progress-reporter
                   (format-message "Loading and instrumenting %s..." file)))
        (format-alist nil)
        (after-insert-file-functions nil)
        cloned-buffer)
    (load-with-code-conversion
     fullname file nil :nomessage
     (lambda (buffer file)
       ;; ‘load-with-code-conversion’ unconditionally kills the buffer being
       ;; evaluated, so we need to clone it.
       (with-current-buffer buffer
         (setq cloned-buffer (clone-buffer (format "*%s*" file))))
       ;; The file buffer needs to be current for Edebug instrumentation to
       ;; work.
       (with-current-buffer cloned-buffer
         (let ((edebug-all-defs t)
               (edebug-new-definition-function #'@new-definition)
               (edebug-after-instrumentation-function #'@after-instrumentation))
           (eval-buffer cloned-buffer nil file nil :do-allow-print)
           ;; Yuck!  We have to mess with internal Edebug data here.
           ;; Byte-compile all functions to be a bit more realistic.
           (dolist-with-progress-reporter (data edebug-form-data)
               "Compiling instrumented forms..."
             (byte-compile (edebug--form-data-name data))))
         (setq-local @source-file file))))
    (progress-reporter-done reporter)
    cloned-buffer))

(defun @new-definition (name)
  "Enable line coverage collection for NAME.
This can be used as ‘edebug-new-definition-function’."
  (declare (ftype (function (symbol) t)))
  (cl-check-type name symbol)
  ;; Check for duplicate names, if possible.  Duplicates cause subtle errors
  ;; that are otherwise very hard to debug, cf. https://debbugs.gnu.org/41853.
  (when (get name 'edebug-behavior)
    (error "Symbol ‘%s’ instrumented twice" name))
  (put name 'edebug-behavior '@coverage)
  (let ((offsets (caddr (get name 'edebug))))
    ;; Unlike Edebug, we initialize the coverage data to nil and only set the
    ;; “before” entry to a non-nil value so that we can easily distinguish
    ;; between “before” and “after” positions later.
    (put name '@coverage (make-vector (length offsets) nil))))

(defun @after-instrumentation (form)
  "Instrument FORM to collect line and branch coverage information.
This can be used as ‘edebug-after-instrumentation-function’.
Return FORM."
  (declare (ftype (function (t) t)))
  (let ((seen (make-hash-table :test #'eq)))
    (@instrument-form seen nil form))
  form)

;; Emacs 29 incorrectly warns about overly long docstrings in generated code.
;; FIXME: Remove suppression once we drop support for Emacs 29.
(with-suppressed-warnings ((docstrings nil))
  (cl-defstruct (@coverage-data
                 (:constructor @make-coverage-data)
                 (:copier nil))
    "Coverage data for a specific form.
The ‘edebug-after-instrumentation-function’ initializes the ‘@coverage’
property of each instrumented symbol to a vector.  Some elements of the
vector will be objects of this type."
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
if no branch element should be incremented.")))

(defun @instrument-form (seen vector form)
  "Instrument FORM to collect line coverage information.
SEEN is a hashtable used to prevent infinite recursion.  VECTOR is
either nil (for a toplevel definition) or a vector of optional
‘@coverage-data’ objects with the same length as the offset vector.  The
vector is attached to the ‘@coverage’ property of the symbol being
defined."
  (declare (ftype (function (hash-table (or null vector)) t)))
  (cl-check-type seen hash-table)
  (cl-check-type vector (or null vector))
  (unless (gethash form seen)
    (puthash form t seen)
    (pcase form
      (`(edebug-enter ',func ,_args ,body)
       (let ((vector (get func '@coverage)))
         (cl-check-type vector vector)  ; set by ‘@new-definition’
         (@instrument-form seen vector body)))
      ((or `(edebug-after (edebug-before ,index) ,_after-index ,form)
           `(edebug-after ,_ ,index ,form))
       (cl-check-type vector vector)  ; set by ‘@new-definition’
       (cl-check-type index natnum)
       (cl-check-type (aref vector index) null)  ; not yet prepared
       ;; We prefer setting the “before” entry to a non-nil value so that we can
       ;; easily distinguish between “before” and “after” positions later.  We
       ;; have to do this in ‘edebug-after-instrumentation-function’ because
       ;; otherwise we couldn’t distinguish between forms that aren’t
       ;; instrumented and forms that are instrumented but not executed.  The
       ;; second branch of the ‘or’ above is chosen for forms without a “before”
       ;; entry, in which case we have to fall back to the “after” entry.
       (let ((data (aset vector index (@make-coverage-data))))
         (@instrument-form seen vector form)
         ;; Determine whether this is a branching form.  If so, generate a
         ;; branch frequency vector and attach it to DATA.
         (when-let ((branches (@instrument-branches vector form)))
           (setf (@coverage-data-branches data) branches))))
      ((pred proper-list-p)
       ;; Use ‘dolist’ where possible to avoid deep recursion.
       (dolist (element form)
         (@instrument-form seen vector element)))
      (`(,car . ,cdr)
       (@instrument-form seen vector car)
       (@instrument-form seen vector cdr))
      ((pred vectorp)
       (cl-loop for element across form
                do (@instrument-form seen vector element)))
      ;; Literals that can’t be instrumented.
      ((or (pred symbolp) (pred numberp) (pred stringp)))
      ;; Everything else is unexpected.
      (_ (signal 'elisp/syntax-error (list form))))))

(define-error 'elisp/syntax-error "Syntax error" 'invalid-read-syntax)

(defun @instrument-branches (vector form)
  "Instrument a branching FORM.
VECTOR is the coverage data vector attached to the ‘@coverage’ property
of the symbol being defined.  Return nil if FORM doesn’t define a
branching construct.  Otherwise, return a new vector containing
per-branch frequencies (hit counts).  If a branch can’t be instrumented,
the corresponding element in the return value will be nil."
  (declare (ftype (function (vector t) (or null vector))))
  (cl-check-type vector vector)
  (pcase form
    (`(,(or 'if 'when 'unless 'while) ,cond . ,_)
     (let ((branches (vector nil nil)))
       (@instrument-branch vector branches (list cond) nil 0 1)
       branches))
    (`(,(or 'and 'or) . ,(and (pred proper-list-p) conditions))
     ;; The last condition form won’t introduce a new branch.
     (cl-callf butlast conditions)
     ;; Assume that each remaining condition introduces two branches.  This
     ;; isn’t quite correct, but should be good enough.
     (let ((branches (make-vector (* 2 (length conditions)) nil)))
       (cl-loop for form in conditions
                and index from 0 by 2
                do (@instrument-branch vector branches (list form)
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
                do (@instrument-branch vector branches clause index nil nil))
       branches))
    ((or `(,(or 'condition-case 'condition-case-unless-debug)
           ,_var ,(and bodyform (let body (list bodyform))) .
           ,(and (pred proper-list-p) clauses))
         `(handler-bind ,(and (pred proper-list-p) clauses) . ,body))
     ;; Similar to the ‘cond’ case, but here we have one branch for each handler
     ;; as well as one branch for a successful exit.
     (let ((branches (make-vector (1+ (length clauses)) nil)))
       ;; Specifying a THEN-INDEX and ELSE-INDEX instead of a BRANCH-INDEX
       ;; forces evaluation after the form finishes, which is exactly what we
       ;; want here.
       (@instrument-branch vector branches body nil 0 0)
       (cl-loop for clause in clauses
                and index from 1
                when (listp clause)  ; gently skip over syntax errors
                do (@instrument-branch vector branches clause index nil nil))
       branches))
    (`(,(or 'if-let 'when-let) (,(pred symbolp) ,expr) . ,_)
     (let ((branches (vector nil nil)))
       (@instrument-branch vector branches (list expr) nil 0 1)
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
                      (@instrument-branch vector branches (list form)
                                          nil index (1+ index)))))
       branches))
    (`(cl-loop . ,(and (pred proper-list-p) rest))
     (when-let ((conditions (cl-loop for (keyword form) on rest
                                     when (memq keyword '(if when unless))
                                     collect form)))
       (let ((branches (make-vector (* 2 (length conditions)) nil)))
         (cl-loop for form in conditions
                  and index from 0 by 2
                  do (@instrument-branch vector branches (list form)
                                         nil index (1+ index)))
         branches)))))

(defun @instrument-branch
    (vector branches forms branch-index then-index else-index)
  "Instrument a single branch of a branching form.
VECTOR is the coverage data vector attached to the ‘@coverage’ property
of the symbol being defined.  BRANCHES is the branch frequency vector
for the parent form.  FORMS is the list of forms to be instrumented.
BRANCH-INDEX, THEN-INDEX, and ELSE-INDEX will be used for the
‘branch-index’, ‘then-index’, and ‘else-index’ properties of the
‘@coverage-data’ object of the newly-instrumented form, respectively."
  (declare
   (ftype
    (function
     (vector vector list (or natnum null) (or natnum null) (or natnum null))
     t)))
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
      ;; Prefer the “before” spot, like ‘@instrument-form’.
      ((or `(edebug-after (edebug-before ,form-index) ,_after-index ,_form)
           `(edebug-after ,_ ,form-index ,_form))
       ;; The data object has been prepared by ‘@instrument-form’.
       (let ((data (aref vector form-index)))
         (cl-check-type data @coverage-data)
         (setf (@coverage-data-parent-branches data) branches
               (@coverage-data-branch-index data) branch-index
               (@coverage-data-then-index data) then-index
               (@coverage-data-else-index data) else-index))
       ;; Initialize branch frequency vector.  Some elements might remain nil,
       ;; if Edebug hasn’t generated any instrumentation for the form.
       (dolist (i (list branch-index then-index else-index))
         (when i (aset branches i 0)))
       (cl-return)))))

;; Innermost function being executed, dynamically bound by ‘@edebug-enter’.
(defvar @coverage-function)

;; Current coverage data vector, dynamically bound by ‘@edebug-enter’.
(defvar @coverage-vector)

(defun @edebug-enter (func args body)
  "Implementation of ‘edebug-enter’ for ERT coverage instrumentation.
See ‘edebug-enter’ for the meaning of FUNC, ARGS, and BODY."
  (declare (ftype (function (symbol list function) t)))
  (cl-check-type func symbol)
  (cl-check-type args list)
  (cl-check-type body function)
  (let ((@coverage-function func)
        (@coverage-vector (get func '@coverage)))
    (funcall body)))

(defun @edebug-before (before-index)
  "Implementation of ‘edebug-before’ for ERT coverage instrumentation.
BEFORE-INDEX is the index into ‘@frequency-vector’ for the beginning of
the form.  Return (before . BEFORE-INDEX)."
  (declare (ftype (function (natnum) cons)))
  (cl-check-type before-index natnum)
  (let ((data (@coverage-data before-index)))
    ;; Increment hit count.  We prefer doing that here because the beginning of
    ;; a form tends to be more interesting and the end, and we’d like to
    ;; increment the hit count for the first line of a form instead of the last.
    ;; See ‘@edebug-after’ for a case where this isn’t possible.
    (cl-incf (@coverage-data-hits data))
    ;; If this is a subform of a branching form and we’re supposed to
    ;; unconditionally increment a branch frequency, do so now.
    (when-let ((branches (@coverage-data-parent-branches data))
               (branch-index (@coverage-data-branch-index data)))
      (cl-incf (aref branches branch-index))))
  ;; The return value gets passed to the BEFORE-INDEX argument of
  ;; ‘edebug-after’.  Pick a form that allows it to distinguish this case from
  ;; the case of a plain variable, which doesn’t involve ‘edebug-before’.
  `(before . ,before-index))

(defun @edebug-after (before after-index value)
  "Implementation of ‘edebug-before’ for ERT coverage instrumentation.
BEFORE is normally of the form (before . BEFORE-INDEX).  BEFORE-INDEX
and AFTER-INDEX are the indices into ‘@frequency-vector’ for the
beginning and end of the form, respectively.  VALUE is the value of the
form.  Return VALUE."
  (declare (ftype (function (t natnum t) t)))
  (cl-check-type after-index natnum)
  ;; Edebug uses two different forms for instrumentation: For list forms it
  ;; emits (edebug-after (edebug-before BEFORE-INDEX) AFTER-INDEX form), but for
  ;; variables it just emits (edebug-after 0 AFTER-INDEX form).  We prefer
  ;; incrementing the hit count for the beginning of the form, see
  ;; ‘@edebug-before’.  However, where this isn’t possible, increment the hit
  ;; count for the end of them form.  This should only happen for variables that
  ;; rarely span more than one line.
  (let (incrementp form-index)
    (pcase before
      ;; Increment line hit count only if ‘edebug-before’ hasn’t incremented it
      ;; yet.
      (`(before . ,before-index) (setq incrementp nil form-index before-index))
      (_ (setq incrementp t form-index after-index)))
    (let ((data (@coverage-data form-index)))
      (when incrementp
        ;; Increment line hit count, because that hasn’t happened yet in
        ;; ‘edebug-before’.
        (cl-incf (@coverage-data-hits data)))
      ;; Check if this form is a condition of a branching form such as ‘if’.  If
      ;; so, increment the branch hit count for the “then” or “else” branch
      ;; depending on VALUE.  We need to do this in ‘edebug-after’ because only
      ;; then we know the return value of the form.
      (when-let ((branches (@coverage-data-parent-branches data))
                 (branch-index (if value
                                   (@coverage-data-then-index data)
                                 (@coverage-data-else-index data))))
        (cl-incf (aref branches branch-index)))))
  value)

(defun @coverage-data (index)
  "Return INDEX’th element of the current coverage vector.
The return value is of type ‘@coverage-data’."
  (declare (ftype (function (natnum) @coverage-data))
           (side-effect-free t))
  (cl-check-type index natnum)
  (let ((data (aref @coverage-vector index)))
    (unless data
      ;; This is typically an error in the Edebug specification of a macro being
      ;; expanded, not a bug in this library.  Give the user a more helpful
      ;; error message than “wrong type”.  To check which macro is the culprit,
      ;; look through the body of the ‘@coverage-function’.  The most
      ;; common error is to use ‘form’ where ‘def-form’ would be required; see
      ;; Info node ‘(elisp) Specification List’.
      (signal 'elisp/missing-coverage-data
              (list @coverage-function @coverage-vector index)))
    (cl-the @coverage-data data)))

(define-error 'elisp/missing-coverage-data "Missing coverage data")

(defun @write-coverage-report (coverage-dir buffers)
  "Write a coverage report to a file in COVERAGE-DIR.
BUFFERS is a list of buffers containing Emacs Lisp sources
instrumented using Edebug."
  (declare (ftype (function (string list) t)))
  (cl-check-type coverage-dir string)
  (cl-check-type buffers list)
  (with-temp-buffer
    (let ((coding-system-for-write 'utf-8-unix)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (when-let ((test-name (@getenv "TEST_TARGET")))
        (insert "TN:" (@sanitize-string test-name) ?\n))
      (dolist (buffer buffers)
        (@insert-coverage-report buffer)
        (kill-buffer buffer))
      (write-region nil nil (expand-file-name "emacs-lisp.dat" coverage-dir)
                    :append))))

(eval-when-compile
  (defmacro @hash-get-or-put (key table &rest body)
    "Return the value associated with KEY in TABLE.
If no such value exists, evaluate BODY and put its value into
TABLE."
    (declare (ftype (function (t t &rest t) t)) (indent 2) (debug t))
    (cl-once-only (key table)
      (cl-with-gensyms (value default)
        `(let* ((,default (cons nil nil)) ; unique object
                (,value (gethash ,key ,table ,default)))
           (if (eq ,value ,default)
               (puthash ,key ,(macroexp-progn body) ,table)
             ,value))))))

(defun @insert-coverage-report (buffer)
  "Insert a coverage report into the current buffer.
BUFFER must be a different buffer containing the contents of an Emacs
Lisp source file that has been instrumented with Edebug using
‘@load-instrument’."
  (declare (ftype (function (buffer) t)))
  (cl-check-type buffer buffer)
  (let ((file-name (@sanitize-string
                    (@file-display-name
                     (buffer-local-value '@source-file buffer))))
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
       for ours = (eq (get name 'edebug-behavior) '@coverage)
       for coverage = (get name (if ours '@coverage 'edebug-freq-count))
       for frequency = (if ours
                           (lambda (cov)
                             (and cov (@coverage-data-hits cov)))
                         #'identity)
       ;; We don’t really know the number of function calls, so assume it’s the
       ;; same as the hit count of the first breakpoint.
       for calls = (cl-loop for cov across coverage
                            for hits = (funcall frequency cov)
                            thereis (and (not (eql hits 0)) hits)
                            finally return 0)
       for (begin _ offsets) = (get name 'edebug)
       for begin-line = (line-number-at-pos begin)
       do
       (unless (eq (marker-buffer begin) buffer)
         (error "Function %s got redefined in some other file" name))
       (cl-incf functions-hit (min calls 1))
       (@hash-get-or-put begin-line lines calls)
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
        ;; see the discussion in ‘@edebug-after’.
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
              (when-let ((frequencies (@coverage-data-branches cov)))
                ;; Remove branches that Edebug didn’t instrument.
                (cl-callf2 cl-remove nil frequencies)
                ;; If fewer than two branches are left, we don’t really have any
                ;; meaningful branch coverage data.
                (when (> (length frequencies) 1)
                  (let* ((u (@hash-get-or-put line branches
                              (make-hash-table :test #'eql)))
                         (v (@hash-get-or-put offset u
                              (make-vector (length frequencies) 0))))
                    (cl-loop for f across frequencies
                             and n across-ref v
                             do (cl-callf max n f)))))))))
       (push (list begin-line
                   (@sanitize-string (symbol-name name))
                   calls)
             functions)))
    (cl-callf sort functions #'car-less-than-car)
    ;; The expected format is described to some extend in the
    ;; geninfo(1) man page.
    (insert (format "SF:%s\n" file-name))
    (pcase-dolist (`(,line ,name ,_calls) functions)
      (insert (format "FN:%d,%s\n" line name)))
    (pcase-dolist (`(,_line ,name ,calls) functions)
      (insert (format "FNDA:%d,%s\n" calls name)))
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

(defun @sanitize-string (string)
  "Return a sanitized version of STRING for the coverage file."
  (declare (ftype (function (string) string))
           (side-effect-free error-free))
  (cl-check-type string string)
  ;; The coverage file is line-based, so the string shouldn’t contain any
  ;; newlines.
  (let ((case-fold-search nil))
    (replace-regexp-in-string (rx (not (any alnum blank punct))) "?" string)))


;;;; Environment variable utilities:

(defun @getenv (name)
  "Return the value of the environment variable NAME.
If NAME is unset or set to the empty string, return nil."
  (declare (ftype (function (string) (or null string)))
           (side-effect-free error-free))
  (cl-check-type name string)
  (let ((value (getenv name)))
    (and value (not (string-empty-p value)) value)))

(defun @env-file (name)
  "Return a file name from the environment variable NAME.
If NAME is unset or set to the empty string, return nil.  Otherwise,
assume the file name always refers to a local file, and quote it."
  (declare (ftype (function (string) (or null string)))
           (side-effect-free error-free))
  (cl-check-type name string)
  (when-let ((value (@getenv name)))
    ;; Expand relative filenames so that they are unaffected by changes to
    ;; ‘default-directory’.
    (@expand-and-quote value)))


;;;; File utilities:

(defun @expand-and-quote (filename)
  "Make FILENAME absolute, and quote the result."
  ;; Note that ‘expand-file-name’ can’t expand quoted filenames, so expand
  ;; before quoting, but suppress any filename handler.
  (let ((file-name-handler-alist ()))
    (concat "/:" (expand-file-name filename))))

(defun @file-display-name (filename &optional directory)
  "Return a relative or absolute name for FILENAME, whichever is shorter.
DIRECTORY is the directory that could contain FILENAME."
  (declare (ftype (function (string &optional string) string)))
  (cl-check-type filename string)
  (cl-check-type directory (or null string))
  (unless directory (setq directory default-directory))
  ;; If we got a virtual runfile filename but a physical directory, make an
  ;; educated guess about the location relative to the test working directory.
  (and (string-prefix-p "/bazel-runfile:" filename)
       (not (string-prefix-p "/bazel-runfile:" directory))
       (let ((repository (file-name-nondirectory
                          (directory-file-name directory))))
         (setq directory (concat "/bazel-runfile:" repository
                                 (unless (string-empty-p repository) "/")))))
  ;; Work around https://debbugs.gnu.org/46219.
  (unless (file-remote-p filename)
    (cl-callf @expand-and-quote filename))
  (unless (file-remote-p directory)
    (cl-callf @expand-and-quote directory))
  (let ((relative (file-relative-name filename directory))
        (absolute (abbreviate-file-name (file-name-unquote filename))))
    (if (< (length relative) (length absolute)) relative absolute)))

(defun @file-equal-p (file-1 file-2)
  "Return whether FILE-1 and FILE-2 are probably the same file.
This is more lenient than ‘file-equal-p’ because it also treats
exact copies as equal."
  (declare (ftype (function (string string) boolean)))
  (cl-check-type file-1 string)
  (cl-check-type file-2 string)
  (or (string-equal file-1 file-2)
      (file-equal-p file-1 file-2)
      (when-let* ((file-1 (file-truename file-1))
                  (file-2 (file-truename file-2))
                  (attributes-1 (file-attributes file-1))
                  (attributes-2 (file-attributes file-2)))
        (and (null (file-attribute-type attributes-1))
             (null (file-attribute-type attributes-2))
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


;;; Main code:

(unless noninteractive
  (error "This file works only in batch mode"))

(setq attempt-stack-overflow-recovery nil
      attempt-orderly-shutdown-on-fatal-signal nil
      edebug-initial-mode 'Go-nonstop)  ; ‘step’ doesn’t work in batch mode

;; We perform our own coverage instrumentation.
(push '(@coverage @edebug-enter @edebug-before @edebug-after)
      edebug-behavior-alist)

;; TEST_SRCDIR and TEST_TMPDIR are required,
;; cf. https://bazel.build/reference/test-encyclopedia#initial-conditions.
(unless (@getenv "TEST_SRCDIR")
  (error "TEST_SRCDIR not set"))

(let ((temp-dir (@env-file "TEST_TMPDIR")))
  (unless temp-dir
    (error "TEST_TMPDIR not set"))
  (setq temporary-file-directory (file-name-as-directory temp-dir)))

(declare-function elisp/runfiles/file-handler "elisp/runfiles/runfiles"
                  (operation &rest args))

;; We could get the repository name from the TEST_WORKSPACE environment
;; variable, but that one’s optional
;; (cf. https://bazel.build/reference/test-encyclopedia#initial-conditions).
(let* ((repository-name (file-name-nondirectory
                         (directory-file-name default-directory)))
       (runfiles-handler-installed
        (rassq #'elisp/runfiles/file-handler file-name-handler-alist))
       ;; If the runfiles filename handler is installed, use that.  It’s more
       ;; correct and should also work on Windows.
       (resource-root (file-name-as-directory
                       (if runfiles-handler-installed
                           (concat "/bazel-runfile:" repository-name)
                         default-directory))))
  ;; Best-effort support for ‘ert-resource-directory’ and ‘ert-resource-file’.
  ;; The directory returned by ‘ert-resource-directory’ will typically be in the
  ;; execution root and no longer be valid when the test runs.  Therefore, strip
  ;; out everything up to the repository directory in the execution root
  ;; (cf. https://bazel.build/remote/output-directories#layout-diagram), and
  ;; replace it with the default directory.  Robust tests should use the
  ;; ‘elisp/runfiles/runfiles’ library to find their data files.
  (setq ert-resource-directory-trim-left-regexp
        (rx (* nonl) ?/ (literal repository-name) ?/)
        ert-resource-directory-format
        (concat (replace-regexp-in-string (rx ?%) "%%" resource-root
                                          :fixedcase :literal)
                "%s-resources/")))

(random (or (@getenv "TEST_RANDOM_SEED") ""))

(when-let ((shard-status-file (@env-file "TEST_SHARD_STATUS_FILE")))
  (let ((coding-system-for-write 'no-conversion)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region "" nil shard-status-file :append)))

(let ((fail-fast (equal (getenv "TESTBRIDGE_TEST_RUNNER_FAIL_FAST") "1"))
      (coverage-enabled (equal (getenv "COVERAGE") "1"))
      (coverage-manifest (@env-file "COVERAGE_MANIFEST"))
      (coverage-dir (@env-file "COVERAGE_DIR"))
      (verbose-coverage (when (@getenv "VERBOSE_COVERAGE") t))
      (load-buffers ())
      (failure-messages (make-hash-table :test #'eq))
      (start-time (current-time))
      test-sources skip-tests skip-tags selector tests reporter exit-code)

  ;; Parse command-line options.
  (cl-flet ((unquote (if (memq system-type '(ms-dos windows-nt cygwin))
                         (lambda (argument)
                           (decode-coding-string
                            (url-unhex-string argument :allow-newlines)
                            'utf-8-unix))
                       #'identity)))
    (let ((continue t))
      (while (and continue command-line-args-left)
        (pcase (pop command-line-args-left)
          ("--" (setq continue nil))
          ((rx bos "--test-source=" (let file (+ anything)) eos)
           (push (concat "/:" (unquote file)) test-sources))
          ((rx bos "--skip-test=" (let test (+ anything)) eos)
           (push (intern (unquote test)) skip-tests))
          ((rx bos "--skip-tag=" (let tag (+ anything)) eos)
           (push (intern (unquote tag)) skip-tags))
          (unknown (error "Unknown command-line switch %s" unknown)))))
    (cl-callf nreverse test-sources)
    (cl-callf nreverse skip-tests)
    (cl-callf nreverse skip-tags)
    (when coverage-enabled
      (push :nocover skip-tags))
    (cl-callf2 mapcar #'unquote command-line-args-left))

  ;; Build up the test selector.  We optimize the test selector somewhat.  It’s
  ;; displayed to the user if no test matches, and then we’d like to avoid empty
  ;; branches such as ‘(and)’.
  (let* ((test-filter (@getenv "TESTBRIDGE_TEST_ONLY"))
         (filters (list (if test-filter (read test-filter) t)
                        (pcase (mapcar (lambda (tag) `(tag ,tag)) skip-tags)
                          ('() t)
                          (`(,elt) `(not ,elt))
                          (elts `(not (or ,@elts))))
                        (pcase skip-tests
                          ('() t)
                          (`(,elt) `(not ,elt))
                          (elts `(not (member ,@elts)))))))
    (setq selector (pcase (delq t filters)
                     ('() t)
                     (`(,elt) elt)
                     (elts `(and ,@elts)))))

  ;; Set up coverage if desired.
  (when coverage-enabled
    (unless coverage-manifest
      (error "Coverage requested but COVERAGE_MANIFEST not set"))
    (unless coverage-dir
      (error "Coverage requested but COVERAGE_DIR not set"))
    (when verbose-coverage
      (message "Reading coverage manifest %s" coverage-manifest))
    (let ((format-alist nil)
          (after-insert-file-functions nil)
          ;; See
          ;; https://github.com/bazelbuild/bazel/issues/374#issuecomment-2594713891.
          (coding-system-for-read 'utf-8-unix)
          (instrumented-files ()))
      (with-temp-buffer
        (insert-file-contents coverage-manifest)
        (while (not (eobp))
          ;; The filenames in the coverage manifest are typically relative to
          ;; the current directory, so expand them here.
          (let ((file-name-handler-alist ()))
            (push (@expand-and-quote
                   (buffer-substring-no-properties (point) (line-end-position)))
                  instrumented-files))
          (forward-line)))
      (when verbose-coverage
        (message "Found %d files in coverage manifest"
                 (length instrumented-files)))
      ;; We don’t bother removing the advises since we are going to kill Emacs
      ;; anyway.
      (add-function
       :before-until load-source-file-function
       (lambda (fullname file _noerror _nomessage)
         ;; If we got a magic filename that tells us to instrument a file, then
         ;; instrument the corresponding source file if that exists.  See the
         ;; commentary in //elisp/private:binary.bzl for details.  In all other
         ;; cases, we defer to the normal ‘load-source-file-function’, which is
         ;; also responsible for raising errors if desired.
         (when (string-suffix-p ".el.instrument" fullname)
           (cl-callf2 string-remove-suffix ".instrument" fullname)
           (cl-callf2 string-remove-suffix ".instrument" file)
           (when (and (file-readable-p fullname)
                      ;; We still need to check whether Bazel wants us to
                      ;; instrument the file.
                      (cl-find fullname instrumented-files
                               :test #'@file-equal-p))
             (push (@load-instrument fullname file) load-buffers)
             t))))
      ;; Work around another Edebug specification issue fixed with Emacs commit
      ;; c799ad42f705f64975771e181dee29e1d0ebe97a.
      (when (eql emacs-major-version 29)
        (with-eval-after-load 'cl-macs
          (put #'cl-define-compiler-macro 'edebug-form-spec
               '(&define [&name symbolp "@cl-compiler-macro"]
                         cl-macro-list
                         cl-declarations-or-string def-body))))))

  ;; Load test source files.  If coverage is enabled, check for a file with a
  ;; well-known extension first.  The Bazel runfiles machinery is expected to
  ;; generate these files for source files that should be instrumented.  See the
  ;; commentary in //elisp/private:binary.bzl for details.
  (let ((load-suffixes (if coverage-enabled
                           (cons ".el.instrument" load-suffixes)
                         load-suffixes)))
    (mapc #'load test-sources))

  ;; The test sources should have processed any remaining command-line
  ;; arguments.
  (when-let ((args command-line-args-left))
    (error "Unprocessed command-line arguments: %S" args))
  (setq load-file-name nil)     ; hide ourselves from ‘macroexp-warn-and-return’

  ;; We select the tests now.  ‘ert-run-tests’ could also do it, but we need the
  ;; list of tests for sharding and reporting below.
  (setq tests (ert-select-tests selector t))
  (unless tests
    (error "Selector %S doesn’t match any tests" selector))

  ;; Take test sharding into account if desired.
  (cl-flet ((env-int (name)
              (when-let ((value (getenv name)))
                (cl-parse-integer value))))
    (let ((shard-count (or (env-int "TEST_TOTAL_SHARDS") 1))
          (shard-index (or (env-int "TEST_SHARD_INDEX") 0)))
      (unless (and (natnump shard-count) (natnump shard-index)
                   (< shard-index shard-count))
        (error "Invalid SHARD_COUNT (%s) or SHARD_INDEX (%s)"
               shard-count shard-index))
      (when (> shard-count 1)
        (setq tests (cl-loop for test in tests
                             and i from 0
                             when (eql (mod i shard-count) shard-index)
                             collect test))
        (unless tests
          (message "Empty shard with index %d" shard-index)))))

  ;; Actually run the tests.  Use a block to support fail-fast behavior.
  (cl-block nil
    (ert-run-tests
     `(member ,@tests)
     (lambda (&rest args)
       (pcase-exhaustive args
         (`(run-started ,stats)
          (let ((total (ert-stats-total stats)))
            (setq reporter (make-progress-reporter
                            (format-message "Running %d tests..." total)
                            0 total))))
         (`(test-started ,_stats ,test)
          (message "Running test %s" (ert-test-name test)))
         (`(test-ended ,stats ,test ,result)
          (let* ((name (ert-test-name test))
                 (duration (ert-test-result-duration result))
                 (expected (ert-test-result-expected-p test result))
                 (status (ert-string-for-test-result result expected)))
            (message "Test %s %s and took %d ms" name status
                     (* duration 1000))
            (unless expected
              ;; Print a nice error message that should point back to the source
              ;; file in a compilation buffer.
              (when-let ((prefix (@message-prefix name)))
                (message "%s: Test %s %s" prefix name status)))
            (when (ert-test-result-with-condition-p result)
              (let ((message (@failure-message name result)))
                (message "%s" message)
                ;; @failure-message is potentially slow, cache its results.
                (puthash test message failure-messages)))
            (progress-reporter-update reporter (ert-stats-completed stats))
            (and fail-fast (not expected) (cl-return))))
         (`(run-ended ,stats ,_abortedp)
          (let ((completed (ert-stats-completed stats))
                (unexpected (ert-stats-completed-unexpected stats)))
            (message "Running %d tests finished, %d results unexpected"
                     completed unexpected)
            (setq exit-code (min unexpected 1)))
          (when-let ((report-file (@env-file "XML_OUTPUT_FILE")))
            (@write-report report-file start-time tests stats failure-messages))
          (when coverage-enabled
            (when verbose-coverage
              (message "Writing coverage report into directory %s"
                       coverage-dir))
            (@write-coverage-report coverage-dir load-buffers))
          (progress-reporter-done reporter))))))

  ;; Report test status back to the Bazel test runner.
  (kill-emacs exit-code))

;; Local Variables:
;; read-symbol-shorthands: (("@" . "elisp/private/tools/run-test--"))
;; End:

;;; run-test.el ends here
