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

(require 'cl-lib)
(require 'ert)
(require 'json)
(require 'pp)

(require 'elisp/runfiles/runfiles)

(defun elisp/ert/run-batch-and-exit ()
  "Run ERT tests in batch mode.
This is similar to ‘ert-run-tests-batch-and-exit’, but uses the
TESTBRIDGE_TEST_ONLY environmental variable as test selector.
Treat all remaining command-line arguments as names of test
source files and load them."
  (or noninteractive (error "This function works only in batch mode"))
  (setq attempt-stack-overflow-recovery nil
        attempt-orderly-shutdown-on-fatal-signal nil)
  (mapc #'load command-line-args-left)
  (let* ((print-escape-newlines t)
         (pp-escape-newlines t)
         (print-circle t)
         (print-gensym t)
         (print-level 8)
         (print-length 50)
         (test-filter (getenv "TESTBRIDGE_TEST_ONLY"))
         (random-seed (or (getenv "TEST_RANDOM_SEED") ""))
         (xml-output-file (getenv "XML_OUTPUT_FILE"))
         (selector (if (member test-filter '(nil "")) t (read test-filter)))
         (tests (ert-select-tests selector t))
         (unexpected 0)
         (report `((start-time . ,(format-time-string "%FT%T.%9NZ" nil t))))
         (test-reports ())
         (start-time (current-time)))
    (or tests (error "Selector %S doesn’t match any tests" selector))
    (random random-seed)
    (elisp/ert/log--message "Running %d tests" (length tests))
    (dolist (test tests)
      (elisp/ert/log--message "Running test %s" (ert-test-name test))
      (let* ((name (ert-test-name test))
             (start-time (current-time))
             (result (ert-run-test test))
             (duration (time-subtract nil start-time))
             (expected (ert-test-result-expected-p test result))
             (status (ert-string-for-test-result result expected))
             (report `((name . ,(symbol-name name))
                       (elapsed . ,(float-time duration))
                       (status . ,status)
                       (expected . ,(if expected :json-true :json-false)))))
        (elisp/ert/log--message "Test %s %s and took %d ms" name status
                                (* (float-time duration) 1000))
        (or expected (cl-incf unexpected))
        (when (ert-test-result-with-condition-p result)
          (with-temp-buffer
            (debugger-insert-backtrace
             (ert-test-result-with-condition-backtrace result) nil)
            (goto-char (point-min))
            (while (not (eobp))
              (message "    %s"
                       (buffer-substring-no-properties
                        (point) (min (line-end-position) (+ 120 (point)))))
              (forward-line))
            (goto-char (point-min))
            (insert (format-message "  Test %s backtrace:\n" name))
            (goto-char (point-max))
            (dolist (info (ert-test-result-with-condition-infos result))
              (insert "  " (car info) (cdr info) ?\n))
            (insert (format-message "  Test %s condition:\n" name))
            (insert "    ")
            (pp (ert-test-result-with-condition-condition result)
                (current-buffer))
            (insert ?\n)
            (let ((message (buffer-substring-no-properties (point-min)
                                                           (point-max))))
              (message "%s" message)
              (push `(message . ,message) report))))
        (push report test-reports)))
    (push `(elapsed . ,(float-time (time-subtract nil start-time))) report)
    (push `(tests . ,(nreverse test-reports)) report)
    (elisp/ert/log--message "Running %d tests finished, %d results unexpected"
                            (length tests) unexpected)
    (unless (member xml-output-file '(nil ""))
      ;; Rather than trying to write a well-formed XML file in Emacs Lisp,
      ;; write the report as a JSON file and let an external binary deal with
      ;; the conversion to XML.
      (let ((process-environment
             (append (elisp/runfiles/env-vars) process-environment))
            (converter (elisp/runfiles/rlocation
                        "phst_rules_elisp/elisp/ert/write_xml_report"))
            (json-file (make-temp-file "ert-report-" nil ".json"
                                       (json-encode (nreverse report)))))
        (unwind-protect
            (with-temp-buffer
              (unless (eq 0 (call-process (file-name-unquote converter) nil t
                                          nil "--" json-file xml-output-file))
                (message "%s" (buffer-string))
                (error "Writing XML output file %s failed" xml-output-file)))
          (delete-file json-file))))
    (kill-emacs (min unexpected 1))))

(defun elisp/ert/log--message (format &rest args)
  "Like ‘(message FORMAT ARGS…)’, but also print a timestamp."
  (message "[%s] %s"
           (format-time-string "%F %T.%3N")
           (apply #'format-message format args)))

(provide 'elisp/ert/runner)
;;; runner.el ends here
