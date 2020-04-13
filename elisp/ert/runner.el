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
(require 'pp)

(defun elisp/ert/run-batch-and-exit ()
  "Run ERT tests in batch mode.
This is similar to ‘ert-run-tests-batch-and-exit’, but uses the
TESTBRIDGE_TEST_ONLY environmental variable as test selector."
  (or noninteractive (error "This function works only in batch mode"))
  (let* ((attempt-stack-overflow-recovery nil)
         (attempt-orderly-shutdown-on-fatal-signal nil)
         (print-escape-newlines t)
         (pp-escape-newlines t)
         (print-circle t)
         (print-gensym t)
         (print-level 8)
         (print-length 50)
         (test-filter (getenv "TESTBRIDGE_TEST_ONLY"))
         (selector (if (member test-filter '(nil "")) t (read test-filter)))
         (tests (ert-select-tests selector t))
         (unexpected 0))
    (or tests (error "Selector %S doesn’t match any tests" selector))
    (elisp/ert/log--message "Running %d tests" (length tests))
    (dolist (test tests)
      (elisp/ert/log--message "Running test %s" (ert-test-name test))
      (let* ((name (ert-test-name test))
             (start-time (current-time))
             (result (ert-run-test test))
             (duration (time-subtract nil start-time))
             (expected (ert-test-result-expected-p test result)))
        (elisp/ert/log--message "Test %s %s and took %d ms" name
                                (ert-string-for-test-result result expected)
                                (* (float-time duration) 1000))
        (or expected (cl-incf unexpected))
        (when (ert-test-result-with-condition-p result)
          (message "  Test %s backtrace:" name)
          (with-temp-buffer
            (debugger-insert-backtrace
             (ert-test-result-with-condition-backtrace result) nil)
            (goto-char (point-min))
            (while (not (eobp))
              (message "    %s"
                       (buffer-substring-no-properties
                        (point) (min (line-end-position) (+ 120 (point)))))
              (forward-line)))
          (dolist (info (ert-test-result-with-condition-infos result))
            (message "  %s%s" (car info) (cdr info)))
          (message "  Test %s condition:\n    %s\n"
                   name
                   (pp-to-string
                    (ert-test-result-with-condition-condition result))))))
    (elisp/ert/log--message "Running %d tests finished, %d results unexpected"
                            (length tests) unexpected)
    (kill-emacs (min unexpected 1))))

(defun elisp/ert/log--message (format &rest args)
  "Like ‘(message FORMAT ARGS…)’, but also print a timestamp."
  (message "[%s] %s"
           (format-time-string "%F %T.%3N")
           (apply #'format-message format args)))

(provide 'elisp/ert/runner)
;;; runner.el ends here
