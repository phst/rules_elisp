;;; test-lib.el --- test to check the test runner -*- lexical-binding: t; -*-

;; Copyright 2020, 2021 Google LLC
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

;; Example functions to test coverage reporting.  This has to be in a separate
;; file because the test runner only instruments newly-loaded files.

;;; Code:

(require 'cl-lib)

(defun tests/test-function (arg)
  ;; The condition and the two branches should be on separate lines, for line
  ;; coverage testing.
  (if
      arg
      (message "Foo")
    (message "Bar"))
  (if
      (not arg)
      (message "Bar")
    (message "Foo"))
  ;; Multiple local functions with the same name should work.
  (cl-flet ((foo () 1))
    (message "%d" (foo)))
  (cl-flet ((foo () 2))
    (message "%d" (foo)))
  ;; More branch coverage examples.
  (cond
   (arg)
   ((or (eq arg 'foo) (and (not arg) 77) (memq arg '(1 2 3))) arg)
   ((not arg))
   ((and (not arg) 123) arg))
  (condition-case nil
      (message "%S" arg)
    (error arg))
  ;; Improper lists and vectors should also work.
  (message "%S %S %S" `(,arg . q) #1='(a . #1#) `[,arg q]))

(provide 'tests/test-lib)
;;; test-lib.el ends here
