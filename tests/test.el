;;; test.el --- test to check the test runner        -*- lexical-binding: t; -*-

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

;; An ERT test that is used by ert_test.go.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tests/test-lib)

;; Ensure that command-line arguments are passed on correctly.
(cl-assert (equal command-line-args-left '("arg 1" "arg\n2 äα𝐴🐈'")) :show-args)
(defconst remaining-args (cl-copy-list command-line-args-left))
(setq command-line-args-left nil)

(ert-deftest pass ()
  (should (= 0 0)))

(ert-deftest fail ()
  (should (= 0 1)))

(ert-deftest skip ()
  (skip-unless (= 1 2))
  (should (= 0 1)))

(ert-deftest expect-failure ()
  :expected-result :failed
  (should (= 0 1)))

(ert-deftest expect-failure-but-pass ()
  :expected-result :failed
  (should (= 0 0)))

(ert-deftest filter ()
  :tags '(skip)
  (should (= 0 1)))

(ert-deftest filter-via-skip-tests-attribute-äα𝐴🐈 ()
  (should (= 0 1)))

(ert-deftest filter-via-skip-tags-attribute ()
  :tags (list (intern "skip-from-attribute \t\n\r\f äα𝐴🐈'\\\""))
  (should (= 0 1)))

(ert-deftest error ()
  (error "Boo"))

(ert-deftest abort ()
  ;; https://bugs.gnu.org/76447
  (skip-unless (not (string-equal emacs-version "30.1")))
  (signal 'undefined-error-symbol '("Boo")))

(ert-deftest throw ()
  (throw 'unknown-tag 'hi))

(ert-deftest special-chars ()
  (error "%s" (concat "Error äöü \t \r\n \0 \uFFFD \uFFFE \uFFFF 𝑨 "
                      "<![CDATA[ ]]> & < > \" ' <!-- -->")))

(ert-deftest coverage ()
  (tests/test-function nil))

(ert-deftest command-line ()
  (should (equal remaining-args '("arg 1" "arg\n2 äα𝐴🐈'"))))

(ert-deftest nocover ()
  :tags '(:nocover)
  (should (= 0 1)))

(ert-deftest ert-fail ()
  "This test validates a workaround for an ERT bug."
  (should (integerp (ert-fail "Fail!"))))

;;; test.el ends here
