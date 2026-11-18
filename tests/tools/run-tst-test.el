;;; run-tst-test.el --- unit tests for run-tst.el -*- lexical-binding: t; -*-

;; Copyright 2023, 2025, 2026 Philipp Stephani
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Unit tests for run-tst.el.

;;; Code:

;; Test that the environment is set up as expected.  See URL
;; ‘https://bazel.build/reference/test-encyclopedia#initial-conditions’.

(ert-deftest LANG ()
  (should-not (getenv "LANG")))

(ert-deftest LANGUAGE ()
  (should-not (getenv "LANGUAGE")))

(ert-deftest LC_ALL ()
  (should-not (getenv "LC_ALL")))

(ert-deftest LC_COLLATE ()
  (should-not (getenv "LC_COLLATE")))

(ert-deftest LC_CTYPE ()
  (should (member (getenv "LC_CTYPE") '(nil "C.UTF-8"))))

(ert-deftest LC_MESSAGE ()
  (should-not (getenv "LC_MESSAGES")))

(ert-deftest LC_MONETARY ()
  (should-not (getenv "LC_MONETARY")))

(ert-deftest LC_NUMERIC ()
  (should-not (getenv "LC_NUMERIC")))

(ert-deftest LC_TIME ()
  (should-not (getenv "LC_TIME")))

(ert-deftest TEST_SRCDIR ()
  (should (getenv "TEST_SRCDIR"))
  (should (file-name-absolute-p (getenv "TEST_SRCDIR")))
  (should (file-readable-p (getenv "TEST_SRCDIR"))))

(ert-deftest TEST_TMPDIR ()
  (should (getenv "TEST_TMPDIR"))
  (should (file-name-absolute-p (getenv "TEST_TMPDIR")))
  (should (file-readable-p (getenv "TEST_TMPDIR")))
  (should (file-writable-p (getenv "TEST_TMPDIR"))))

(ert-deftest TZ ()
  (should (equal (getenv "TZ") "UTC")))

(ert-deftest USER ()
  (should (getenv "USER"))
  (should (equal (getenv "USER") (user-login-name))))

(ert-deftest BAZEL_TEST ()
  (should (getenv "BAZEL_TEST")))

;;; run-tst-test.el ends here
