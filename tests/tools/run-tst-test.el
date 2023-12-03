;;; run-tst-test.el --- unit tests for run-tst.el -*- lexical-binding: t; -*-

;; Copyright 2023, 2025 Philipp Stephani
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

(ert-deftest environment ()
  "Test that the environment is set up as expected.
See URL
‘https://bazel.build/reference/test-encyclopedia#initial-conditions’."
  (should (equal (getenv "HOME") (getenv "TEST_TMPDIR")))
  (should-not (getenv "LANG"))
  (should-not (getenv "LANGUAGE"))
  (should-not (getenv "LC_ALL"))
  (should-not (getenv "LC_COLLATE"))
  (should-not (getenv "LC_CTYPE"))
  (should-not (getenv "LC_MESSAGES"))
  (should-not (getenv "LC_MONETARY"))
  (should-not (getenv "LC_NUMERIC"))
  (should-not (getenv "LC_TIME"))
  (should (equal (getenv "LOGNAME") (getenv "USER")))
  (should (equal (getenv "PATH") "/usr/bin:/usr/sbin:/bin:/sbin"))
  (should (equal (getenv "PWD") (expand-file-name (getenv "TEST_WORKSPACE")
                                                  (getenv "TEST_SRCDIR"))))
  (should (equal (getenv "SHLVL") "2"))
  (should-not (getenv "TEST_RANDOM_SEED"))
  (should-not (getenv "TEST_RUN_NUMBER"))
  (should (equal (getenv "TEST_TARGET") "//tests/tools:run_tst_test"))
  (should (equal (getenv "TEST_SIZE") "small"))
  ;; Bazel quintuples default test timeouts in coverage mode, so accept either
  ;; value.  On Windows, we quadruple the test timeouts, cf. //:.bazelrc.
  (should (member (getenv "TEST_TIMEOUT") '("60" "300" "240")))
  (should-not (getenv "TEST_SHARD_INDEX"))
  (should-not (getenv "TEST_SHARD_STATUS_FILE"))
  (should (getenv "TEST_SRCDIR"))
  (should (file-name-absolute-p (getenv "TEST_SRCDIR")))
  (should (file-readable-p (getenv "TEST_SRCDIR")))
  (should-not (getenv "TEST_TOTAL_SHARDS"))
  (should (getenv "TEST_TMPDIR"))
  (should (file-name-absolute-p (getenv "TEST_TMPDIR")))
  (should (file-readable-p (getenv "TEST_TMPDIR")))
  (should (file-writable-p (getenv "TEST_TMPDIR")))
  (should (equal (getenv "TEST_WORKSPACE") "_main"))
  (should-not (getenv "TESTBRIDGE_TEST_ONLY"))
  (should (equal (getenv "TZ") "UTC"))
  (should (getenv "USER"))
  (should (equal (getenv "USER") (user-login-name)))
  (should (getenv "XML_OUTPUT_FILE"))
  (should (getenv "BAZEL_TEST")))

;;; run-tst-test.el ends here
