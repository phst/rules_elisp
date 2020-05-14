;;; lib-2-test.el --- unit test for lib-2 -*- lexical-binding: t; -*-

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

;; Unit test for lib-2.el.

;;; Code:

(require 'lib-2)

(require 'ert)

(ert-deftest lib-2-test ()
  (should (equal (lib-2-func) "hi from lib-2")))

(ert-deftest lib-2-broken-test ()
  :tags '(skip)
  ;; This test is broken, but Bazel will skip it due to the “skip_tags”
  ;; attribute in the build rule.
  (should (= 0 1)))

;;; lib-2-test.el ends here
