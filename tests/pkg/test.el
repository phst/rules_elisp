;;; test.el --- test to check the test runner        -*- lexical-binding: t; -*-

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

;; An ERT test that ensures that we can load files from other packages.

;;; Code:

(require 'tests/test-lib)

(require 'ert)

(ert-deftest test ()
  (tests/test-function nil))

;;; test.el ends here
