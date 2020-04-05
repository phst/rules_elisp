;;; lib-1-test.el --- unit test for lib-1 -*- lexical-binding: t; -*-

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

;; Unit test for lib-1.el.

;;; Code:

(require 'examples/lib-1)

(require 'ert)

(ert-deftest lib-1-test ()
  (should (equal (lib-1-func) "hi from lib-1")))

;;; lib-1-test.el ends here
