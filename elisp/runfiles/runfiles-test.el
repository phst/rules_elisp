;;; runfiles-test.el --- unit test for runfiles.el  -*- lexical-binding: t; -*-

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

;; Unit tests for runfiles.el.

;;; Code:

(require 'elisp/runfiles/runfiles)

(require 'ert)

(ert-deftest elisp/runfiles/rlocation ()
  (let* ((runfiles (elisp/runfiles/make))
         (filename (elisp/runfiles/rlocation
                    "phst_rules_elisp/elisp/runfiles/test.txt" runfiles))
         (process-environment (elisp/runfiles/env-vars runfiles)))
    (should (object-of-class-p runfiles 'elisp/runfiles/runfiles))
    (should (file-exists-p filename))
    (should (file-readable-p filename))
    (should (file-regular-p filename))
    (should (> (file-attribute-size (file-attributes filename)) 0))
    (should (getenv "RUNFILES_DIR"))))

;;; runfiles-test.el ends here
