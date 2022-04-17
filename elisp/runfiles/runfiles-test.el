;;; runfiles-test.el --- unit test for runfiles.el  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022 Google LLC
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
    (should (cl-typep runfiles 'elisp/runfiles/runfiles))
    (should (file-exists-p filename))
    (should (file-readable-p filename))
    (should (file-regular-p filename))
    (should (> (file-attribute-size (file-attributes filename)) 0))
    (should (or (getenv "RUNFILES_DIR") (getenv "RUNFILES_MANIFEST_FILE")))))

(ert-deftest elisp/runfiles/special-chars/directory ()
  (let ((directory (make-temp-file "runfiles-test-" :directory)))
    (unwind-protect
        (let ((filename (expand-file-name "testäα𝐴🐈'.txt" directory))
              (runfiles (elisp/runfiles/make :manifest "/invalid.manifest"
                                             :directory directory)))
          (write-region "contents\n" nil filename nil nil nil 'excl)
          (should (equal (elisp/runfiles/rlocation "testäα𝐴🐈'.txt" runfiles)
                         filename)))
      (delete-directory directory :recursive))))

(ert-deftest elisp/runfiles/special-chars/manifest ()
  (let* ((manifest (elisp/runfiles/rlocation
                    "phst_rules_elisp/elisp/runfiles/test-manifest"))
         (runfiles (elisp/runfiles/make :manifest manifest
                                        :directory "/invalid/")))
    (should (equal (elisp/runfiles/rlocation "testäα𝐴🐈'.txt" runfiles)
                   "/:/runfiles/testäα𝐴🐈'.txt"))))

(ert-deftest elisp/runfiles/make/empty-file ()
  (let* ((manifest (elisp/runfiles/rlocation
                    "phst_rules_elisp/elisp/runfiles/test-manifest"))
         (runfiles (elisp/runfiles/make :manifest manifest
                                        :directory "/invalid/")))
    (should-error (elisp/runfiles/rlocation "__init__.py" runfiles)
                  :type 'elisp/runfiles/empty)))

(ert-deftest elisp/runfiles/make/manifest/directory ()
  "Check that we find runfiles in a mapped directory.
See https://github.com/bazelbuild/bazel/issues/14336 for
context."
  (let* ((manifest (elisp/runfiles/rlocation
                    "phst_rules_elisp/elisp/runfiles/test-manifest"))
         (runfiles (elisp/runfiles/make :manifest manifest
                                        :directory "/invalid/")))
    (should (equal (elisp/runfiles/rlocation "foo/bar/baz" runfiles)
                   "/:runfiles/foo/bar/baz"))))

(ert-deftest elisp/runfiles/file-handler ()
  (let ((file-name-handler-alist file-name-handler-alist))
    (elisp/runfiles/install-handler)
    (should
     (file-exists-p "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"))
    (should (file-readable-p
             "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"))
    (should
     (natnump
      (file-modes "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt")))
    (access-file "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"
                 "elisp/runfiles/file-handler")
    (should (equal (expand-file-name
                    "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt")
                   "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"))
    (should (equal (expand-file-name
                    "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"
                    "/foobar")
                   "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"))
    (should (equal (expand-file-name "runfiles/test.txt"
                                     "/bazel-runfile:phst_rules_elisp/elisp/")
                   "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"))
    (should (equal (expand-file-name "runfiles/test.txt"
                                     "/bazel-runfile:phst_rules_elisp/elisp")
                   "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"))
    (should (equal (file-relative-name
                    "/bazel-runfile:phst_rules_elisp/elisp/runfiles/test.txt"
                    "/bazel-runfile:phst_rules_elisp/elisp/")
                   "runfiles/test.txt"))
    (should (equal (file-truename "/bazel-runfile:phst_rules_elisp/elisp/")
                   "/bazel-runfile:phst_rules_elisp/elisp/"))
    (let ((load-path '("/bazel-runfile:phst_rules_elisp")))
      (require 'elisp/runfiles/test-lib))))

;;; runfiles-test.el ends here
