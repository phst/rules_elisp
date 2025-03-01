;;; runfiles-test.el --- unit test for runfiles.el  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

(defvar tests/runfiles/rlocations
  (let ((table (make-hash-table :test #'equal)))
    (while command-line-args-left
      (pcase (pop command-line-args-left)
        ((rx bos (let key (+ (not ?=))) ?= (let value (+ anychar)) eos)
         (puthash key value table))
        (other (error "Unknown command-line argument %S" other))))
    table))

(ert-deftest elisp/runfiles/rlocation ()
  (let* ((runfiles (elisp/runfiles/make))
         (filename (elisp/runfiles/rlocation
                    (gethash "test.txt" tests/runfiles/rlocations)
                    runfiles))
         (process-environment (elisp/runfiles/env-vars runfiles)))
    (should (cl-typep runfiles 'elisp/runfiles/runfiles))
    (should (file-exists-p filename))
    (should (file-readable-p filename))
    (should (file-regular-p filename))
    (should (> (file-attribute-size (file-attributes filename)) 0))
    (should (or (getenv "RUNFILES_DIR") (getenv "RUNFILES_MANIFEST_FILE")))))

(ert-deftest elisp/runfiles/special-chars/directory ()
  (let* ((windows (memq system-type '(ms-dos windows-nt)))
         (directory (make-temp-file "runfiles-test-" :directory))
         (filename (concat "testäα𝐴🐈' " (unless windows "\t\n\\") ".txt"))
         (target (expand-file-name filename directory))
         (runfiles (elisp/runfiles/make :manifest "/invalid.manifest"
                                        :directory directory))
         (coding-system-for-write 'utf-8-unix)
         (write-region-annotate-functions nil)
         (write-region-post-annotation-function nil))
    (ert-info (filename :prefix "File name: ")
      (write-region "contents\n" nil target nil nil nil 'excl)
      (should (equal (elisp/runfiles/rlocation filename runfiles) target)))
    (delete-directory directory :recursive)))

(ert-deftest elisp/runfiles/special-chars/manifest ()
  (let* ((manifest (elisp/runfiles/rlocation
                    (gethash "test-manifest" tests/runfiles/rlocations)))
         (runfiles (elisp/runfiles/make :manifest manifest
                                        :directory "/invalid/")))
    (pcase-dolist (`(,source ,target)
                   '(("testäα𝐴🐈'.txt"
                      "/:/runfiles/testäα𝐴🐈'.txt")
                     ("target-with-space"
                      "/:/runfiles/with space\\and backslash")
                     ("target-with-newline"
                      "/:/runfiles/with\nnewline\\and backslash")
                     ("source with space,\nnewline,\\and backslash"
                      "/:/runfiles/with space,\nnewline,\\and backslash")))
      (ert-info (source :prefix "Source: ")
        (should (equal (elisp/runfiles/rlocation source runfiles) target))))))

(ert-deftest elisp/runfiles/make/empty-file ()
  (let* ((manifest (elisp/runfiles/rlocation
                    (gethash "test-manifest" tests/runfiles/rlocations)))
         (runfiles (elisp/runfiles/make :manifest manifest
                                        :directory "/invalid/")))
    (should-error (elisp/runfiles/rlocation "__init__.py" runfiles)
                  :type 'elisp/runfiles/empty)))

(ert-deftest elisp/runfiles/make/manifest/directory ()
  "Check that we find runfiles in a mapped directory.
See https://github.com/bazelbuild/bazel/issues/14336 for
context."
  (let* ((manifest (elisp/runfiles/rlocation
                    (gethash "test-manifest" tests/runfiles/rlocations)))
         (runfiles (elisp/runfiles/make :manifest manifest
                                        :directory "/invalid/")))
    (should (equal (elisp/runfiles/rlocation "foo/bar/baz" runfiles)
                   "/:runfiles/foo/bar/baz"))))

(ert-deftest elisp/runfiles/file-handler ()
  (let* ((file-name-handler-alist file-name-handler-alist)
         (rlocation (gethash "test.txt" tests/runfiles/rlocations))
         (repository (string-trim-right rlocation (rx ?/ (* anychar))))
         (virtual-repo-root (concat "/bazel-runfile:" repository))
         (virtual-file (concat "/bazel-runfile:" rlocation))
         (virtual-dir (concat virtual-repo-root "/tests/")))
    (elisp/runfiles/install-handler)
    (should (rassq #'elisp/runfiles/file-handler file-name-handler-alist))
    (should (eql (cl-count #'elisp/runfiles/file-handler file-name-handler-alist
                           :key #'cdr :test #'eq)
                 1))
    ;; Check that ‘elisp/runfiles/install-handler’ is idempotent.
    (elisp/runfiles/install-handler)
    (should (eql (cl-count #'elisp/runfiles/file-handler file-name-handler-alist
                           :key #'cdr :test #'eq)
                 1))
    (should (file-exists-p virtual-file))
    (should (file-readable-p virtual-file))
    (should (natnump (file-modes virtual-file)))
    (access-file virtual-file "elisp/runfiles/file-handler")
    (should (equal (expand-file-name virtual-file) virtual-file))
    (should (equal (expand-file-name virtual-file "/foobar") virtual-file))
    (should (equal (expand-file-name "runfiles/test.txt" virtual-dir)
                   virtual-file))
    (should (equal (expand-file-name "runfiles/test.txt"
                                     (concat virtual-repo-root "/tests"))
                   virtual-file))
    (should (equal (file-relative-name virtual-file virtual-dir)
                   "runfiles/test.txt"))
    (should (equal (file-truename virtual-dir) virtual-dir))
    (should (equal (abbreviate-file-name virtual-file) virtual-file))
    (let ((load-path (list virtual-repo-root)))
      (require 'tests/runfiles/test-lib))))

(ert-deftest elisp/runfiles/repo-mapping ()
  (let ((temp-dir (make-temp-file "elisp-test-" :directory ".runfiles")))
    (copy-file
     (elisp/runfiles/rlocation
      (gethash "test-mapping" tests/runfiles/rlocations))
     (expand-file-name "_repo_mapping" temp-dir))
    (let ((runfiles (elisp/runfiles/make :manifest "/invalid.manifest"
                                         :directory temp-dir)))
      (pcase-dolist (`(,file ,caller-repo ,want)
                     '(("main/file" nil "main/file")
                       ("main/file" "" "main/file")
                       ("main/file" "main" "main/file")
                       ("main/file" "external" "foobar/file")
                       ("external/file" nil "external/file")
                       ("external/file" "" "external/file")
                       ("external/file" "main" "external/file")
                       ("external/file" "external" "external/file")))
        (ert-info ((format "Looking up file %S from repository %S"
                           file caller-repo))
          (should (equal (elisp/runfiles/rlocation file runfiles
                                                   :caller-repo caller-repo)
                         (expand-file-name want temp-dir))))))
    (delete-directory temp-dir :recursive)))

;;; runfiles-test.el ends here
