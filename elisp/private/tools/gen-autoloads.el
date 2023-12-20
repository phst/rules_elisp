;;; gen-autoloads.el --- generate autoloads file     -*- lexical-binding: t; -*-

;; Copyright 2021 Google LLC
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

;; Generate an autoloads file.
;;
;; Usage:
;;
;;   emacs --quick --batch --load=gen-autoloads.el \
;;     --funcall=elisp/gen-autoloads-and-exit DEST PKGNAME SOURCE...
;;
;; Generates an autoloads file DEST for the SOURCE Emacs Lisp files.
;;
;; Exits with a zero status only if successful.

;;; Code:

(require 'cl-lib)
(require 'nadvice)
(require 'package)

(defun elisp/gen-autoloads-and-exit ()
  "Generate an autoloads file and exit Emacs.
See the file commentary for details."
  (unless noninteractive
    (error "This function works only in batch mode"))

  (add-to-list 'ignored-local-variables 'generated-autoload-file)

  (pcase command-line-args-left
    (`(,out ,pkgname . ,srcs)
     (let* ((workdir (file-name-as-directory (make-temp-file "workdir" :dir)))
            ;; Leaving these enabled leads to undefined behavior and doesn’t make
            ;; sense in batch mode.
            (attempt-stack-overflow-recovery nil)
            (attempt-orderly-shutdown-on-fatal-signal nil)
            (create-lockfiles nil))
       (dolist (f srcs)
         (copy-file f workdir))
       (package-generate-autoloads pkgname workdir)
       (copy-file
        (expand-file-name (concat workdir (format "%s-autoloads.el" pkgname))
                          workdir)
        out t)
       (kill-emacs 0)))
    (_ (error "Usage: emacs elisp/gen-autoloads.el DEST PKGNAME SOURCE..."))))

(provide 'elisp/gen-autoloads)
;;; gen-autoloads.el ends here
