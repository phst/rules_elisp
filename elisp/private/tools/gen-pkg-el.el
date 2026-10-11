;;; gen-pkg-el.el --- generate -pkg.el file     -*- lexical-binding: t; -*-

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

;; Generate a -pkg.el file.
;;
;; Usage:
;;
;;   emacs --quick --batch --load=gen-pkg-el.el \
;;     --funcall=elisp/gen-pkg-el-and-exit SOURCE DEST
;;
;; Generates a -pkg.el file DEST, using the headers from the SOURCE Emacs Lisp
;; file.
;;
;; SOURCE should contain the headers as
;; described in Info node `(elisp)Simple Packages'.
;;
;; Exits with a zero status only if successful.

;;; Code:

(require 'package)
(require 'lisp-mnt)

(defun elisp/gen-pkg-el-and-exit ()
  "Generate a -pkg.el file and exit Emacs.
See the file commentary for details."
  (unless noninteractive
    (error "This function works only in batch mode"))
  (pcase command-line-args-left
    (`(,src ,out)
     (let* (
            ;; Leaving these enabled leads to undefined behavior and doesn’t make
            ;; sense in batch mode.
            (attempt-stack-overflow-recovery nil)
            (attempt-orderly-shutdown-on-fatal-signal nil)
            (pkginfo (with-temp-buffer
                       (insert-file-contents src)
                       (package-buffer-info))))
       (package-generate-description-file pkginfo out)
       (kill-emacs 0)))
    (_ (error "Usage: emacs elisp/gen-pkg-el.el SOURCE DEST"))))

(provide 'elisp/gen-pkg-el)
;;; gen-pkg-el.el ends here
