;;; compile.el --- byte-compile Emacs Lisp files     -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022, 2023 Google LLC
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

;; Byte-compiles a single Emacs Lisp file.
;;
;; Usage:
;;
;;   emacs --quick --batch --load=compile.el SOURCE DEST
;;
;; Compiles the Emacs Lisp file SOURCE and stores the compiled output in the
;; file DEST.  Exits with a zero status only if compilation succeeds.

;;; Code:

(require 'bytecomp)
(require 'warnings)

(add-to-list 'command-switch-alist
             (cons "--fatal-warnings" #'elisp/fatal-warnings))

(defvar elisp/fatal--warnings nil
  "Whether byte compilation warnings should be treated as errors.
The --fatal-warnings option sets this variable.")

(defvar elisp/compilation--in-progress nil
  "Whether a byte compilation is currently running.
Used to detect recursive invocation of
‘elisp/compile-batch-and-exit’.")

(defun elisp/compile-batch-and-exit ()
  "Byte-compiles a single Emacs Lisp file and exits Emacs.
There must be exactly two remaining arguments on the command
line.  These are interpreted as source and output file,
respectively.  If compilation fails, exit with a nonzero exit
code.  If the command line option --fatal-warnings is given,
treat warnings as errors."
  (unless noninteractive
    (error "This function works only in batch mode"))
  (when elisp/compilation--in-progress
    (error "Recursive invocation of ‘elisp/compile-batch-and-exit’"))
  (pcase command-line-args-left
    (`(,src ,out)
     (setq command-line-args-left nil)
     ;; Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44481.
     (when (version< emacs-version "27.2")
       (define-advice system-name (:after-until ()) ""))
     (let* ((elisp/compilation--in-progress t)
            ;; Leaving these enabled leads to undefined behavior and doesn’t
            ;; make sense in batch mode.
            (attempt-stack-overflow-recovery nil)
            (attempt-orderly-shutdown-on-fatal-signal nil)
            ;; Ensure filenames in the output are relative to the current
            ;; directory.
            (byte-compile-root-dir default-directory)
            (warning-fill-column 1000)  ; Bug#52281
            ;; Write output to a temporary file (Bug#44631).
            (temp (make-temp-file "compile-" nil ".elc"))
            (byte-compile-dest-file-function (lambda (_) temp))
            (byte-compile-error-on-warn elisp/fatal--warnings)
            (success (byte-compile-file src)))
       (when success (copy-file temp out :overwrite))
       (delete-file temp)
       (kill-emacs (if success 0 1))))
    (_ (error "Usage: emacs elisp/compile.el SRC OUT"))))

(defun elisp/fatal-warnings (_arg)
  "Process the --fatal-warnings command-line option."
  (setq elisp/fatal--warnings t))

(provide 'elisp/compile)
;;; compile.el ends here
