;;; compile.el --- byte-compile Emacs Lisp files     -*- lexical-binding: t; -*-

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

;; Byte-compiles a single Emacs Lisp file.
;;
;; Usage:
;;
;;   emacs --quick --batch --load=compile.el FATAL-WARN CURRENT-REPO SOURCE DEST
;;
;; Compiles the Emacs Lisp file SOURCE and stores the compiled output in the
;; file DEST.  Exits with a zero status only if compilation succeeds.

;;; Code:

(require 'bytecomp)
(require 'warnings)

(defvar elisp/current-repository)

(unless noninteractive
  (error "This file works only in batch mode"))

;; Leaving these enabled leads to undefined behavior and doesn’t make sense in
;; batch mode.
(setq attempt-stack-overflow-recovery nil
      attempt-orderly-shutdown-on-fatal-signal nil)

;; Ensure filenames in the output are relative to the current directory.
(setq byte-compile-root-dir default-directory)

(cl-destructuring-bind (fatal-warn current-repo src out) command-line-args-left
  (setq command-line-args-left nil
        byte-compile-dest-file-function (lambda (_) out)
        byte-compile-error-on-warn (not (string-empty-p fatal-warn))
        elisp/current-repository current-repo)
  (kill-emacs (if (byte-compile-file src) 0 1)))

;;; compile.el ends here
