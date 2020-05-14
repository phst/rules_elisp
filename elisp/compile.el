;;; compile.el --- byte-compile Emacs Lisp files     -*- lexical-binding: t; -*-

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

(unless noninteractive
  (error "This file works only in batch mode"))

(let* ((src (pop command-line-args-left))
       (out (pop command-line-args-left))
       ;; Leaving these enabled leads to undefined behavior and doesn’t make
       ;; sense in batch mode.
       (attempt-stack-overflow-recovery nil)
       (attempt-orderly-shutdown-on-fatal-signal nil)
       ;; Ensure filenames in the output are relative to the current directory.
       (byte-compile-root-dir default-directory)
       (byte-compile-dest-file-function (lambda (_) out))
       (success (byte-compile-file src)))
  (kill-emacs (if success 0 1)))

;;; compile.el ends here
