;;; generate-bundle.el --- generate protocol buffer bundle -*- lexical-binding: t; -*-

;; Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

;; Generates an overall Emacs Lisp library for a protocol buffer library.
;;
;; Usage:
;;
;;   elisp/proto/generate-bundle OUTPUT-FILE TARGET FEATURE DEPENDENCIES...

;;; Code:

(eval-when-compile (require 'cl-lib))

(cl-deftype elisp/proto/simple-string ()
  '(and string
        (satisfies (lambda (string)
                     (string-match-p
                      (rx bos (+ (any blank alnum ?- "_+./:@~")) eos)
                      string)))))

(cl-destructuring-bind
    (output-file target feature . dependencies)
    command-line-args-left
  (setq command-line-args-left nil)
  (cl-check-type target elisp/proto/simple-string)
  (cl-check-type feature elisp/proto/simple-string)
  (let* ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (output-file (concat "/:" output-file))
         (output-name (file-name-nondirectory output-file)))
    (cl-check-type output-name elisp/proto/simple-string)
    (with-temp-file output-file
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle t)
            (print-gensym t)
            (print-escape-control-characters t)
            (print-escape-newlines t)
            (print-escape-nonascii t))
        (insert ";;; " output-name " --- protocol buffer library " target
                " -*- lexical-binding: t; -*-\n\n"
                ";;; Commentary:\n\n"
                ";; A generated protocol buffer library bundle.\n"
                ";; This library corresponds to the the following "
                "Bazel target:\n"
                ";;   " target "\n\n"
                ";;; Code:\n\n")
        (dolist (dep dependencies)
          (cl-check-type dep elisp/proto/simple-string)
          (prin1 `(require ',(intern dep))) (terpri))
        (when dependencies (terpri))
        (prin1 `(provide ',(intern feature))) (terpri) (terpri)
        (insert ";;; " output-name " ends here\n")))))

;;; generate-bundle.el ends here
