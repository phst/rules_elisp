;;; generate-bundle.el --- generate protocol buffer bundle -*- lexical-binding: t; -*-

;; Copyright 2021-2026 Google LLC
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

;; Gazelle-Type: binary

;;; Commentary:

;; Generates an overall Emacs Lisp library for a protocol buffer library.
;;
;; Usage:
;;
;;   gen-proto-bundle OUTPUT-FILE TARGET FEATURE DEPENDENCIES...

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun @simple-string-p (string)
  "Return non-nil if STRING is safe to put into the generated file."
  (declare (ftype (function (string) t)))
  (cl-check-type string string)
  (string-match-p (rx bos (+ (any blank alnum ?- "_+./:@~")) eos) string))

(cl-destructuring-bind
    (output-file target feature . dependencies)
    command-line-args-left
  (setq command-line-args-left nil)
  (cl-check-type target string)
  (cl-check-type feature string)
  (cl-check-type dependencies list)
  (unless (@simple-string-p target)
    (error "Invalid Bazel target label %s" target))
  (let ((file-name-handler-alist ())
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix)
        (output-name (file-name-nondirectory output-file)))
    (unless (@simple-string-p output-name)
      (error "Invalid output file name %s" output-name))
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
          (unless (@simple-string-p dep)
            (error "Invalid Emacs Lisp feature name %s" dep))
          (prin1 `(require ',(intern dep))) (terpri))
        (when dependencies (terpri))
        (prin1 `(provide ',(intern feature))) (terpri) (terpri)
        (insert ";;; " output-name " ends here\n")))))

;; Local Variables:
;; read-symbol-shorthands: (("@" . "elisp/private/tools/gen-proto-bundle--"))
;; End:

;;; gen-proto-bundle.el ends here
