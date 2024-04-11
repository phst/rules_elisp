;;; generate-bundle.el --- generate protocol buffer bundle -*- lexical-binding: t; -*-

;; Copyright 2021, 2022, 2023, 2024 Google LLC
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
;;   elisp/proto/generate-bundle DESCRIPTOR-FILE OUTPUT-FILE TARGET FEATURE

;;; Code:

(require 'cl-lib)

(require 'elisp/proto/proto)

(cl-deftype elisp/proto/simple-string ()
  '(and string
        (satisfies (lambda (string)
                     (string-match-p
                      (rx bos (+ (any blank alnum ?_ ?- ?. ?/ ?: ?@ ?~)) eos)
                      string)))))

(pcase command-line-args-left
  (`(,descriptor-file ,output-file ,target ,feature)
   (setq command-line-args-left nil)
   (cl-check-type target elisp/proto/simple-string)
   (cl-check-type feature elisp/proto/simple-string)
   (let* ((coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix)
          (serialized-file-descriptor-set
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally descriptor-file)
             (buffer-substring-no-properties (point-min) (point-max))))
          (parsed (elisp/proto/parse-file-descriptor-set
                   serialized-file-descriptor-set))
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
         (dolist (file parsed)
           (cl-destructuring-bind (proto-file _desc _deps _msgs _enums) file
             (cl-check-type proto-file elisp/proto/simple-string)
             (prin1 `(require ',(intern proto-file))) (terpri)))
         (when parsed (terpri))
         (prin1 `(provide ',(intern feature))) (terpri) (terpri)
         (insert ";;; " output-name " ends here\n")))))
  (_ (user-error (concat "Usage: elisp/proto/generate-bundle "
                         "DESCRIPTOR-FILE OUTPUT-FILE TARGET FEATURE"))))

;;; generate-bundle.el ends here
