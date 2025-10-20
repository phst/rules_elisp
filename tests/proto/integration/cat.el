;;; cat.el --- helper binary for module test -*- lexical-binding: t; -*-

;; Copyright 2024, 2025 Philipp Stephani
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Test helper binary for //tests/proto:module_test.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'elisp/proto/module)

(set-binary-mode 'stdin :binary)
(set-binary-mode 'stdout :binary)

(cl-destructuring-bind (op file) command-line-args-left
  (setq command-line-args-left nil)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((file-name-handler-alist ())
          (coding-system-for-read 'no-conversion)
          (coding-system-for-write 'no-conversion)
          (inhibit-modification-hooks t)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
      (pcase-exhaustive op
        (">"
         (with-temp-file file
           (elisp/proto/insert-stdin)))
        ("<"
         (insert-file-contents-literally file)
         (elisp/proto/write-stdout
          (buffer-substring-no-properties (point-min) (point-max))))))))

;;; cat.el ends here
