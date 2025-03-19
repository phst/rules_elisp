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

(require 'elisp/proto/module)

(set-binary-mode 'stdin :binary)
(set-binary-mode 'stdout :binary)

(with-temp-buffer
  (set-buffer-multibyte nil)
  (let ((args command-line-args-left)
        (coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion)
        (inhibit-modification-hooks t)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (setq command-line-args-left nil)
    (pcase-exhaustive args
      (`(">" ,file)
       (elisp/proto/insert-stdin)
       (write-region nil nil (concat "/:" file)))
      (`("<" ,file)
       (insert-file-contents-literally (concat "/:" file))
       (elisp/proto/write-stdout
        (buffer-substring-no-properties (point-min) (point-max)))))))

;;; cat.el ends here
