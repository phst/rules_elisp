;;; merge.el --- merge generated documentation into combined manual  -*- lexical-binding: t; -*-

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

;; Usage: docs/merge OUTPUT.org MAIN.org INCLUDE-DIR

;;; Code:

(require 'org)
(require 'ox)

(unless noninteractive (user-error "This file works only in batch mode"))

(pcase command-line-args-left
  (`(,output ,main ,include-dir)
   (setq command-line-args-left nil)
   (let* ((coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix)
          (format-alist nil)
          (after-insert-file-functions nil)
          (write-region-annotate-functions nil)
          (write-region-post-annotation-function nil))
     (with-temp-buffer
       (insert-file-contents main :visit)
       (org-mode)
       (org-export-expand-include-keyword nil include-dir)
       (write-region nil nil output))))
  (_ (user-error "Usage: docs/merge OUTPUT.org MAIN.org INCLUDE-DIR")))

;;; merge.el ends here
