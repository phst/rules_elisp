;;; merge.el --- merge generated documentation into combined manual  -*- lexical-binding: t; -*-

;; Copyright 2021, 2022 Google LLC
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

;; Usage: docs/merge OUTPUT.org MAIN.org INCLUDE.org…

;;; Code:

(require 'org)
(require 'ox)

(unless noninteractive (user-error "This file works only in batch mode"))

(pcase command-line-args-left
  (`(,output ,main . ,includes)
   (setq command-line-args-left nil)
   (let ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (temp-dir (file-name-as-directory (make-temp-file "merge" :dir))))
     (dolist (file includes)
       (copy-file file temp-dir))
     (with-temp-buffer
       (insert-file-contents main :visit)
       (org-mode)
       (org-export-expand-include-keyword nil temp-dir)
       (write-region nil nil output))
     (delete-directory temp-dir :recursive)))
  (_ (user-error "Usage: docs/merge OUTPUT.org MAIN.org INCLUDE.org…")))

;;; merge.el ends here
