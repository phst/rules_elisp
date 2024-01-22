;;; export-org.el --- export Org-mode file to Texinfo manual  -*- lexical-binding: t; -*-

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

;; Internal implementation of the ‘elisp_manual’ Bazel rule.
;; Usage:
;;    emacs export-org.el INPUT.org OUTPUT.texi

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-texinfo)
(require 'warnings)

(unless noninteractive (user-error "This file works only in batch mode"))

(pcase command-line-args-left
  (`(,input ,output)
   (setq command-line-args-left nil)
   (let ((warning-fill-column 1000)  ; https://debbugs.gnu.org/52281
         (coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (format-alist nil)
         (after-insert-file-functions nil)
         (write-region-annotate-functions nil)
         (write-region-post-annotation-function nil)
         (org-export-coding-system 'utf-8-unix)
         (org-export-time-stamp-file nil)
         (org-export-use-babel nil))
     (with-temp-buffer
       (insert-file-contents input)
       (write-region (org-export-as 'texinfo) nil output))))
  (_ (user-error "Usage: elisp/export-org INPUT OUTPUT")))

;;; export-org.el ends here
