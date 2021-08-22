;;; export-org.el --- export Org-mode file to Texinfo manual  -*- lexical-binding: t; -*-

;; Copyright 2021 Google LLC
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

(unless noninteractive (user-error "This file works only in batch mode"))

(let ((input (pop command-line-args-left))
      (output (pop command-line-args-left))
      (coding-system-for-read 'utf-8-unix)
      (coding-system-for-write 'utf-8-unix))
  (when command-line-args-left (user-error "Too many command-line arguments"))
  (unless input (user-error "No input file given"))
  (unless output (user-error "No output file given"))
  (with-temp-buffer
    (insert-file-contents (file-name-quote input))
    (write-region (org-export-as 'texinfo) nil (file-name-quote output))))

;;; export-org.el ends here
