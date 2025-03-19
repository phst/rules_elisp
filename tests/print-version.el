;;; print-version.el --- print the Emacs version     -*- lexical-binding: t; -*-

;; Copyright 2025 Philipp Stephani
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

;; Print the Emacs version to the file specified on the command line.

;;; Code:

(eval-when-compile (require 'cl-lib))

(cl-destructuring-bind (file) command-line-args-left
  (setq command-line-args-left nil)
  (let ((file-name-handler-alist ()))
    (setq file (concat "/:" (expand-file-name file))))
  (let ((coding-system-for-write 'utf-8-unix)
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (write-region emacs-version nil file)))

;;; print-version.el ends here
