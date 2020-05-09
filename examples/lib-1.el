;;; lib-1.el --- example library 1 -*- lexical-binding: t; -*-

;; Copyright 2020 Google LLC
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

;; Example library.

;;; Code:

(require 'lib-2)
(require 'lib-4)

(require 'elisp/runfiles/runfiles)

(defun lib-1-func ()
  (lib-2-func)
  (lib-4-func)
  (message "hi from lib-1"))

(defun lib-1-data-dep ()
  ;; Use runfiles library to access data dependencies.
  (with-temp-buffer
    (insert-file-contents
     (elisp/runfiles/rlocation "phst_rules_elisp/examples/data.txt"))
    (message "%s" (buffer-string))))

(provide 'examples/lib-1)
;;; lib-1.el ends here
