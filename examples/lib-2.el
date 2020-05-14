;;; lib-2.el --- example library 2                   -*- lexical-binding: t; -*-

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

(defun lib-2-func ()
  (message "hi from lib-2"))

;; This definition generates a byte-compile warning.  Normally this would cause
;; compilation to fail, but since the library rule use “fatal_warnings = False”
;; it only prints a warning message.
(defvar foo)

(provide 'examples/lib-2)
(provide 'lib-2)
;;; lib-2.el ends here
