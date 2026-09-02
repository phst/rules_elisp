;;; exit.el --- exit Emacs with a given exit code -*- lexical-binding: t; -*-

;; Copyright 2026 Philipp Stephani
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

;;; Code:

(require 'cl-lib)

(unless noninteractive (user-error "This file works only in batch mode"))

(cl-destructuring-bind (string) command-line-args-left
  (setq command-line-args-left nil)
  (kill-emacs (cl-parse-integer string)))

;;; exit.el ends here
