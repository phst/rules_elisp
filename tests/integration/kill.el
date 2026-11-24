;;; process-signal.el --- kill the Emacs process with a signal  -*- lexical-binding: t; -*-

;; Copyright 2026 Philipp Stephani
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

;; Test program that kills the Emacs process with a signal.

;;; Code:

(eval-when-compile (require 'cl-lib))

(let ((attempt-orderly-shutdown-on-fatal-signal nil)
      (attempt-stack-overflow-recovery nil))
  (cl-destructuring-bind (name) command-line-args-left
    (setq command-line-args-left nil)
    (signal-process (emacs-pid) (intern name))))

;;; kill.el ends here
