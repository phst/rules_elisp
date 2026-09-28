;;; gen-metadata.el --- generate package info file     -*- lexical-binding: t; -*-

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

;; Generate a package metadata file.
;;
;; Usage:
;;
;;   emacs --quick --batch --load=gen-metadata.el \
;;     --funcall=elisp/gen-metadata-and-exit SOURCE DEST
;;
;; Generates a metadata file DEST using the SOURCE -pkg.el.
;; This is a JSON file that contains package metadata to allow easier extraction
;; for non-Emacs tools.
;;
;; Exits with a zero status only if successful.

;;; Code:

(require 'package)
(require 'json)

(defun elisp/gen-metadata-and-exit ()
  "Generate package metadata file and exit Emacs.
See the file commentary for details."
  (unless noninteractive
    (error "This function works only in batch mode"))
  (pcase command-line-args-left
    (`(,src ,out)
     (let* (
            ;; Leaving these enabled leads to undefined behavior and doesn’t
            ;; make sense in batch mode.
            (attempt-stack-overflow-recovery nil)
            (attempt-orderly-shutdown-on-fatal-signal nil)
            (metadata (with-temp-buffer
                        (insert-file-contents src)
                        (or (package-process-define-package
                             (read (current-buffer)))
                            (error "Can't find define-package in %s" src)))))
       (write-region
        (json-encode
         `((name . ,(package-desc-name metadata))
           (version . ,(package-version-join (package-desc-version metadata)))))
        nil out)
       (kill-emacs 0)))
    (_ (error "Usage: emacs elisp/gen-metadata.el SOURCE DEST"))))

(provide 'elisp/gen-metadata)
;;; gen-metadata.el ends here
