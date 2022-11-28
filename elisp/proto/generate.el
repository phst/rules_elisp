;;; generate.el --- generate protocol buffer bindings -*- lexical-binding: t; -*-

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

;; Generates Emacs Lisp bindings for a protocol buffer definition.
;;
;; Usage:
;;
;;   elisp/proto/generate DESCRIPTOR-FILE OUTPUT-FILE TARGET FEATURE \
;;     DEPENDENCIES...

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'subr-x)

(require 'elisp/proto/proto)

(defun elisp/proto/simple-string-p (o)
  "Return whether O is a string that’s unlikely to cause trouble."
  (declare (side-effect-free t))
  (and (stringp o)
       (string-match-p (rx bos (+ (any blank alnum ?_ ?- ?. ?/ ?: ?@)) eos) o)))

(defun elisp/proto/generate-message (full-name fields)
  "Generate code for the protocol buffer message type FULL-NAME.
FIELDS is a list of field names."
  (cl-check-type full-name string)
  (cl-check-type fields list)
  (let* ((print-escape-newlines nil)
         (print-escape-control-characters nil)
         (pp-escape-newlines nil)
         (struct (intern (cl-substitute ?/ ?. full-name)))
         (struct-doc
          (concat
           (format "Wrapper for protocol buffer messages of type ‘%s’.\n"
                   full-name)
           (and fields
                (format "Fields: %s.\n"
                        (mapconcat (lambda (field) (format "‘%s’" field))
                                   fields ", ")))
           "The structure fields are internal and should not be accessed
directly."))
         (conc-name (intern (format "%s--" struct)))
         (private-constructor (intern (format "%s--new" struct)))
         (public-constructor (intern (format "%s-new" struct)))
         (public-constructor-doc
          (concat
           (format "Create a new mutable protocol buffer message of type ‘%s’.
This type corresponds to the protocol buffer message type ‘%s’."
                   struct full-name)
           (and fields
                (concat "\nYou can pass keyword-value pairs "
                        "to initialize the fields.\n\n"
                        (prin1-to-string `(fn &key ,@fields)))))))
    (pp `(cl-defstruct (,struct
                        (:conc-name ,conc-name)
                        (:constructor ,private-constructor (arena ptr))
                        (:include elisp/proto/message)
                        :noinline)
           ,struct-doc))
    (terpri)
    (pp `(defun ,public-constructor (,@(and fields '(&rest fields)))
           ,public-constructor-doc
           (declare (side-effect-free t))
           ,(if fields
                `(apply #'elisp/proto/make ',struct fields)
              `(elisp/proto/make ',struct))))
    (terpri)
    (when fields
      (pp `(cl-define-compiler-macro ,public-constructor
               (&whole form &rest keys)
             (elisp/proto/check--keys form ',struct keys)))
      (terpri))))

(defun elisp/proto/generate-enum (full-name values)
  "Generate code for the protocol buffer enumeration type FULL-NAME.
VALUES is a list of (NAME NUMBER) pairs."
  (cl-check-type full-name string)
  (cl-check-type values list)
  (let* ((parent (string-trim-right full-name (rx ?. (+ (not (any ?.))))))
         (prefix (concat (cl-substitute ?/ ?. parent) "/")))
    (pcase-dolist (`(,name ,number) values)
      (let ((symbol (intern (concat prefix (symbol-name name))))
            (doc (format "Enumerator ‘%s’ of enumeration ‘%s’."
                         name full-name)))
        (prin1 `(defconst ,symbol ,number ,doc))
        (terpri))))
  (terpri))

(pcase command-line-args-left
  (`(,descriptor-file ,output-file ,target ,feature . ,dependencies)
   (setq command-line-args-left nil)
   (cl-check-type target elisp/proto/simple-string)
   (cl-check-type feature elisp/proto/simple-string)
   (let* ((coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix)
          (output-name (file-name-nondirectory output-file))
          (serialized-file-descriptor-set
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally descriptor-file)
             (buffer-substring-no-properties (point-min) (point-max))))
          (parsed (elisp/proto/parse-file-descriptor-set
                   serialized-file-descriptor-set)))
     (cl-check-type output-name elisp/proto/simple-string)
     (cl-destructuring-bind (files messages enums) parsed
       (with-temp-file (concat "/:" output-file)
         (let ((standard-output (current-buffer))
               (print-level nil)
               (print-length nil)
               (print-circle t)
               (print-gensym t)
               (print-escape-control-characters t)
               (print-escape-newlines t)
               (print-escape-nonascii t)
               (pp-escape-newlines t))
           (insert ";;; " output-name " --- protocol buffer library " target
                   " -*- lexical-binding: t; -*-\n\n"
                   ";;; Commentary:\n\n"
                   ";; A generated protocol buffer library.\n"
                   ";; This library corresponds to the the following "
                   "Bazel target:\n"
                   ";;   " target "\n"
                   ";; This file was generated from the following files:\n")
           (dolist (file files)
             (cl-check-type file elisp/proto/simple-string)
             (insert ";;   " file "\n"))
           (insert "\n;;; Code:\n\n")
           (prin1 '(require 'cl-lib)) (terpri) (terpri)
           (prin1 '(require 'elisp/proto/proto)) (terpri) (terpri)
           (dolist (dep dependencies)
             (cl-check-type dep elisp/proto/simple-string)
             (prin1 `(require ',(intern dep))) (terpri))
           (when dependencies (terpri))
           (prin1 `(elisp/proto/register-file-descriptor-set
                    ,serialized-file-descriptor-set))
           (terpri) (terpri)
           (dolist (message messages)
             (cl-destructuring-bind (full-name . fields) message
               (elisp/proto/generate-message full-name fields)))
           (dolist (enum enums)
             (cl-destructuring-bind (full-name . values) enum
               (elisp/proto/generate-enum full-name values)))
           (prin1 `(provide ',(intern feature))) (terpri) (terpri)
           (insert ";; Local Variables:\n"
                   ;; Generated docstrings can be overly long if they contain
                   ;; lengthy message names.  Don’t issue byte-compiler warnings
                   ;; for them.
                   ";; byte-compile-docstring-max-column: 500\n"
                   ";; End:\n\n"
                   ";;; " output-name " ends here\n"))))))
  (_ (user-error (concat "Usage: elisp/proto/generate "
                         "DESCRIPTOR-FILE OUTPUT-FILE TARGET FEATURE "
                         "DEPENDENCIES..."))))

;; Page delimiter so that Emacs doesn’t get confused by the strings above.  See
;; Info node ‘(emacs) Specifying File Variables’.


;;; generate.el ends here
