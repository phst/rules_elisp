;;; gen-proto.el --- generate protocol buffer bindings -*- lexical-binding: t; -*-

;; Copyright 2021, 2022, 2023, 2024, 2025 Google LLC
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

;; Version: 0.1.0

;;; Commentary:

;; Generates Emacs Lisp bindings for a protocol buffer definition.
;;
;; Usage:
;;
;;   gen-proto DESCRIPTOR-FILE OUTPUT-FILE FEATURE

;;; Code:

(require 'cl-lib)
(require 'pp)
(require 'subr-x)

(require 'elisp/proto/proto)

(cl-deftype @simple-string ()
  '(and string
        (satisfies (lambda (string)
                     (string-match-p
                      (rx bos (+ (any blank alnum ?- "_+./:@~")) eos)
                      string)))))

(defun @generate-message (full-name fields)
  "Generate code for the protocol buffer message type FULL-NAME.
FIELDS is a list of field names."
  (declare (ftype (function (string list) t)))
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
                        (:constructor nil)
                        (:constructor ,private-constructor (arena ptr))
                        (:include elisp/proto/message)
                        :noinline)
           ,struct-doc))
    (terpri)
    (pp `(defun ,public-constructor (,@(and fields '(&rest fields)))
           ,public-constructor-doc
           (declare (ftype (function (&rest t) ,struct)) (side-effect-free t))
           ,(if fields
                `(apply #'elisp/proto/make ',struct fields)
              `(elisp/proto/make ',struct))))
    (terpri)
    (when fields
      (pp `(cl-define-compiler-macro ,public-constructor
               (&whole form &rest keys)
             (elisp/proto/proto--check-keys form ',struct keys)))
      (terpri))))

(defun @generate-enum (full-name values)
  "Generate code for the protocol buffer enumeration type FULL-NAME.
VALUES is a list of (NAME NUMBER) pairs."
  (declare (ftype (function (string list) t)))
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

(cl-defun @generate-file ((proto-file descriptor deps messages enums))
  "Generate code for a single protocol buffer definition file.
PROTO-FILE is the name of the protocol buffer definition file, relative
to its repository root.  DESCRIPTOR is a unibyte string containing the
serialized file descriptor message for PROTO-FILE.  DEPS is a list of
other protocol buffer files that PROTO-FILE depends on.  MESSAGES is a
list of (FULL-NAME . FIELDS) pairs, where FULL-NAME specifies the
qualified name of a protocol buffer message type, and FIELDS is a list
of its field names.  ENUMS is a list of (FULL-NAME . VALUES) pairs,
where FULL-NAME specifies the qualified name of a protocol buffer
enumeration type, and VALUES is a list of (NAME NUMBER) pairs."
  (declare (ftype (function (cons) t)))
  (cl-check-type proto-file @simple-string)
  (cl-check-type descriptor string)
  (cl-check-type deps list)
  (cl-check-type messages list)
  (cl-check-type enums list)
  (with-temp-buffer
    (let* ((feature proto-file)
           (output-file (concat proto-file ".el"))
           (output-name (file-name-nondirectory output-file))
           (standard-output (current-buffer))
           (print-level nil)
           (print-length nil)
           (print-circle t)
           (print-gensym t)
           (print-escape-control-characters t)
           (print-escape-newlines t)
           (print-escape-nonascii t)
           (pp-escape-newlines t))
      (cl-check-type output-name @simple-string)
      (insert ";;; " output-name " --- protocol buffer library " proto-file
              " -*- lexical-binding: t; -*-\n\n"
              ";;; Commentary:\n\n"
              ";; A generated protocol buffer library.\n"
              ";; This file was generated from the following file:\n")
      (insert ";;   " proto-file "\n")
      (insert "\n;;; Code:\n\n")
      (prin1 '(require 'cl-lib)) (terpri) (terpri)
      (prin1 '(require 'elisp/proto/proto)) (terpri) (terpri)
      (dolist (dep deps)
        (cl-check-type dep @simple-string)
        (prin1 `(require ',(intern dep))) (terpri))
      (when deps (terpri))
      (prin1 `(elisp/proto/register-file-descriptor ,descriptor))
      (terpri) (terpri)
      (pcase-dolist (`(,full-name . ,fields) messages)
        (@generate-message full-name fields))
      (pcase-dolist (`(,full-name . ,values) enums)
        (@generate-enum full-name values))
      (prin1 `(provide ',(intern feature))) (terpri) (terpri)
      (insert ";; Local Variables:\n"
              ;; Generated docstrings can be overly long if they contain lengthy
              ;; message names.  Don’t issue byte-compiler warnings for them.
              ";; byte-compile-docstring-max-column: 500\n"
              ";; End:\n\n"
              ";;; " output-name " ends here\n")
      (cons output-file
            (buffer-substring-no-properties (point-min) (point-max))))))

(set-binary-mode 'stdin :binary)
(set-binary-mode 'stdout :binary)
(let* ((standard-output #'external-debugging-output)
       (inhibit-modification-hooks t)
       (stdin (with-temp-buffer
                (set-buffer-multibyte nil)
                (elisp/proto/insert-stdin)
                (buffer-substring-no-properties (point-min) (point-max))))
       (request (elisp/proto/parse-code-generator-request stdin))
       (response (mapcar #'@generate-file request))
       (stdout (elisp/proto/serialize-code-generator-response response)))
  (elisp/proto/write-stdout stdout))

;; Page delimiter so that Emacs doesn’t get confused by the strings above.  See
;; Info node ‘(emacs) Specifying File Variables’.


;; Local Variables:
;; read-symbol-shorthands: (("@" . "elisp/private/tools/gen-proto--"))
;; End:

;;; gen-proto.el ends here
