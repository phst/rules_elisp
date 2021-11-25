;;; runfiles.el --- access to Bazel runfiles  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021 Google LLC
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

;; This library implements support for Bazel runfiles.  See
;; https://docs.bazel.build/skylark/rules.html#runfiles for some background
;; about runfiles.
;;
;; Use the function ‘elisp/runfiles/rlocation’ to map a runfile name to a
;; filename in the local filesystem.  For more advanced use cases, see the
;; class ‘elisp/runfiles/runfiles’.
;;
;; This library also provides a file name handler for runfiles,
;; ‘elisp/runfiles/file-handler’.  It uses the prefix "/bazel-runfile:".

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'pcase)
(require 'rx)


;;;; Main interface:

(defclass elisp/runfiles/runfiles () ()
  :abstract t
  :documentation "Provides access to Bazel runfiles.
This class is abstract; use ‘elisp/runfiles/make’ to create
instances or ‘elisp/runfiles/get’ access a global instance.")

(cl-defun elisp/runfiles/make (&key (manifest (getenv "RUNFILES_MANIFEST_FILE"))
                                    (directory (getenv "RUNFILES_DIR")))
  "Return a new instance of a subclass of ‘elisp/runfiles/runfiles’.
MANIFEST and DIRECTORY specify the location of the runfiles.  By
default, use environmental variable to find the runfiles.
MANIFEST specifies the filename of a runfile manifest.  DIRECTORY
specifies the runfile directory.  Either of them can be nil or
empty.  If the runfiles aren’t found at either location, signal
an error of type ‘elisp/runfiles/not-found’."
  (cl-check-type manifest (or null string))
  (cl-check-type directory (or null string))
  (or (and manifest (not (string-equal manifest ""))
           (file-regular-p manifest)
           (file-readable-p manifest)
           (elisp/runfiles/make--manifest manifest))
      (and directory (not (string-equal directory ""))
           (file-accessible-directory-p directory)
           (elisp/runfiles/make--directory directory))
      (signal 'elisp/runfiles/not-found (list manifest directory))))

(defvar elisp/runfiles/global--cache nil
  "Cache for ‘elisp/runfiles/get’.")

(defun elisp/runfiles/get ()
  "Return a global instance of the class ‘elisp/runfiles/runfiles’.
Cache the result once created; this means that changes to the
runfile-specific environmental variables don’t take effect once
the global instance is initialized."
  (or elisp/runfiles/global--cache
      (setq elisp/runfiles/global--cache (elisp/runfiles/make))))

(cl-deftype elisp/runfiles/filename ()
  '(and string (satisfies elisp/runfiles/filename-p)))

(cl-defun elisp/runfiles/rlocation
    (filename &optional (runfiles (elisp/runfiles/get)))
  "Return a filename corresponding to the runfile FILENAME.
RUNFILES must be an object of the class
‘elisp/runfiles/runfiles’; it defaults to a global instance.
Signal an error of type ‘elisp/runfiles/not-found’ if FILENAME
wasn’t found in the runfiles tree.  Signal an error of type
‘elisp/runfiles/empty’ if FILENAME is present in the runfiles
manifest, but doesn’t map to a real file on the filesystem; this
indicates that an empty file should be used in its place."
  (cl-check-type filename elisp/runfiles/filename)
  (cl-check-type runfiles elisp/runfiles/runfiles)
  (elisp/runfiles/rlocation--internal runfiles filename))

(cl-defun elisp/runfiles/env-vars (&optional (runfiles (elisp/runfiles/get)))
  "Return a list of environmental variable for subprocesses.
Prepend this list to ‘process-environment’ if you want to invoke
a process that should have access to the runfiles.  RUNFILES must
be an object of the class ‘elisp/runfiles/runfiles’; it defaults
to a global instance."
  (cl-check-type runfiles elisp/runfiles/runfiles)
  (elisp/runfiles/env-vars--internal runfiles))

(defun elisp/runfiles/filename-p (string)
  "Return whether STRING is a possible argument for ‘elisp/runfiles/rlocation’."
  (let ((case-fold-search nil))
    ;; This uses similar criteria as
    ;; https://github.com/bazelbuild/bazel/blob/2.2.0/tools/cpp/runfiles/runfiles_src.cc#L171.
    ;; We also ban absolute filenames as well as some forms that only exist in
    ;; Emacs.
    (and (stringp string)
         (not (file-remote-p string))
         (not (file-name-quoted-p string))
         (not (file-name-absolute-p string))
         (not (string-prefix-p "../" string))
         (not (string-match-p (rx "/..") string))
         (not (string-prefix-p "./" string))
         (not (string-match-p (rx "/./") string))
         (not (string-suffix-p "/." string))
         (not (string-match-p (rx "//") string))
         (not (string-prefix-p "\\" string)))))


;;;; Error symbols:

;; We derive them from ‘file-error’, which represents a generic I/O error.
(define-error 'elisp/runfiles/not-found "Runfiles not found" 'file-missing)
(define-error 'elisp/runfiles/read-only "Runfiles are read-only" 'file-error)
(define-error 'elisp/runfiles/syntax-error "Syntax error in runfiles manifest")
(define-error 'elisp/runfiles/empty "Empty runfile")


;;;; File name handler:

(defun elisp/runfiles/install-handler ()
  "Install the file name handler ‘elisp/runfiles/file-handler’.
See Info node ‘(elisp)Magic File Names’ for background on file
name handlers.  Once installed, a filename starting with the
magic string \"/bazel-runfile:\" will resolve to a file in the
runfiles tree.  For example, \"/bazel-runfile:dir/file\" will
have the same effect as using
‘(elisp/runfiles/get \"dir/file\")’."
  (cl-pushnew (cons (rx bos "/bazel-runfile:") #'elisp/runfiles/file-handler)
              file-name-handler-alist))

(defun elisp/runfiles/file-handler (operation &rest args)
  "File name handler for Bazel runfiles.
See Info node ‘(elisp)Magic File Names’ for background on file
name handlers.  OPERATION is the file operation to perform, an
ARGS are the arguments to the operation."
  (let ((inhibit-file-name-handlers
         (cons #'elisp/runfiles/file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation)
        (handler-func (intern (format "elisp/runfiles/handle--%s" operation))))
    (cond
     ((fboundp handler-func)
      ;; A specific handler exists; call it.
      (apply handler-func args))
     ((memq operation '(file-name-case-insensitive-p
                        file-writable-p
                        find-backup-file-name
                        vc-registered))
      ;; These operations can trivially return nil for runfiles.
      nil)
     ((memq operation '(delete-directory
                        delete-file
                        dired-compress-file
                        make-directory
                        make-directory-internal
                        rename-file
                        set-file-acl
                        set-file-modes
                        set-file-selinux-context
                        set-file-times
                        write-region))
      ;; These operations would require the runfiles tree to be writable.
      (signal 'elisp/runfiles/read-only nil))
     ;; Attempt to handle everything else in a generic way.
     (t (elisp/runfiles/handle--generic operation args)))))

(defun elisp/runfiles/handle--directory-files
    (directory full-name match-regexp nosort)
  "Implementation of ‘directory-files’ for Bazel runfiles.
See Info node ‘(elisp) Contents of Directories’ for the meaning
of DIRECTORY, FULL-NAME, MATCH-REGEXP, and NOSORT."
  (let ((files (directory-files (elisp/runfiles/transform--name directory) nil
                                match-regexp nosort)))
    (if full-name
        (mapcar (lambda (file) (expand-file-name file directory)) files)
      files)))

(defun elisp/runfiles/handle--directory-files-and-attributes
    (directory full-name match-regexp nosort id-format)
  "Implementation of ‘directory-files-and-attributes’ for Bazel runfiles.
See Info node ‘(elisp) Contents of Directories’ for the meaning
of DIRECTORY, FULL-NAME, MATCH-REGEXP, NOSORT, and ID-FORMAT."
  (let ((files (directory-files-and-attributes
                (elisp/runfiles/transform--name directory) nil
                match-regexp nosort id-format)))
    (if full-name
        (mapcar (lambda (info)
                  (setcar info (expand-file-name (car info) directory)))
                files)
      files)))

(defun elisp/runfiles/handle--expand-file-name (filename &optional directory)
  "Implementation of ‘expand-file-name’ for Bazel runfiles.
See Info node ‘(elisp) File Name Expansion’ for the meaning of
FILENAME and DIRECTORY."
  (cond ((string-prefix-p "/bazel-runfile:" filename)
         ;; Bazel runfile names are always canonical (or invalid), so return the
         ;; name as-is.
         filename)
        ((file-name-absolute-p filename)
         (expand-file-name filename "/"))
        (t (let ((directory (or directory default-directory)))
             (if (string-prefix-p "/bazel-runfile:" directory)
                 (concat (file-name-as-directory directory) filename)
               (expand-file-name filename directory))))))

(defun elisp/runfiles/handle--file-remote-p (file identification _connected)
  "Implementation of ‘file-remote-p’ for Bazel runfiles.
See Info node ‘(elisp) Magic File Names’ for the meaning of FILE
and IDENTIFICATION."
  (pcase-exhaustive file
    ((rx bos "/bazel-runfile:" (let name (* anything)) eos)
     (cl-ecase identification
       ((nil) "/bazel-runfile:")
       (method "bazel-runfile")
       ((user host) nil)
       (localname name)))))

(defun elisp/runfiles/handle--get-file-buffer (filename)
  "Implementation of ‘get-file-buffer’ for Bazel runfiles.
See Info node ‘(elisp) Buffer File Name’ for the meaning of
FILENAME."
  ;; We accept both buffers visiting FILENAME directly as well as the
  ;; transformed filename.
  (or (get-file-buffer filename)
      (get-file-buffer (elisp/runfiles/transform--name filename))))

(defun elisp/runfiles/handle--process-file
    (program infile buffer display &rest args)
  "Implementation of ‘process-file’ for Bazel runfiles.
See Info node ‘(elisp) Synchronous Processes’ for the meaning of
PROGRAM, INFILE, BUFFER, DISPLAY, and ARGS."
  (let ((default-directory (elisp/runfiles/transform--name default-directory)))
    (apply #'process-file
           (elisp/runfiles/transform--name program)
           (elisp/runfiles/transform--name infile)
           (pcase-exhaustive buffer
             (`(:file ,stdout)
              `(:file ,(elisp/runfiles/transform--name stdout)))
             (`((:file ,stdout) ,stderr)
              `((:file ,(elisp/runfiles/transform--name stdout))
                ,(elisp/runfiles/transform--name stderr)))
             (`(,stdout ,stderr)
              `(,stdout ,(elisp/runfiles/transform--name stderr)))
             (buffer buffer))
           display
           args)))

(defun elisp/runfiles/handle--start-file-process
    (name buffer-or-name program &rest args)
  "Implementation of ‘start-file-process’ for Bazel runfiles.
See Info node ‘(elisp) Asynchronous Processes’ for the meaning of
NAME, BUFFER-OR-NAME, PROGRAM, and ARGS."
  (let ((default-directory (elisp/runfiles/transform--name default-directory)))
    (apply #'start-file-process
           name buffer-or-name (elisp/runfiles/transform--name program) args)))

(defun elisp/runfiles/handle--unhandled-file-name-directory (filename)
  "Implementation of ‘unhandled-file-name-directory’ for Bazel runfiles.
See Info node ‘(elisp) Magic File Names’ for the meaning of
FILENAME."
  (file-name-as-directory
   (condition-case nil
       (elisp/runfiles/transform--name filename)
     (elisp/runfiles/not-found temporary-file-directory))))

(defconst elisp/runfiles/generic--handlers
  '((add-name-to-file file newfile arg)
    (access-file file arg)
    (byte-compiler-base-file-name file)
    (copy-directory file newfile arg arg arg)
    (copy-file file newfile arg arg arg arg)
    (diff-latest-backup-file file)
    (directory-file-name arg)
    (directory-files file arg arg arg)
    (directory-files-and-attributes file arg arg arg arg)
    (dired-uncache file)
    (file-accessible-directory-p file noerror)
    (file-attributes file arg)
    (file-acl file)
    (file-directory-p file noerror)
    (file-equal-p file file noerror)
    (file-executable-p file noerror)
    (file-exists-p file noerror)
    (file-in-directory-p file file noerror)
    (file-local-copy file)
    (file-modes file)
    (file-name-all-completions file file noerror)
    (file-name-as-directory arg)
    (file-name-completion file file arg noerror)
    (file-name-directory arg)
    (file-newer-than-file-p file file noerror)
    (file-name-nondirectory arg)
    (file-notify-add-watch file arg arg)
    (file-notify-rm-watch arg)
    (file-notify-valid-p arg)
    (file-ownership-preserved-p file arg noerror)
    (file-readable-p file noerror)
    (file-regular-p file noerror)
    (file-name-sans-versions arg arg)
    (file-selinux-context file)
    (file-symlink-p file noerror)
    (file-truename file)
    (insert-directory file arg arg arg)
    (insert-file-contents file arg arg arg arg)
    (load file arg arg arg arg)
    (make-auto-save-file-name)
    (make-nearby-temp-file file arg arg)
    (make-symbolic-link name newfile arg)
    (set-visited-file-modtime arg)
    (shell-command arg arg arg)
    (substitute-in-file-name arg)
    (temporary-file-directory)
    (verify-visited-file-modtime arg))
  "Alist of file operations that can be handled generically.
Each element is a list (OPERATION . ARGS), where OPERATION
specifies the file operation to be handled.  ARGS specifies how
to transform arguments for OPERATION.  Each element of ARGS
corresponds to the argument at the same position.  Each element
of ARGS must be one of the following symbols:

- ‘arg’: Don’t modify the argument; pass it on as-is.  This is
  for arguments that don’t represent filenames.

- ‘file’: Transform the filename using
  ‘elisp/runfiles/transform--filename’.  This is for files the
  operation should read.

- ‘newfile’: Signal an error of type ‘elisp/runfiles/read-only’
  if the argument is a Bazel runfile filename, otherwise pass it
  on as-is.  This is for files the operation would attempt to
  create.

In addition, ARGS can contain the symbol ‘noerror’ as its last
element.  If ‘noerror’ is present, return nil instead of
signaling an ‘elisp/runfiles/not-found’ error if a runfile isn’t
found.  This is useful for predicates like ‘file-readable-p’.")

(defun elisp/runfiles/handle--generic (operation args)
  "Handle file operation OPERATION in a generic way.
ARGS are the arguments to the operation; they are handled as
determined by the constant ‘elisp/runfiles/generic--handlers’,
which see."
  (let ((match (assq operation elisp/runfiles/generic--handlers)))
    (unless match
      (error "Unhandled file name operation %s" operation))
    (cl-flet ((invoke
               ()
               (let ((default-directory
                       (elisp/runfiles/transform--name default-directory)))
                 (apply operation
                        (cl-loop
                         for type in (remq 'noerror (cdr match))
                         for arg in args
                         collect
                         (cl-ecase type
                           (file (elisp/runfiles/transform--name arg))
                           (newfile
                            (if (string-prefix-p "/bazel-runfile:" arg)
                                (signal 'elisp/runfiles/read-only nil)
                              arg))
                           (arg arg)))))))
      (if (memq 'noerror (cdr match))
          (condition-case nil
              (invoke)
            (elisp/runfiles/not-found nil))
        (invoke)))))

(defun elisp/runfiles/transform--name (filename)
  "Return the filename in the filesystem corresponding to the runfile FILENAME.
If FILENAME doesn’t start with \"/bazel-runfile:\", return it
unchanged."
  (pcase-exhaustive filename
    ((and (pred stringp) (rx bos "/bazel-runfile:" (let name (* anything)) eos))
     (elisp/runfiles/rlocation name))
    (other other)))


;;;; Implementations of the ‘elisp/runfiles/runfiles’ class:

(defclass elisp/runfiles/runfiles--manifest (elisp/runfiles/runfiles)
  ((filename :initarg :filename :type string)
   (manifest :initarg :manifest :type hash-table))
  :documentation "Manifest-based runfiles implementation.")

(defun elisp/runfiles/make--manifest (filename)
  "Parse the runfile manifest in the file FILENAME.
Return an object of class ‘elisp/runfiles/runfiles--manifest’."
  (let ((manifest (make-hash-table :test #'equal)))
    (with-temp-buffer
      ;; At least Java hard-codes UTF-8 for runfiles manifest, see
      ;; https://github.com/bazelbuild/bazel/blob/4.2.1/tools/java/runfiles/Runfiles.java#L204.
      (let ((coding-system-for-read 'utf-8)
            (format-alist nil)
            (after-insert-file-functions nil))
        (insert-file-contents filename))
      ;; Perform the same parsing as
      ;; https://github.com/bazelbuild/bazel/blob/2.2.0/tools/cpp/runfiles/runfiles_src.cc#L191.
      (while (not (eobp))
        (pcase (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
          ((rx bol (let key (+ (not (any ?\n ?\s))))
               ?\s (let value (+ nonl)) eol)
           ;; Runfiles are always local, so quote them unconditionally.
           (puthash key (concat "/:" value) manifest))
          ((rx bol (let key (+ (not (any ?\n ?\s)))) ?\s eol)
           (puthash key :empty manifest))
          (other (signal 'elisp/runfiles/syntax-error (list filename other))))
        (forward-line)))
    (elisp/runfiles/runfiles--manifest :filename filename
                                       :manifest manifest)))

(cl-defmethod elisp/runfiles/rlocation--internal
  ((runfiles elisp/runfiles/runfiles--manifest) filename)
  "Implementation of ‘elisp/runfiles/rlocation’ for manifest-based runfiles.
RUNFILES is a runfiles object and FILENAME the name to look up."
  (let ((result (gethash filename (oref runfiles manifest))))
    (cond
      ((not result)
       (signal 'elisp/runfiles/not-found
               (list filename (oref runfiles filename))))
      ((eq result :empty)
       (signal 'elisp/runfiles/empty
               (list filename (oref runfiles filename))))
      (t result))))

(cl-defmethod elisp/runfiles/env-vars--internal
  ((runfiles elisp/runfiles/runfiles--manifest))
  "Implementation of ‘elisp/runfiles/rlocation’ for manifest-based runfiles.
RUNFILES is a runfiles object."
  (list (concat "RUNFILES_MANIFEST_FILE=" (oref runfiles filename))
        "RUNFILES_DIR" "JAVA_RUNFILES"))

(defclass elisp/runfiles/runfiles--directory (elisp/runfiles/runfiles)
  ((directory :initarg :directory :type string))
  :documentation "Directory-based runfiles implementation.")

(defun elisp/runfiles/make--directory (directory)
  "Create a directory-based runfiles object for DIRECTORY.
Return an object of class ‘elisp/runfiles/runfiles--directory’."
  (cl-check-type directory (and string (not file-remote) file-name-absolute))
  (elisp/runfiles/runfiles--directory
   :directory (file-name-as-directory directory)))

(cl-defmethod elisp/runfiles/rlocation--internal
  ((runfiles elisp/runfiles/runfiles--directory) filename)
  "Implementation of ‘elisp/runfiles/rlocation’ for directory-based runfiles.
RUNFILES is a runfiles object and FILENAME the name to look up."
  (expand-file-name filename (oref runfiles directory)))

(cl-defmethod elisp/runfiles/env-vars--internal
  ((runfiles elisp/runfiles/runfiles--directory))
  "Implementation of ‘elisp/runfiles/env-vars’ for directory-based runfiles.
RUNFILES is a runfiles object."
  (with-slots (directory) runfiles
    (list "RUNFILES_MANIFEST_FILE"
          (concat "RUNFILES_DIR=" directory)
          ;; TODO(laszlocsomor): remove JAVA_RUNFILES once the Java launcher
          ;; can pick up RUNFILES_DIR.
          (concat "JAVA_RUNFILES=" directory))))

(provide 'elisp/runfiles/runfiles)
;;; runfiles.el ends here
