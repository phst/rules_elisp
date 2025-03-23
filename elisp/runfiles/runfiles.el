;;; runfiles.el --- access to Bazel runfiles  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022, 2023, 2024, 2025 Google LLC
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

;; This library implements support for Bazel runfiles.
;; See Info node ‘(rules_elisp) Runfiles’ for more information and usage
;; instructions.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'rx)
(require 'subr-x)


;;;; Main interface:

(cl-defstruct (elisp/runfiles/runfiles
               (:conc-name @runfiles-)
               (:constructor nil)
               (:constructor @runfiles-make (impl repo-mapping))
               (:copier nil)
               :noinline)
  "Provides access to Bazel runfiles.
Use ‘elisp/runfiles/make’ to create instances of
‘elisp/runfiles/runfiles’, or ‘elisp/runfiles/get’ access a
global instance.  All structure fields are implementation
details."
  (impl nil :read-only t)
  (repo-mapping nil :type (or null hash-table) :read-only t))

(cl-defun elisp/runfiles/make (&key manifest directory)
  "Return a new instance of the type ‘elisp/runfiles/runfiles’.
MANIFEST and DIRECTORY specify the location of the runfiles.  By
default, use environmental variable to find the runfiles.
MANIFEST specifies the filename of a runfile manifest.  DIRECTORY
specifies the runfile directory.  Either of them can be nil or
empty.  If the runfiles aren’t found at either location, signal
an error of type ‘elisp/runfiles/not-found’."
  (declare (ftype (function (&rest t) elisp/runfiles/runfiles)))
  (cl-check-type manifest (or null string))
  (cl-check-type directory (or null string))
  (cl-flet ((env (var)
              (let ((value (getenv var))
                    (file-name-handler-alist ()))
                (and value (not (string-empty-p value))
                     (concat "/:" (expand-file-name value))))))
    (unless manifest
      (when-let ((value (env "RUNFILES_MANIFEST_FILE")))
        (setq manifest value)))
    (unless directory
      (when-let ((value (env "RUNFILES_DIR")))
        (setq directory value))))
  (let* ((impl (or (and manifest (not (string-empty-p manifest))
                        (file-regular-p manifest)
                        (file-readable-p manifest)
                        (@make-manifest manifest))
                   (and directory (not (string-empty-p directory))
                        (file-accessible-directory-p directory)
                        (@make-directory directory))
                   (signal 'elisp/runfiles/not-found
                           (list "Runfiles not found" manifest directory))))
         (mapping (when-let ((file (ignore-error elisp/runfiles/not-found
                                     (@rlocation impl "_repo_mapping"))))
                    (@parse-repo-mapping file))))
    (@runfiles-make impl mapping)))

(defvar @global-cache nil
  "Cache for ‘elisp/runfiles/get’.")

(defun elisp/runfiles/get ()
  "Return a global instance of the type ‘elisp/runfiles/runfiles’.
Cache the result once created; this means that changes to the
runfile-specific environmental variables don’t take effect once
the global instance is initialized."
  (declare (ftype (function () elisp/runfiles/runfiles)))
  (or @global-cache (setq @global-cache (elisp/runfiles/make))))

(cl-deftype elisp/runfiles/filename ()
  '(and string (satisfies elisp/runfiles/filename-p)))

(cl-defun elisp/runfiles/rlocation
    (filename &optional runfiles &key caller-repo)
  "Return a filename corresponding to the runfile FILENAME.
RUNFILES must be an object of the type ‘elisp/runfiles/runfiles’;
it defaults to a global instance.  CALLER-REPO, if present, is
the canonical name of the repository of the calling function;
it’s used for repository mappings.  If CALLER-REPO is not given
and a call to the ‘elisp/runfiles/rlocation’ function is being
compiled, attempt to determine the caller’s repository using
‘macroexp-file-name’.  Signal an error of type
‘elisp/runfiles/not-found’ if FILENAME wasn’t found in the
runfiles tree.  Signal an error of type ‘elisp/runfiles/empty’ if
FILENAME is present in the runfiles manifest, but doesn’t map to
a real file on the filesystem; this indicates that an empty file
should be used in its place."
  (declare (ftype (function (elisp/runfiles/filename
                             &optional (or null elisp/runfiles/runfiles)
                             &rest t)
                            string)))
  (cl-check-type filename elisp/runfiles/filename)
  (cl-check-type runfiles (or null elisp/runfiles/runfiles))
  (cl-check-type caller-repo (or null string))
  (unless runfiles (setq runfiles (elisp/runfiles/get)))
  (when-let ((table (@runfiles-repo-mapping runfiles))
             (canonical caller-repo))
    (pcase-exhaustive filename
      ((rx bos
           (let apparent (+ (not (any "/\n"))))
           (let rest (? ?/ (+ nonl)))
           eos)
       (when-let ((mapping (gethash (cons canonical apparent) table)))
         (setq filename (concat mapping rest))))))
  (@rlocation (@runfiles-impl runfiles) filename))

(eval-and-compile
  (defun elisp/runfiles/current-repo ()
    "Attempt to detect the repository containing the file being compiled.
This works somewhat reliably for files being compiled as part of
a Bazel action (e.g., ‘elisp_library’ targets).  The result is
only meaningful while byte compilation is in progress (e.g.,
while expanding macros during byte compilation).  Return nil if
the current repository can’t be determined."
    (declare (ftype (function () (or null string)))
             (side-effect-free error-free))
    ;; ‘elisp/current-repository’ is bound by //elisp:compile.el.
    (or (bound-and-true-p elisp/current-repository)
        (pcase (macroexp-file-name)
          ;; The directory after the execution root should be the repository
          ;; name at compile time.  See
          ;; https://bazel.build/remote/output-directories#layout-diagram.  If
          ;; --incompatible_sandbox_hermetic_tmp is enabled, the execution root
          ;; is /tmp/bazel-working-directory or /tmp/bazel-execroot instead;
          ;; cf. https://github.com/bazelbuild/bazel/blob/master/src/main/java/com/google/devtools/build/lib/sandbox/LinuxSandboxedSpawnRunner.java.
          ((rx ?/ (or "execroot" "bazel-working-directory" "bazel-execroot") ?/
               (let name (+ (not (any ?/)))) ?/)
           ;; The canonical name of the main repository is the empty string.
           (if (string-equal name "_main") "" name))))))

;; A somewhat robust way to determine the caller repository is at compile time.
;; We could also try to determine the caller at runtime using ‘backtrace-frame’
;; and ‘symbol-file’, but that doesn’t work in cases where the caller is an
;; anonymous function, e.g., in ERT tests.
(cl-define-compiler-macro elisp/runfiles/rlocation
    (&whole form filename &optional runfiles &key caller-repo)
  (unless caller-repo
    (when-let ((name (elisp/runfiles/current-repo)))
      (setq form `(elisp/runfiles/rlocation ,filename ,runfiles
                                            :caller-repo ,name))))
  form)

(cl-defun elisp/runfiles/env-vars
    (&optional runfiles (remote (file-remote-p default-directory)))
  "Return a list of environmental variable for subprocesses.
Prepend this list to ‘process-environment’ if you want to invoke
a process that should have access to the runfiles.  RUNFILES must
be an object of the type ‘elisp/runfiles/runfiles’; it defaults
to a global instance.  REMOTE must be nil or a remote host
identifier (see Info node ‘(elisp) Magic File Names’); if the
returned values are on the same remote host as REMOTE, return the
local names on that host, otherwise signal an error of type
‘elisp/runfiles/remote’."
  (declare (ftype (function (&optional (or null elisp/runfiles/runfiles)
                                       (or string null))
                            list)))
  (cl-check-type runfiles (or null elisp/runfiles/runfiles))
  (cl-check-type remote (or string null))
  (unless runfiles (setq runfiles (elisp/runfiles/get)))
  (@env-vars (@runfiles-impl runfiles) remote))

(defun elisp/runfiles/filename-p (string)
  "Return whether STRING is a possible argument for ‘elisp/runfiles/rlocation’."
  (declare (ftype (function (string) t)))
  (let ((case-fold-search nil))
    ;; This uses similar criteria as
    ;; https://github.com/bazelbuild/bazel/blob/7.4.1/tools/cpp/runfiles/runfiles_src.cc#L221-L223.
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
(define-error 'elisp/runfiles/remote "Remote runfiles manifest or directory")


;;;; File name handler:

(defun elisp/runfiles/install-handler ()
  "Install the file name handler ‘elisp/runfiles/file-handler’.
See Info node ‘(elisp)Magic File Names’ for background on file
name handlers.  Once installed, a filename starting with the
magic string \"/bazel-runfile:\" will resolve to a file in the
runfiles tree.  For example, \"/bazel-runfile:dir/file\" will
have the same effect as using
‘(elisp/runfiles/get \"dir/file\")’."
  (declare (ftype (function () t)))
  (cl-pushnew (cons (rx bos "/bazel-runfile:") #'elisp/runfiles/file-handler)
              file-name-handler-alist
              :test #'equal))

(defun elisp/runfiles/file-handler (operation &rest args)
  "File name handler for Bazel runfiles.
See Info node ‘(elisp)Magic File Names’ for background on file
name handlers.  OPERATION is the file operation to perform, an
ARGS are the arguments to the operation."
  (declare (ftype (function (symbol &rest t) t)))
  (let ((inhibit-file-name-handlers
         (cons #'elisp/runfiles/file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation)
        (handler-func
         (intern (format "elisp/runfiles/runfiles--%s" operation))))
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
     (t (@handle-generic operation args)))))

(defun @abbreviate-file-name (filename)
  "Implementation of ‘abbreviate-file-name’ for Bazel runfiles.
See Info node ‘(elisp) Directory Names’ for the meaning of
FILENAME."
  (declare (ftype (function (string) string)))
  ;; We don’t support abbreviated file names, so we return the file name
  ;; unchanged.
  filename)

(defun @directory-files (directory full-name match-regexp nosort count)
  "Implementation of ‘directory-files’ for Bazel runfiles.
See Info node ‘(elisp) Contents of Directories’ for the meaning of
DIRECTORY, FULL-NAME, MATCH-REGEXP, NOSORT, and COUNT."
  (declare (ftype (function (string t (or null string) t (or null integer))
                            list)))
  (let ((files (directory-files (@transform-name directory) nil
                                match-regexp nosort count)))
    (if full-name
        (mapcar (lambda (file) (expand-file-name file directory)) files)
      files)))

(defun @directory-files-and-attributes
    (directory full-name match-regexp nosort id-format count)
  "Implementation of ‘directory-files-and-attributes’ for Bazel runfiles.
See Info node ‘(elisp) Contents of Directories’ for the meaning of
DIRECTORY, FULL-NAME, MATCH-REGEXP, NOSORT, ID-FORMAT, and COUNT."
  (declare (ftype (function
                   (string t (or null string) t symbol (or null integer))
                   list)))
  (let ((files (directory-files-and-attributes
                (@transform-name directory) nil
                match-regexp nosort id-format count)))
    (if full-name
        (mapcar (lambda (info)
                  (setcar info (expand-file-name (car info) directory)))
                files)
      files)))

(defun @expand-file-name (filename directory)
  "Implementation of ‘expand-file-name’ for Bazel runfiles.
See Info node ‘(elisp) File Name Expansion’ for the meaning of
FILENAME and DIRECTORY."
  (declare (ftype (function (string (or null string)) string)))
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

(defun @file-remote-p (file identification _connected)
  "Implementation of ‘file-remote-p’ for Bazel runfiles.
See Info node ‘(elisp) Magic File Names’ for the meaning of FILE
and IDENTIFICATION."
  (declare (ftype (function (string symbol t) (or null string))))
  (pcase-exhaustive file
    ((rx bos "/bazel-runfile:" (let name (* anything)) eos)
     (cl-ecase identification
       ((nil) "/bazel-runfile:")
       (method "bazel-runfile")
       ((user host) nil)
       (localname name)))))

(defun @get-file-buffer (filename)
  "Implementation of ‘get-file-buffer’ for Bazel runfiles.
See Info node ‘(elisp) Buffer File Name’ for the meaning of
FILENAME."
  (declare (ftype (function (string) (or null buffer))))
  ;; We accept both buffers visiting FILENAME directly as well as the
  ;; transformed filename.
  (or (get-file-buffer filename)
      (get-file-buffer (@transform-name filename))))

(defun @process-file (program infile buffer display &rest args)
  "Implementation of ‘process-file’ for Bazel runfiles.
See Info node ‘(elisp) Synchronous Processes’ for the meaning of
PROGRAM, INFILE, BUFFER, DISPLAY, and ARGS."
  (declare (ftype (function (string string t t &rest string) t)))
  (let ((default-directory (@transform-name default-directory)))
    (apply #'process-file
           (@transform-name program)
           (@transform-name infile)
           (pcase-exhaustive buffer
             (`(:file ,stdout)
              `(:file ,(@transform-name stdout)))
             (`((:file ,stdout) ,stderr)
              `((:file ,(@transform-name stdout))
                ,(@transform-name stderr)))
             (`(,stdout ,stderr)
              `(,stdout ,(@transform-name stderr)))
             (buffer buffer))
           display
           args)))

(defun @start-file-process (name buffer-or-name program &rest args)
  "Implementation of ‘start-file-process’ for Bazel runfiles.
See Info node ‘(elisp) Asynchronous Processes’ for the meaning of
NAME, BUFFER-OR-NAME, PROGRAM, and ARGS."
  (declare (ftype (function (string t string &rest string) process)))
  (let ((default-directory (@transform-name default-directory)))
    (apply #'start-file-process
           name buffer-or-name (@transform-name program) args)))

(defun @unhandled-file-name-directory (filename)
  "Implementation of ‘unhandled-file-name-directory’ for Bazel runfiles.
See Info node ‘(elisp) Magic File Names’ for the meaning of
FILENAME."
  (declare (ftype (function (string) string)))
  (file-name-as-directory
   (condition-case nil
       (@transform-name filename)
     (elisp/runfiles/not-found temporary-file-directory))))

(defconst @generic-handlers
  '((add-name-to-file file newfile arg)
    (access-file file arg)
    (byte-compiler-base-file-name file)
    (copy-directory file newfile arg arg arg)
    (copy-file file newfile arg arg arg arg)
    (diff-latest-backup-file file)
    (directory-file-name arg)
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
    (file-truename arg)
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

- ‘file’: Transform the filename using ‘@transform-name’.  This is for
  files the operation should read.

- ‘newfile’: Signal an error of type ‘elisp/runfiles/read-only’
  if the argument is a Bazel runfile filename, otherwise pass it
  on as-is.  This is for files the operation would attempt to
  create.

In addition, ARGS can contain the symbol ‘noerror’ as its last
element.  If ‘noerror’ is present, return nil instead of
signaling an ‘elisp/runfiles/not-found’ error if a runfile isn’t
found.  This is useful for predicates like ‘file-readable-p’.")

(defun @handle-generic (operation args)
  "Handle file operation OPERATION in a generic way.
ARGS are the arguments to the operation; they are handled as determined
by the constant ‘@generic-handlers’, which see."
  (declare (ftype (function (symbol list) t)))
  (let ((match (assq operation @generic-handlers)))
    (unless match
      (error "Unhandled file name operation %s" operation))
    (cl-flet ((invoke ()
                (let ((default-directory (@transform-name default-directory)))
                  (apply operation
                         (cl-loop
                          for type in (remq 'noerror (cdr match))
                          and arg in args
                          collect
                          (cl-ecase type
                            (file (@transform-name arg))
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

(defun @transform-name (filename)
  "Return the filename in the filesystem corresponding to the runfile FILENAME.
If FILENAME doesn’t start with \"/bazel-runfile:\", return it
unchanged."
  (declare (ftype (function (string) string)))
  (pcase-exhaustive filename
    ((and (pred stringp) (rx bos "/bazel-runfile:" (let name (* anything)) eos))
     (elisp/runfiles/rlocation name))
    (other other)))


;;;; Implementations of the ‘elisp/runfiles/runfiles’ type:

(cl-defstruct (@manifest
               (:constructor nil)
               (:constructor @manifest-make (filename manifest))
               (:copier nil))
  "Manifest-based runfiles implementation."
  (filename nil :read-only t :type string)
  (manifest nil :read-only t :type hash-table))

(defun @make-manifest (filename)
  "Parse the runfile manifest in the file FILENAME.
Return an object of type ‘@manifest’."
  (declare (ftype (function (string) @manifest)))
  (let ((manifest (make-hash-table :test #'equal))
        (filename (expand-file-name filename)))
    (with-temp-buffer
      ;; See
      ;; https://github.com/bazelbuild/bazel/issues/374#issuecomment-2594713891.
      (let ((coding-system-for-read 'utf-8-unix)
            (format-alist nil)
            (after-insert-file-functions nil))
        (insert-file-contents filename))
      ;; Perform the same parsing as
      ;; https://github.com/bazelbuild/rules_cc/blob/0.1.1/cc/runfiles/runfiles.cc#L274.
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (point) (line-end-position)))
              (escaped (eql (following-char) ?\s)))
          (cl-flet* ((syntax-error ()
                       (signal 'elisp/runfiles/syntax-error
                               (list filename line)))
                     (unescape (string &rest other)
                       (let ((pairs `(,@other ("\\n" . "\n") ("\\b" . "\\"))))
                         (replace-regexp-in-string
                          (rx ?\\ (? anychar))
                          (lambda (seq)
                            (or (cdr (assoc seq pairs)) (syntax-error)))
                          string :fixedcase :literal))))
            (pcase (if escaped (substring-no-properties line 1) line)
              ((rx bos (let key (+ (not (any "\n "))))
                   ?\s (let value (* nonl)) eos)
               (when escaped
                 (cl-callf unescape key '("\\s" . " "))
                 (cl-callf unescape value))
               (puthash key
                        ;; Runfiles are always local, so quote them
                        ;; unconditionally.
                        (if (string-empty-p value) :empty (concat "/:" value))
                        manifest))
              (_ (syntax-error)))))
        (forward-line)))
    (@manifest-make filename manifest)))

(cl-defmethod @rlocation ((runfiles @manifest) filename)
  "Implementation of ‘elisp/runfiles/rlocation’ for manifest-based runfiles.
RUNFILES is a runfiles object and FILENAME the name to look up."
  (let* ((table (@manifest-manifest runfiles))
         (manifest-file (@manifest-filename runfiles))
         (result (gethash filename table)))
    (unless result
      ;; Look for ancestor directory mapping.  See
      ;; https://github.com/bazelbuild/bazel/issues/14336.
      (let ((continue t)
            (candidate filename))
        (while continue
          (pcase candidate
            ((rx bos (let prefix (+ anything)) ?/ (+ anything) (? ?/) eos)
             (if-let ((dir (gethash prefix table)))
                 (setq result (concat dir (substring-no-properties
                                           filename (length prefix)))
                       continue nil)
               (setq candidate prefix)))
            (_ (setq continue nil)))))      )
    (cl-case result
      ((nil)
       (signal 'elisp/runfiles/not-found
               (list "Runfile not found in manifest" filename manifest-file)))
      (:empty (signal 'elisp/runfiles/empty (list filename manifest-file)))
      (otherwise result))))

(cl-defmethod @env-vars ((runfiles @manifest) remote)
  "Implementation of ‘elisp/runfiles/env-vars’ for manifest-based runfiles.
RUNFILES is a runfiles object, and REMOTE is the remote host
identifier."
  (let ((filename (@manifest-filename runfiles)))
    (unless (equal (file-remote-p filename) remote)
      (signal 'elisp/runfiles/remote (list filename remote)))
    (list (concat "RUNFILES_MANIFEST_FILE="
                  (file-name-unquote (file-local-name filename)))
          "RUNFILES_DIR" "JAVA_RUNFILES")))

(cl-defstruct (@directory
               (:constructor nil)
               (:constructor @directory-make (directory))
               (:copier nil))
  "Directory-based runfiles implementation."
  (directory nil :read-only t :type string))

(defun @make-directory (directory)
  "Create a directory-based runfiles object for DIRECTORY.
Return an object of type ‘@directory’."
  (declare (ftype (function (string) @directory)))
  (@directory-make (file-name-as-directory (expand-file-name directory))))

(cl-defmethod @rlocation ((runfiles @directory) filename)
  "Implementation of ‘elisp/runfiles/rlocation’ for directory-based runfiles.
RUNFILES is a runfiles object and FILENAME the name to look up."
  (let ((directory (@directory-directory runfiles)))
    (expand-file-name filename directory)))

(cl-defmethod @env-vars ((runfiles @directory) remote)
  "Implementation of ‘elisp/runfiles/env-vars’ for directory-based runfiles.
RUNFILES is a runfiles object, and REMOTE is the remote host
identifier."
  (let ((directory (@directory-directory runfiles)))
    (unless (equal (file-remote-p directory) remote)
      (signal 'elisp/runfiles/remote (list directory remote)))
    (let ((directory (file-name-unquote (file-local-name directory))))
      (list "RUNFILES_MANIFEST_FILE"
            (concat "RUNFILES_DIR=" directory)
            ;; TODO(laszlocsomor): remove JAVA_RUNFILES once the Java launcher
            ;; can pick up RUNFILES_DIR.
            (concat "JAVA_RUNFILES=" directory)))))


;;;; Repository mappings

(defun @parse-repo-mapping (file)
  "Parse and return repository mappings from FILE.
The return value is a hashtable mapping (CANONICAL . APPARENT)
pairs to the mapped repository; see URL
‘https://github.com/bazelbuild/proposals/blob/main/designs/2022-07-21-locating-runfiles-with-bzlmod.md#1-emit-a-repository-mapping-manifest-for-each-executable-target’.
If there’s no repository mapping file, the return value is nil."
  (declare (ftype (function (string) (or null string))))
  (cl-check-type file string)
  (with-temp-buffer
    (let ((coding-system-for-read 'us-ascii)
          (coding-system-for-write 'us-ascii)
          (format-alist nil)
          (after-insert-file-functions nil)
          (case-fold-search nil))
      (when (ignore-error file-missing (insert-file-contents file))
        (let ((table (make-hash-table :test #'equal :size 5)))
          (while (not (eobp))
            (unless (looking-at (rx bol
                                    (group (* (not (any ",\n")))) ?,
                                    (group (+ (not (any ",\n")))) ?,
                                    (group (+ (not (any ",\n"))))
                                    eol))
              (signal 'elisp/runfiles/syntax-error
                      (list file (line-number-at-pos))))
            (let ((canonical (match-string-no-properties 1))
                  (apparent (match-string-no-properties 2))
                  (mapping (match-string-no-properties 3)))
              (puthash (cons canonical apparent) mapping table))
            (forward-line))
          table)))))

(provide 'elisp/runfiles/runfiles)

;; Local Variables:
;; read-symbol-shorthands: (("@" . "elisp/runfiles/runfiles--"))
;; End:

;;; runfiles.el ends here
