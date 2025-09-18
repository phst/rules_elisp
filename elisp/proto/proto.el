;;; proto.el --- basic protocol buffer functionality  -*- lexical-binding: t; -*-

;; Copyright 2020-2023, 2025 Google LLC
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

;; Provides basic support for protocol buffers.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'elisp/proto/module)

(cl-defstruct (elisp/proto/object
               (:conc-name @object-)
               (:constructor nil)
               (:copier nil)
               (:predicate nil)
               :noinline)
  "Internal base type for protocol buffer objects.
This type is internal and should not be used directly."
  (arena nil :type user-ptr :read-only t)
  (ptr nil :type user-ptr :read-only t))

(cl-defstruct (elisp/proto/message
               (:conc-name @message-)
               (:constructor nil)
               (:copier nil)
               (:include elisp/proto/object)
               :noinline)
  "Base type for generated protocol buffer messages.
The fields are internal and should not be accessed directly.")

(gv-define-simple-setter elisp/proto/field elisp/proto/set-field)

(cl-define-compiler-macro elisp/proto/make (&whole form type &rest keys)
  (if (macroexp-const-p type)
      (@check-keys form (eval type t) keys)
    form))

(defun @check-keys (form type keys)
  "Check whether KEYS are valid keyword arguments to initialize TYPE.
TYPE should be a message structure type symbol; KEYS are the unevaluated
keyword arguments.  Make a best-effort attempt to print a warning if any
keyword in KEYS doesn’t refer to a field in TYPE.  In any case, return
FORM.  This internal function is meant to be used in compiler macros."
  (declare (ftype (function (t symbol t) t)))
  ;; Emacs turns errors in compiler macros into messages, see
  ;; ‘macroexp--compiler-macro’.  Print a compiler warning instead.
  (if-let ((message (@keys-message type keys)))
      (macroexp-warn-and-return message form 'callargs)
    form))

(defun @keys-message (type keys)
  "Return a warning message if KEYS are not valid to initialize TYPE.
TYPE should be a message structure type symbol; KEYS are the unevaluated
keyword arguments.  If KEYS are valid keyword-value pairs for TYPE, or
if their validity can’t be determined statically, return nil."
  (declare (ftype (function (symbol t) (or null string)))
           (side-effect-free t))
  (condition-case-unless-debug err
      (cl-loop for (key . _) on keys by #'cddr
               when (macroexp-const-p key)
               do (elisp/proto/check-field-keyword type (eval key t)))
    (elisp/proto/unknown-field
     (cl-destructuring-bind (_symbol message-name field allowed-fields) err
       (concat (format-message "Unknown field ‘%s’ for message type ‘%s’; "
                               field message-name)
               (if allowed-fields
                   (format-message "valid fields are %s"
                                   (mapconcat (lambda (s) (format "‘%s’" s))
                                              allowed-fields ", "))
                 "the message type has no fields"))))
    (wrong-type-argument
     (pcase err
       (`(,_ elisp/proto/field-keyword-p ,value)
        (format-message "Invalid field keyword argument ‘%s’" value))
       (_ (error-message-string err))))))

(pcase-defmacro elisp/proto (type &rest fields)
  "Extract FIELDS from a protocol buffer message of type TYPE.
TYPE must be the name of a generated protocol buffer structure type,
i.e., a type that is derived from ‘elisp/proto/message’.  Each field in
FIELDS is a list (NAME PATTERN).  It can also be a plain symbol FIELD,
which is a shorthand for (FIELD FIELD).  NAME names a field in the
protocol buffer message type TYPE.  The value of the field is matched
against PATTERN."
  (cl-check-type type (and symbol (not null) (not keyword)))
  (cl-with-gensyms (message)
    (let ((pairs (mapcar (lambda (field)
                           (pcase-exhaustive field
                             (`(,name ,pattern) `(,name . ,pattern))
                             (name `(,name . ,name))))
                         fields)))
      (pcase-dolist (`(,name . ,_) pairs)
        (elisp/proto/check-field-name type name))
      `(and (pred ,(intern (format "%s-p" type)))
            ,message
            ,@(mapcar (pcase-lambda (`(,name . ,pattern))
                        `(let ,pattern (elisp/proto/field ,message ',name)))
                      pairs)))))

(cl-defmethod cl-print-object ((message elisp/proto/message) stream)
  "Print protocol buffer MESSAGE to STREAM.
MESSAGE must be a protocol buffer message object, i.e., its type must be
a subtype of ‘elisp/proto/message’.  STREAM must be an output stream as
defined in the Info node ‘(elisp) Output Streams’."
  (elisp/proto/print-message message stream))

(cl-defstruct (elisp/proto/array
               (:conc-name @array-)
               (:constructor nil)
               (:constructor @array-new (arena ptr))
               (:copier nil)
               (:include elisp/proto/object)
               :noinline)
  "Wraps a protocol buffer message array.
Such an array is typically the result of obtaining the value of a
repeated field using ‘elisp/proto/field’ or ‘elisp/proto/mutable-field’.
Arrays returned by ‘elisp/proto/field’ are immutable; arrays returned by
‘elisp/proto/mutable-field’ are mutable.  The ‘elisp/proto/array’ type
implements the generalized sequence type defined by the ‘seq.el’
library; see the Info node ‘(elisp) Sequence Functions’.  The fields are
internal and should not be accessed directly.")

(cl-defmethod cl-print-object ((array elisp/proto/array) stream)
  "Print protocol buffer ARRAY to STREAM.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.
STREAM must be an output stream as defined in the Info node ‘(elisp)
Output Streams’."
  (elisp/proto/print-array array stream))

(gv-define-simple-setter elisp/proto/array-elt elisp/proto/set-array-elt)

(cl-defmethod seq-elt ((array elisp/proto/array) index)
  "Return the element at INDEX in the protocol buffer ARRAY.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’."
  (elisp/proto/array-elt array index))

(cl-defmethod (setf seq-elt) (value (array elisp/proto/array) index)
  "Set the element at INDEX in the protocol buffer ARRAY to VALUE.
ARRAY must be a mutable protocol buffer array of type
‘elisp/proto/array’.  Return VALUE."
  (elisp/proto/set-array-elt array index value))

(cl-defmethod seq-length ((array elisp/proto/array))
  "Return the number of elements in the protocol buffer ARRAY.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’."
  (elisp/proto/array-length array))

(cl-defmethod seq-do (function (array elisp/proto/array))
  "Call FUNCTION for each element in ARRAY.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’."
  (elisp/proto/do-array function array))

(cl-defmethod seqp ((_ elisp/proto/array))
  "Return t."
  t)

(cl-defmethod seq-copy ((array elisp/proto/array))
  "Return a shallow copy of ARRAY.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.
The return value is mutable."
  (elisp/proto/copy-array array))

(cl-defmethod seq-subseq ((array elisp/proto/array) start &optional end)
  "Return a shallow copy of a subarray of ARRAY from START to END.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.  The
return value is mutable."
  (elisp/proto/subarray array start end))

(cl-defmethod seq-remove-at-position ((array elisp/proto/array) n)
  "Return a copy of ARRAY with the element at index N removed.
The return value is mutable."
  (let ((copy (elisp/proto/copy-array array)))
    (elisp/proto/array-pop copy n)
    copy))

(cl-defmethod seq-sort (pred (array elisp/proto/array))
  "Return a sorted shallow copy of the protocol buffer ARRAY in place.
Compare values with PRED.  ARRAY must be a protocol buffer array of type
‘elisp/proto/array’."
  (let ((copy (elisp/proto/copy-array array)))
    (elisp/proto/sort-array copy pred)
    copy))

(cl-defmethod seq-reverse ((array elisp/proto/array))
  "Return a reversed shallow copy of the protocol buffer ARRAY.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’."
  (let ((copy (elisp/proto/copy-array array)))
    (elisp/proto/nreverse-array copy)
    copy))

(cl-defmethod seq-into-sequence ((array elisp/proto/array))
  "Return a shallow copy of ARRAY as a vector.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’."
  (elisp/proto/make-vector-from-array array))

(cl-defmethod seq-into ((array elisp/proto/array)
                        (_type (eql elisp/proto/array)))
  "Return ARRAY."
  array)

(cl-defmethod seq-into ((array elisp/proto/array) type)
  "Return a shallow copy of ARRAY as TYPE.
ARRAY must be a protocol buffer array of type ‘elisp/proto/array’.  TYPE
specifies the return type, one of ‘vector’, ‘string’, or ‘list’."
  (seq-into (elisp/proto/make-vector-from-array array) type))

;; Note that we don’t specialize ‘seq-into’ for a TYPE of ‘elisp/proto/array’
;; since it’s not possible to create protocol buffer arrays from scratch.  For
;; the same reason, we also don’t specialize ‘seq-concatenate’.

(cl-defstruct (elisp/proto/map
               (:conc-name @map-)
               (:constructor nil)
               (:constructor @map-new (arena ptr))
               (:copier nil)
               (:include elisp/proto/object)
               :noinline)
  "Wraps a protocol buffer message array.
Such an array is typically the result of obtaining the value of a map
field using ‘elisp/proto/field’ or ‘elisp/proto/mutable-field’.  Maps
returned by ‘elisp/proto/field’ are immutable; maps returned by
‘elisp/proto/mutable-field’ are mutable.  The ‘elisp/proto/map’ type
implements the generalized map type defined by the ‘map.el’ library.
The fields are internal and should not be accessed directly.")

(cl-defmethod cl-print-object ((map elisp/proto/map) stream)
  "Print protocol buffer MAP to STREAM.
ARRAY must be a protocol buffer map of type ‘elisp/proto/map’.  STREAM
must be an output stream as defined in the Info node ‘(elisp) Output
Streams’."
  (elisp/proto/print-map map stream))

(gv-define-simple-setter elisp/proto/map-get elisp/proto/map-put)

(cl-defmethod map-elt ((map elisp/proto/map) key &optional default)
  "Return the value mapped to KEY in the protocol buffer MAP.
MAP must be a protocol buffer map of type ‘elisp/proto/map’.  Return
DEFAULT if KEY isn't present in MAP."
  (elisp/proto/map-get map key default))

(cl-defmethod map-delete ((map elisp/proto/map) key)
  "Attempt to remove KEY from the protocol buffer MAP.
MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’."
  (elisp/proto/map-delete map key)
  map)

(cl-defmethod map-length ((map elisp/proto/map))
  "Return the number of elements in the protocol buffer MAP.
MAP must be a protocol buffer map of type ‘elisp/proto/map’."
  (elisp/proto/map-length map))

(cl-defmethod map-copy ((map elisp/proto/map))
  "Return a shallow copy of MAP.
MAP must be a protocol buffer map of type ‘elisp/proto/map’.  The return
value is mutable."
  (elisp/proto/copy-map map))

(cl-defmethod map-into ((map elisp/proto/map) (_type (eql elisp/proto/map)))
  "Return MAP."
  map)

(cl-defmethod map-do (function (map elisp/proto/map))
  "Call FUNCTION for each element in MAP.
MAP must be a protocol buffer array of type ‘elisp/proto/map’.  Call
FUNCTION with two arguments, the key and the value."
  (elisp/proto/do-map function map))

(cl-defmethod mapp ((_ elisp/proto/map))
  "Return t."
  t)

(cl-defmethod map-contains-key ((map elisp/proto/map) key)
  "Return whether the protocol buffer MAP contains KEY.
MAP must be a protocol buffer map of type ‘elisp/proto/map’."
  (elisp/proto/map-contains-key map key))

(cl-defmethod map-put! ((map elisp/proto/map) key value)
  "Insert a VALUE with a KEY into the protocol buffer MAP.
MAP must be a mutable protocol buffer map of type ‘elisp/proto/map’.  If
an entry with KEY is already present in MAP, overwrite it.  Return
VALUE."
  (condition-case nil
      (elisp/proto/map-put map key value)
    (elisp/proto/immutable (signal 'map-not-inplace (list map)))))

(gv-define-simple-setter elisp/proto/timestamp elisp/proto/set-timestamp)
(gv-define-simple-setter elisp/proto/duration elisp/proto/set-duration)

(provide 'elisp/proto/proto)

;; Local Variables:
;; read-symbol-shorthands: (("@" . "elisp/proto/proto--"))
;; End:

;;; proto.el ends here
