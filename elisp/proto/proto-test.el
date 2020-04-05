;;; proto-test.el --- unit tests for protocol buffer support  -*- lexical-binding: t; -*-

;; Copyright 2022 Google LLC
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

;; Unit tests for protocol buffer support (proto.el and module.c).

;;; Code:

(require 'elisp/proto/proto)

(require 'ert)
(require 'ert-x)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'any_proto)
(require 'descriptor_proto)
(require 'duration_proto)
(require 'test_messages_proto2_proto)
(require 'test_messages_proto3_proto)
(require 'timestamp_proto)
(require 'wrappers_proto)

(ert-deftest elisp/proto/make ()
  (let ((message (elisp/proto/make 'google/protobuf/Duration :seconds 333)))
    (should (google/protobuf/Duration-p message))
    (should (elisp/proto/message-p message))
    (should (elisp/proto/message-mutable-p message))
    (should (time-equal-p (elisp/proto/duration message) 333)))
  (let ((data (should-error (elisp/proto/make (intern "unknown/Message"))
                            :type 'wrong-type-argument)))
    (should (equal data '(wrong-type-argument elisp/proto/message-type-p
                                              "unknown.Message")))))

(ert-deftest elisp/proto/make/odd-number-of-args ()
  (should-error (elisp/proto/make 'google/protobuf/Duration :seconds)))

(ert-deftest elisp/proto/make/scalars ()
  (protobuf_test_messages/proto3/TestAllTypesProto3-new :optional_int32 1
                                                        :optional_int64 2
                                                        :optional_uint32 3
                                                        :optional_uint64 4
                                                        :optional_float 5
                                                        :optional_double 6
                                                        :optional_bool 7
                                                        :optional_string "8"
                                                        :optional_bytes "9"))

(ert-deftest elisp/proto/field ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :optional_uint64 123)))
    (should (eql (elisp/proto/field message 'optional_uint64) 123))
    (should (eql (setf (elisp/proto/field message 'optional_uint64) 456) 456))
    (should-not (elisp/proto/has-field message 'optional_nested_message))
    (should-not (elisp/proto/field message 'optional_nested_message))
    (let ((field (elisp/proto/mutable-field message 'optional_nested_message)))
      (should field)
      (should (eql (elisp/proto/field field 'a) 0)))
    (let ((field (gv-ref (elisp/proto/field message 'optional_nested_message))))
      (setf (gv-deref field)
            (protobuf_test_messages/proto3/TestAllTypesProto3/NestedMessage-new
             :a 123)))
    (should-error (elisp/proto/field message (intern ""))
                  :type 'wrong-type-argument)))

(ert-deftest elisp/proto/mutable-field ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new)))
    (should (elisp/proto/message-mutable-p message))
    (let ((field (elisp/proto/mutable-field message 'optional_nested_message)))
      (should field)
      (should (eql (elisp/proto/field field 'a) 0)))
    (let ((field (gv-ref (elisp/proto/field message 'optional_nested_message)))
          (value
           (protobuf_test_messages/proto3/TestAllTypesProto3/NestedMessage-new
            :a 123)))
      (should (eq (setf (gv-deref field) value) value)))
    (should-error (elisp/proto/mutable-field message 'optional_int32)
                  :type 'elisp/proto/atomic-field)
    (let ((array (elisp/proto/mutable-field message 'repeated_uint64)))
      (elisp/proto/append-array array 987654)
      (should (eq (elisp/proto/set-field message 'repeated_uint64 array) array))
      (should (eq (elisp/proto/set-field message 'repeated_uint32 array) array))
      (elisp/proto/set-field message 'repeated_fixed32 [2 3 4]))
    (let ((map (elisp/proto/mutable-field message 'map_string_bytes)))
      (should (equal (elisp/proto/map-put map "føo" "bar") "bar"))
      (should (eq (elisp/proto/set-field message 'map_string_bytes map) map))
      (should (eq (elisp/proto/set-field message 'map_string_string map) map))
      (should-error (elisp/proto/set-field message 'map_string_float map)))))

(ert-deftest elisp/proto/set-field/wrong-message-type ()
  (let* ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new))
         (duration (google/protobuf/Duration-new))
         (err (should-error
               (elisp/proto/set-field message 'optional_nested_message duration)
               :type 'wrong-type-argument)))
    (should
     (equal err
            `(wrong-type-argument
              protobuf_test_messages/proto3/TestAllTypesProto3/NestedMessage-p
              ,duration)))))

(ert-deftest elisp/proto/clear-field ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :optional_foreign_message
                  (protobuf_test_messages/proto3/ForeignMessage-new :c 25))))
    (should (elisp/proto/has-field message 'optional_foreign_message))
    (elisp/proto/clear-field message 'optional_foreign_message)
    (should-not (elisp/proto/has-field message 'optional_foreign_message))))

(ert-deftest elisp/proto/print-message ()
  (let ((message (elisp/proto/make-duration 3456)))
    (should (equal (cl-prin1-to-string message)
                   (concat "#<protocol buffer message google.protobuf.Duration "
                           "{ seconds: 3456 }>")))))

(ert-deftest elisp/proto/array ()
  (let* ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                   :repeated_int32 [1 2 3]))
         (array (elisp/proto/field message 'repeated_int32))
         (mutable-array (elisp/proto/mutable-field message 'repeated_int32)))
    (should (elisp/proto/array-p array))
    (should (elisp/proto/array-p mutable-array))
    (should-not (elisp/proto/array-mutable-p array))
    (should (elisp/proto/array-mutable-p mutable-array))
    (should (seqp array))
    (should (seqp mutable-array))
    (should-not (seq-empty-p array))
    (should-not (seq-empty-p mutable-array))
    (should (eql (seq-length array) 3))
    (should (eql (seq-elt array 1) 2))
    (should (eql (seq-elt mutable-array 1) 2))
    (should-error (setf (seq-elt array 2) 5) :type 'elisp/proto/immutable)
    (should (eql (setf (seq-elt mutable-array 2) 5) 5))
    (should (eql (seq-elt array 2) 5))
    (should-error (elisp/proto/append-array array 6)
                  :type 'elisp/proto/immutable)
    (elisp/proto/append-array mutable-array 6)
    (should-error (elisp/proto/append-array mutable-array "Foo")
                  :type 'wrong-type-argument)
    (should (eql (seq-length array) 4))
    (should (eql (seq-elt array 3) 6))
    (should (equal (seq-into-sequence array) [1 2 5 6]))
    (should (equal (elisp/proto/make-vector-from-array (seq-subseq array 1 3))
                   [2 5]))
    (should (equal (elisp/proto/make-vector-from-array (seq-subseq array 3))
                   [6]))
    (should (equal (elisp/proto/make-vector-from-array (seq-subseq array -4 -1))
                   [1 2 5]))
    (let ((err (should-error (seq-subseq array 5) :type 'args-out-of-range)))
      (should (equal err '(args-out-of-range 5 -4 4))))
    (let ((err (should-error (seq-subseq array -5) :type 'args-out-of-range)))
      (should (equal err '(args-out-of-range -5 -4 4))))
    (let ((err (should-error (seq-subseq array 1 5) :type 'args-out-of-range)))
      (should (equal err '(args-out-of-range 5 -4 4))))
    (let ((err (should-error (seq-subseq array 3 2) :type 'args-out-of-range)))
      (should (equal err '(args-out-of-range 2 3 4))))
    (should (equal (seq-into-sequence (seq-reverse array)) [6 5 2 1]))
    (should (eql (setf (seq-elt mutable-array 0) 2) 2))
    (should (equal (seq-into-sequence (seq-sort #'> array)) [6 5 2 2]))
    (should (equal (seq-map #'1+ array) '(3 3 6 7)))
    (should (equal (seq-into-sequence (seq-copy array)) [2 2 5 6]))
    (should (equal (cl-prin1-to-string array)
                   "#<protocol buffer array with 4 elements [2 2 5 6]>"))
    (let ((print-length 2))
      (should (equal (cl-prin1-to-string array)
                     "#<protocol buffer array with 4 elements [2 2...]>")))))

(ert-deftest elisp/proto/array-elt/message ()
  (let* ((message
          (protobuf_test_messages/proto3/TestAllTypesProto3-new
           :repeated_foreign_message
           (list (protobuf_test_messages/proto3/ForeignMessage-new :c 1))))
         (array (elisp/proto/field message 'repeated_foreign_message)))
    (should (eql (elisp/proto/field (seq-elt array 0) 'c) 1))
    (should-error (setf (elisp/proto/field (seq-elt array 0) 'c) 2)
                  :type 'elisp/proto/immutable)))

(ert-deftest elisp/proto/print-array ()
  (let* ((message
          (protobuf_test_messages/proto3/TestAllTypesProto3-new
           :repeated_foreign_message
           (list (protobuf_test_messages/proto3/ForeignMessage-new :c 1))))
         (array (elisp/proto/field message 'repeated_foreign_message)))
    (should (equal (cl-prin1-to-string array)
                   "#<protocol buffer array with 1 element [{ c: 1 }]>"))))

(ert-deftest elisp/proto/map ()
  (let* ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new))
         (mutable-map (elisp/proto/mutable-field message 'map_int32_float))
         (map (elisp/proto/field message 'map_int32_float)))
    (should (elisp/proto/map-p map))
    (should (elisp/proto/map-p mutable-map))
    (should-not (elisp/proto/map-mutable-p map))
    (should (elisp/proto/map-mutable-p mutable-map))
    (when (eval-when-compile (>= emacs-major-version 27))
      (should (mapp map))
      (should (mapp mutable-map))
      (should (map-empty-p map))
      (should (map-empty-p mutable-map))
      (should-error (map-put! map 123 4.5) :type 'map-not-inplace)
      (map-put! mutable-map 123 4.5)
      (should (eql (map-elt map 123) 4.5))
      (should (equal (map-apply (lambda (key value) (list (1+ key) value)) map)
                     '((124 4.5))))
      (should (equal (cl-prin1-to-string map)
                     "#<protocol buffer map with 1 entry [(123 4.5)]>"))
      (let ((copy (map-copy mutable-map)))
        (map-put! mutable-map 555 0)
        (should (mapp copy))
        (should (elisp/proto/map-mutable-p copy))
        (should (eql (map-length copy) 1)))
      (should (eq (map-delete mutable-map 555) mutable-map)))
    (should-error (elisp/proto/map-put map 234 -1.2)
                  :type 'elisp/proto/immutable)
    (should-not (elisp/proto/map-get map 234))
    (should (eq (elisp/proto/map-get map 234 'default) 'default))
    (should-not (elisp/proto/map-contains-key map 234))
    (elisp/proto/map-put mutable-map 234 -7)
    (should (eql (elisp/proto/map-get map 234) -7.0))
    (should (elisp/proto/map-contains-key map 234))
    (should (eql (setf (elisp/proto/map-get mutable-map 234) 4.5) 4.5))
    (elisp/proto/clear-map mutable-map)
    (should (eql (elisp/proto/map-length map) 0))))

(ert-deftest elisp/proto/timestamp ()
  (let* ((timestamp (elisp/proto/make-timestamp (encode-time 1 2 3 4 5 2022 t)))
         (time (elisp/proto/timestamp timestamp)))
    (should (equal (format-time-string "%F %T" time t)
                   "2022-05-04 03:02:01"))
    (should (eql (setf (elisp/proto/timestamp timestamp) 9999) 9999)))
  (should-error (elisp/proto/make-timestamp (encode-time 0 0 0 0 0 20000 t))
                :type 'args-out-of-range)
  (should-error (elisp/proto/make-timestamp (encode-time 0 0 0 0 0 -20000 t))
                :type 'args-out-of-range))

(ert-deftest elisp/proto/duration ()
  (let* ((duration (elisp/proto/make-duration 456))
         (time (elisp/proto/duration duration)))
    (should (equal (format-time-string "%s.%N" time t) "456.000000000"))
    (should (eql (setf (elisp/proto/duration duration) 9999) 9999)))
  (pcase (elisp/proto/make-duration -1.5)
    ((elisp/proto google/protobuf/Duration seconds nanos)
     (should (eql seconds -1))
     (should (eql nanos -500000000))))
  (should-error (elisp/proto/make-duration (encode-time 0 0 0 0 0 20000 t))
                :type 'args-out-of-range)
  (should-error (elisp/proto/make-duration (encode-time 0 0 0 0 0 -20000 t))
                :type 'args-out-of-range))

(ert-deftest elisp/proto/unknown-field ()
  (let ((duration (google/protobuf/Duration-new)))
    (should-error (elisp/proto/field duration 'unknown)
                  :type 'elisp/proto/unknown-field)))

(ert-deftest elisp/proto/no-presence ()
  (let ((duration (google/protobuf/Duration-new)))
    (should-error (elisp/proto/has-field duration 'seconds)
                  :type 'elisp/proto/no-presence)))

(ert-deftest elisp/proto/pcase ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :optional_int32 123)))
    (setf (elisp/proto/field
           (elisp/proto/mutable-field message 'optional_foreign_message) 'c)
          678)
    (pcase-exhaustive message
      ((elisp/proto protobuf_test_messages/proto3/TestAllTypesProto3
                    optional_int32 (optional_string str)
                    optional_nested_message
                    (optional_foreign_message
                     (elisp/proto protobuf_test_messages/proto3/ForeignMessage
                                  c)))
       (should (eql optional_int32 123))
       (should (equal str ""))
       (should-not optional_nested_message)
       (should (eql c 678))))))

(ert-deftest elisp/proto/pcase/defaults ()
  (pcase (protobuf_test_messages/proto3/TestAllTypesProto3-new)
    ((elisp/proto protobuf_test_messages/proto3/TestAllTypesProto3
                  (optional_int32 i32) (optional_int64 i64)
                  (optional_fixed32 u32) (optional_fixed64 u64)
                  (optional_float float) (optional_double double)
                  (optional_bool bool) (optional_string str)
                  (optional_bytes bytes))
     (should (eql i32 0))
     (should (eql i64 0))
     (should (eql u32 0))
     (should (eql u64 0))
     (should (eql float 0.0))
     (should (eql double 0.0))
     (should-not bool)
     (should (equal str ""))
     (should (equal bytes "")))))

(ert-deftest elisp/proto/pcase/unknown-field ()
  (let ((data (should-error
               (macroexpand
                '(pcase message
                   ((elisp/proto google/protobuf/Timestamp unknown-field))))
               :type 'elisp/proto/unknown-field)))
    (should (equal data '(elisp/proto/unknown-field
                          "google.protobuf.Timestamp"
                          unknown-field (seconds nanos))))))

(ert-deftest elisp/proto/make-unknown-field ()
  (ert-with-message-capture messages
    (let ((debug-on-error nil)
          (text-quoting-style 'curve))
      (cl-compiler-macroexpand '(elisp/proto/make 'google/protobuf/Timestamp
                                                  :seconds 123
                                                  :unknown 456)))
    (should (equal messages
                   (concat "Warning: Unknown field ‘unknown’ for "
                           "message type ‘google.protobuf.Timestamp’; "
                           "valid fields are ‘seconds’, ‘nanos’\n"))))
  (ert-with-message-capture messages
    (let ((debug-on-error nil)
          (text-quoting-style 'curve))
      (cl-compiler-macroexpand '(elisp/proto/make 'google/protobuf/Duration
                                                  'garbage)))
    (should (equal messages
                   "Warning: Invalid field keyword argument ‘garbage’\n")))
  ;; Only check if the type and field names are constant.
  (ert-with-message-capture messages
    (let ((debug-on-error nil)
          (text-quoting-style 'curve))
      (cl-compiler-macroexpand '(elisp/proto/make var :unknown 456))
      (cl-compiler-macroexpand '(elisp/proto/make 'google/protobuf/Timestamp
                                                  :seconds 123
                                                  var 456)))
    (should (string-empty-p messages))))

(ert-deftest elisp/proto/construct-unknown-field ()
  (ert-with-message-capture messages
    (let ((debug-on-error nil)
          (text-quoting-style 'curve))
      (cl-compiler-macroexpand '(google/protobuf/Timestamp-new :seconds 123
                                                               :unknown 456)))
    (should (equal messages (concat "Warning: Unknown field ‘unknown’ for "
                                    "message type ‘google.protobuf.Timestamp’; "
                                    "valid fields are ‘seconds’, ‘nanos’\n"))))
  ;; Only check if the field names are constant.
  (ert-with-message-capture messages
    (let ((debug-on-error nil)
          (text-quoting-style 'curve))
      (cl-compiler-macroexpand '(google/protobuf/Timestamp-new :seconds 123
                                                               var 456)))
    (should (string-empty-p messages))))

(ert-deftest elisp/proto/uint64 ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :optional_uint64 #xFFFFFFFFFFFFFFFF)))
    (should (eql (elisp/proto/field message 'optional_uint64)
                 #xFFFFFFFFFFFFFFFF))
    (let ((err (should-error
                (cl-incf (elisp/proto/field message 'optional_uint64))
                :type 'args-out-of-range)))
      (should (equal err '(args-out-of-range #x10000000000000000
                                             0 #xFFFFFFFFFFFFFFFF))))))

(ert-deftest elisp/proto/serialize-parse ()
  (let* ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                   :packed_int32 [1 2 3]))
         (serialized (elisp/proto/serialize message)))
    (should (stringp serialized))
    (should-not (multibyte-string-p serialized))
    (should-not (string-empty-p serialized))
    (let ((parsed (elisp/proto/parse
                   'protobuf_test_messages/proto3/TestAllTypesProto3
                   serialized)))
      (should (protobuf_test_messages/proto3/TestAllTypesProto3-p parsed))
      (let ((field (elisp/proto/mutable-field parsed 'packed_int32)))
        (should (equal (seq-into field 'vector) [1 2 3]))
        (should (eql (setf (seq-elt field 1) 77) 77))))
    ;; Check that parsing has created a new message.
    (pcase-exhaustive message
      ((elisp/proto protobuf_test_messages/proto3/TestAllTypesProto3
                    packed_int32)
       (should (equal (seq-into packed_int32 'vector) [1 2 3]))))))

(ert-deftest elisp/proto/parse/malformed ()
  (should-error (elisp/proto/parse 'google/protobuf/Duration "\xFF")
                :type 'elisp/proto/malformed))

(ert-deftest elisp/proto/parse/malformed-utf-8 ()
  (let ((serialized (elisp/proto/serialize
                     (google/protobuf/BytesValue-new :value "\xFF"))))
    (should-error (elisp/proto/parse 'google/protobuf/StringValue serialized)
                  :type 'elisp/proto/malformed-utf-8)))

(ert-deftest elisp/proto/parse/missing-required-field ()
  (let ((serialized (elisp/proto/serialize (google/protobuf/StringValue-new))))
    (should-error
     (elisp/proto/parse 'google/protobuf/UninterpretedOption/NamePart
                        serialized)
     :type 'elisp/proto/missing-required-field)))

(ert-deftest elisp/proto/serialize-text/small ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :packed_int32 [1 2 3])))
    (should (equal (elisp/proto/serialize-text message :deterministic t)
                   (concat "packed_int32: 1\n"
                           "packed_int32: 2\n"
                           "packed_int32: 3\n")))
    (should (equal (elisp/proto/serialize-text message :compact t)
                   (concat "packed_int32: 1 "
                           "packed_int32: 2 "
                           "packed_int32: 3 ")))
    (should-error (elisp/proto/serialize-text message 'garbage))
    (should-error (elisp/proto/serialize-text message :unknown t)
                  :type 'elisp/proto/wrong-choice)
    (should-error (elisp/proto/serialize-text message :compact t :compact nil)
                  :type 'elisp/proto/duplicate-key)))

(ert-deftest elisp/proto/serialize-text/large ()
  ;; Make the message large enough to trigger reallocation in
  ;; SerializeMessageText.
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :packed_int32 (make-vector #x2000 1))))
    (should (equal (elisp/proto/serialize-text message)
                   (string-join (make-vector #x2000 "packed_int32: 1\n"))))))

(ert-deftest elisp/proto/parse-json ()
  (pcase (elisp/proto/parse-json
          'protobuf_test_messages/proto3/TestAllTypesProto3
          "{\"packedInt32\":[1,2,3]}")
    ((elisp/proto protobuf_test_messages/proto3/TestAllTypesProto3 packed_int32)
     (should (equal (seq-into packed_int32 'vector) [1 2 3])))
    (otherwise (ert-fail otherwise)))
  (pcase (elisp/proto/parse-json
          'protobuf_test_messages/proto3/TestAllTypesProto3
          "{\"packedInt32\":[1,2,3],\"unknown\":8765}"
          :discard-unknown t)
    ((elisp/proto protobuf_test_messages/proto3/TestAllTypesProto3 packed_int32)
     (should (equal (seq-into packed_int32 'vector) [1 2 3])))
    (otherwise (ert-fail otherwise)))
  (should-error
   (elisp/proto/parse-json 'protobuf_test_messages/proto3/TestAllTypesProto3
                           "{\"packedInt32\":[1,2,3],\"unknown\":8765}")
   :type 'elisp/proto/json-parse-error))

(ert-deftest elisp/proto/serialize-json/small ()
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :packed_int32 [1 2 3])))
    (should (equal (elisp/proto/serialize-json message)
                   "{\"packedInt32\":[1,2,3]}"))))

(ert-deftest elisp/proto/serialize-json/large ()
  ;; Make the message large enough to trigger reallocation in
  ;; SerializeMessageJson.
  (let ((message (protobuf_test_messages/proto3/TestAllTypesProto3-new
                  :packed_int32 (make-vector #x2000 1))))
    (should (equal (elisp/proto/serialize-json message)
                   (concat "{\"packedInt32\":["
                           (string-join (make-vector #x2000 "1") ",")
                           "]}")))))

(ert-deftest elisp/proto/any ()
  (let* ((message (elisp/proto/make-duration 123))
         (any (elisp/proto/pack-any message)))
    (should (google/protobuf/Any-p any))
    (should (equal (elisp/proto/field any 'type_url)
                   "type.googleapis.com/google.protobuf.Duration"))
    (let ((unpacked (elisp/proto/unpack-any any)))
      (should (google/protobuf/Duration-p unpacked))
      (should (time-equal-p (elisp/proto/duration unpacked) 123))))
  (should-error (elisp/proto/unpack-any (google/protobuf/Any-new))
                :type 'elisp/proto/uninitialized-any)
  (dolist (url '(nil "" "/" "a" "/a" "a/" "/abcdef" "abcdef/" "abcdef"))
    (ert-info (url :prefix "Type URL: ")
      (should-error (elisp/proto/unpack-any
                     (google/protobuf/Any-new :type_url url
                                              :value "garbage"))
                    :type 'wrong-type-argument))))

(ert-deftest elisp/proto/parse-file-descriptor-set ()
  (let* ((field (google/protobuf/FieldDescriptorProto-new
                 :type google/protobuf/FieldDescriptorProto/TYPE_INT64
                 :name "field"
                 :json_name "field"
                 :number 3
                 :label google/protobuf/FieldDescriptorProto/LABEL_REPEATED))
         (message (google/protobuf/DescriptorProto-new :name "Message"
                                                       :field (list field)))
         (value (google/protobuf/EnumValueDescriptorProto-new :name "VALUE"
                                                              :number 77))
         (enum (google/protobuf/EnumDescriptorProto-new :name "Enum"
                                                        :value (list value)))
         (file (google/protobuf/FileDescriptorProto-new
                :name "test.proto"
                :package "test"
                :message_type (list message)
                :enum_type (list enum)))
         (set (google/protobuf/FileDescriptorSet-new :file (list file)))
         (serialized (elisp/proto/serialize set)))
    (should (equal (elisp/proto/parse-file-descriptor-set serialized)
                   '(("test.proto")
                     (("test.Message" field))
                     (("test.Enum" (VALUE 77)))))))
  (should-error (elisp/proto/parse-file-descriptor-set "garbage")))

;;; proto-test.el ends here
