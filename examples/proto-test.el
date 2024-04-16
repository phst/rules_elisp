;;; proto-test.el --- unit tests for protocol buffer library  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022, 2024 Google LLC
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

;; Unit tests for example protocol buffer library.

;;; Code:

(require 'examples/example.proto)

(require 'elisp/proto/proto)

(require 'ert)

(ert-deftest examples/example_elisp_proto/person ()
  (let ((person (examples/Person-new :name "John" :age 40)))
    (should (examples/Person-p person))
    (should (elisp/proto/message-p person))
    (should (elisp/proto/message-mutable-p person))
    (should (equal (elisp/proto/field person 'name) "John"))
    (should (eql (elisp/proto/field person 'age) 40))
    (should (eql (setf (elisp/proto/field person 'age) 42) 42))
    (should-error (elisp/proto/field person 'doesnotexit)
                  :type 'elisp/proto/unknown-field)
    (should-error (setf (elisp/proto/field person 'doesnotexit) 123)
                  :type 'elisp/proto/unknown-field)
    (should-error (setf (elisp/proto/field person 'age) 'wrong-type)
                  :type 'wrong-type-argument)
    (pcase-exhaustive person
      ((elisp/proto examples/Person name age)
       (should (equal name "John"))
       (should (eql age 42))))
    (let ((serialized (elisp/proto/serialize person)))
      (should (stringp serialized))
      (should-not (multibyte-string-p serialized))
      (let ((parsed (elisp/proto/parse 'examples/Person serialized)))
        (should-not (eq parsed person))
        (should (equal (elisp/proto/field parsed 'name)
                       (elisp/proto/field person 'name)))))))

;;; proto-test.el ends here
