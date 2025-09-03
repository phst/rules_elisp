;; Copyright 2021-2023, 2025 Google LLC
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

;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((indent-tabs-mode . nil)
         (fill-column . 80)))
 (c++-mode . ((page-delimiter . "^///")
              (mode . subword)))
 (c-mode . ((page-delimiter . "^///")
            (mode . subword)))
 (js-json-mode . ((js-indent-level . 2)))
 ("elisp/proto/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../" "../../bazel-bin/" "../../bazel-bin/external/protobuf+/src/")))))
 ("elisp/runfiles/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../")))))
 ("examples/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../" "./" "ext/" "../bazel-bin/" "../bazel-bin/external/protobuf+/src/")))))
 ("tests/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../")))))
 ("tests/pkg/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../")))))
 ("tests/proto/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../" "../../bazel-bin/" "../../bazel-bin/external/protobuf+/src/")))))
 ("tests/runfiles/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../"))))))
