;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((indent-tabs-mode . nil)
         (fill-column . 80)))
 (c++-mode . ((page-delimiter . "^///")
              (mode . subword)))
 (c-mode . ((page-delimiter . "^///")
            (mode . subword)))
 ("elisp/proto/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../" "../../bazel-bin/" "../../bazel-bin/external/com_google_protobuf/")))))
 ("elisp/runfiles/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../../")))))
 ("examples/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../" "./" "ext/" "../bazel-bin/" "../bazel-bin/external/com_google_protobuf/")))))
 ("tests/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path "../")))))
 ("tests/pkg/" . ((emacs-lisp-mode . ((elisp-flymake-byte-compile-load-path . ("../../")))))))
