(defpackage cl-cycle
  (:use #:common-lisp #:iter)
  (:import-from #:bind #:bind))
(in-package #:cl-cycle)
(trivial-package-local-nicknames:add-package-local-nickname
 '#:sera '#:serapeum)
(trivial-package-local-nicknames:add-package-local-nickname
 '#:alex '#:alexandria)
