;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :cmlisp1
  (:use)
  (:export))

(defpackage :cmlisp1-internal
  (:use :cmlisp1 :cl :fiveam))

(in-package :cmlisp1-internal)
(def-suite cmlisp1)