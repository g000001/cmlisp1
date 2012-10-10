;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :cmlisp1
  (:use)
  (:export :eval :apply))

(defpackage :cmlisp1-internal
  (:use :cmlisp1 :cl :fiveam :named-readtables)
  (:shadowing-import-from :cmlisp1 
                          :eval
                          :apply))



(in-package :cmlisp1-internal)
(def-suite cmlisp1)
