(cl:in-package :cmlisp1-internal)

(defstruct
  (xapping (:print-function print-xapping)
           (:constructor xap
             (domain range &optional
              (default ':unknown defaultp)
              (infinite (and defaultp :constant))
              (exceptions '()))))
  domain
  range
  default
  (infinite nil :type (member nil :constant :universal))
  exceptions)