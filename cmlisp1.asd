;;;; cmlisp.asd

(cl:in-package :asdf)

(defsystem :cmlisp1
  :serial t
  :components ((:file "package")
               (:file "xapping")
               (:file "reader")
               (:file "printer")
               (:file "cmlisp1")))

(defmethod perform ((o test-op) (c (eql (find-system :cmlisp1))))
  (load-system :cmlisp1)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :cmlisp1-internal :cmlisp1))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

