(cl:in-package :cmlisp1-internal)

(defun iota-list (n)     ;Return list of integers from 0 to n-1
  (do ((j (- n 1) (- j 1))
       (z '() (cons j z)))
      ((< j 0) z)))

(defun iota-list-p (list)
  (and (null list) (return-from iota-list-p nil))
  (do* ((len (length list))
        (i 0 (1+ i))
        (tail list (cdr tail)))
      ((>= i len) t)
    (typecase (car tail)
      ((integer 0 *)
         (when (/= i (car tail))
           (return nil)))
      (otherwise (return nil)))))

(test iota-list
  (is-true (iota-list-p (mapcar #'1+ (cons -1 (iota-list 8))))))

(defun xectorp (obj)
  (typep (xapping-domain obj) '(satisfies iota-list-p)))

(defun xetp (obj)
  (equal (xapping-domain obj)
         (xapping-range obj)))

(defun finite-part-is-xetp (obj)
  (xetp obj))

(defun print-xapping (xapping stream depth)
  (declare (ignore depth))
  (format stream
          ;; Are you ready for this one?
          "~:[{~;[~]~:{~S~:[→~S~;~*~]~:^ ~}~:[~; ~]~
           ~{~S→~^ ~}~:[~; ~]~[~*~;→~S~;→~*~]~:[}~;]~]"
          ;; Is that clear?
          (xectorp xapping)
          (do ((vp (xectorp xapping))
               (sp (finite-part-is-xetp xapping))
               (d (xapping-domain xapping) (cdr d))
               (r (xapping-range xapping) (cdr r))
               (z '() (cons (list (if vp (car r) (car d))
                                  (or vp sp)
                                  (car r))
                            z)))
              ((null d) (reverse z)))
          (and (xapping-domain xapping)
               (or (xapping-exceptions xapping)
                   (xapping-infinite xapping)))
          (xapping-exceptions xapping)
          (and (xapping-exceptions xapping)
               (xapping-infinite xapping))
          (ecase (xapping-infinite xapping)
            ((nil) 0)
            (:constant 1)
            (:universal 2))
          (xapping-default xapping)
          (xectorp xapping)))

;; printer
(test print-xapping
  (is (string= (write-to-string (xap '(foo bar baz) '(a b c)))
               "{FOO→A BAR→B BAZ→C}")))

(test print-xector
  (is (string= (write-to-string (xap '(0 1 2) '(a b c)))
               "[A B C]")))

(test print-xet
  (is (string= (write-to-string (xap '(foo bar baz) '(foo bar baz)))
               "{FOO BAR BAZ}"))
  (is (string= (write-to-string
                (xap '(foo bar baz) '(a b c) 'default :constant '(qux quux)))
               "{FOO→A BAR→B BAZ→C QUX→ QUUX→ →DEFAULT}")))
