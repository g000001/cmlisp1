(cl:in-package :cmlisp1-internal)


(defconstant bullet-char (name-char "BULLET"))
(defconstant alpha-char (name-char "GREEK_SMALL_LETTER_ALPHA"))
(defconstant rightwards-arrow-char (name-char "RIGHTWARDS_ARROW"))

#||
 (defstruct rightwards-arrow-char)
 (defmethod print-object ((obj rightwards-arrow-char) stream)
  (format stream "~A" rightwards-arrow-char))
||#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun open-bracket-macro-char (stream macro-char)
    (declare (ignore macro-char))
    (let ((range (read-delimited-list #\] stream t)))
      (xap (iota-list (length range)) range))))

(defun %reader-error (s mesg)
  (error mesg :stream s))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun open-brace-macro-char (s macro-char)
  (declare (ignore macro-char))
  (do ((ch (peek-char t s t nil t) (peek-char t s t nil t))
       (domain '())  (range '())  (exceptions '()))
      ((char= ch #\})
       (read-char s t nil t)
;       (construct-xapping (reverse domain) (reverse range))
       (xap (reverse domain) (reverse range)))
    (cond ((char= ch rightwards-arrow-char) ;→?
           (read-char s t nil t)
           (let ((nextch (peek-char nil s t nil t)))
             (cond ((char= nextch #\})
                    (read-char s t nil t)
                    (return (xap (reverse domain)
                                 (reverse range)
                                 nil :universal exceptions)))
                   (t (let ((item (read s t nil t)))
                        (cond ((char= (peek-char t s t nil t) #\})
                               (read-char s t nil t)
                               (return (xap (reverse domain)
                                            (reverse range)
                                            item :constant
                                            exceptions)))
                              (t (%reader-error s
                                   "Default → item must be last"))))))))
          (t (let ((item (read-preserving-whitespace s t nil t))
                   (nextch (peek-char nil s t nil t)))
               (cond ((char= nextch rightwards-arrow-char)
                      (read-char s t nil t)
                      (cond ((member (peek-char nil s t nil t)
                                     '(#\Space #\Tab #\Newline))
                             (push item exceptions))
                            (t (push item domain)
                               (push (read s t nil t) range))))
                     ((char= nextch #\})
                      (read-char s t nil t)
                      (push item domain)
                      (push item range)
                      (return (xap (reverse domain) (reverse range))))
                     (t (push item domain)
                        (push item range)))))))) )


