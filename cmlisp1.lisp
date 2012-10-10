;;;; cmlisp.lisp

(cl:in-package :cmlisp1-internal)
(in-readtable :cmlisp)

(defstruct (closure (:constructor make-closure (exp env context indices)))
  exp env context indices)

(defun xref (x k)
  (etypecase x
    (XAPPING (primitive-xref x k))
    (CLOSURE
     (make-closure
      (closure-exp x)
      (closure-env x)
      (primitive-xref (closure-context x) k)
      (cons k (closure-indices x))))))


;; simple eval
(defun eval (exp env indices)
  (etypecase exp
    ((OR NUMBER STRING) exp)
    (SYMBOL (lookup exp env))
    (CONS (case (car exp)
            (QUOTE (cadr exp))
            (FUNCTION (fneval (cadr exp) env indices))
            (IF (if (eval (cadr exp) env indices)
                    (eval (caddr exp) env indices)
                    (eval (cadddr exp) env indices)))
            (ALPHA  (eval α(cadr exp) env •(cons '{→} indices)))
            (BULLET
               (if indices
                   (xref (eval (cadr exp) env (cdr indices)) (car indices))
                   (error "Misplaced '•' before ~S" (cadr exp))))
            (t (apply (fneval (car exp) env indices)
                      (evlis (cdr exp) env indices)))))))
(eval '(αsqrt [1 2 3 4]) nil 0)
#|(eval '(funcall #'+ 1 3) () 1)|#

;; (type-of '{a b c})
;(eval '{a b c} () 0)

;;; 

(defun lookup (exp env)
  (let ((pair (assoc exp env)))
    (if pair (cddr pair) (symbol-value exp))))


(defun fneval (fnexp env indices)
  (cond ((symbolp fnexp) fnexp)
        ((and (consp fnexp) (eq (car fnexp) 'lambda))
         (make-closure fnexp env nil indices)) ;context?
        (t (error "Bad functional forms ~S" fnexp))))


(defun evlis (argforms env indices)
  (and argforms
       (cons (eval (car argforms) env indices) 
             (evlis (cdr argforms) env indices))))


(defun apply (fn args)
  (etypecase fn
    (SYMBOL (cl:apply fn args))
    (CLOSURE (evprogn (cddr (closure-function fn))
                      (pairlis (cadr (closure-function fn)) args (closure-env fn))
                      (closure-indices fn)))
    (XAPPING α(apply •fn •(list-xapping-transpose args '{→()})))))

(defun evprogn (body env indices)
  (cond ((null (cdr body)) (eval (car body) env indices)) 
        (t (eval (car body) env indices) 
           (evprogn (cdr body) env indices))))


(defun list-lapping-transpose (list-of-xappings xapping-of-lists)
  (if (null list-of-xappings) xapping-of-lists
      (list-xapping-transpose (cdr list-of-xappings)
                              α(cons •(car list-of-xappings) •xapping-of-lists))))

;;; 


