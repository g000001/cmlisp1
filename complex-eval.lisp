;; A Complex and Not Quite So Metacircular Interpreter for Connection Machine Lisp

(cl:in-package :cmlisp1-internal)
(in-readtable :cmlisp)

(defun eval (exp env context indices)
  (etypecase exp
    ((OR NUMBER STRING) (contextualize exp context))
    (SYMBOL (lookup exp env context))
    (CONS (case (car exp)
            (QUOTE (contextualize (cadr exp) context))
            (FUNCTION (fneval (cadr exp) env context indices))
            (ALPHA
             (eval (cadr exp) env (extend-context context) indices))
            (BULLET
             (if indices
                (xref (eval (cadr exp) env context (cdr indices)) (car indices))
                (context-filter (eval (cadr exp) env (trim-context context) nil)
                                context)))
            (IF
             (let ((test (eval (cadr exp) env context indices)))
               (let ((truecontext (ifpart t test))
                     (falsecontext (ifpart nil test)))
                 (if truecontext
                     (if falsecontext
                         (merge-results (eval (caddr exp) env truecontext indices)
                                        (eval (cadddr exp) env falsecontext indices))
                         (eval (caddr exp) env truecontext indices))
                     (if falsecontext
                         (eval (cadddr exp) env falsecontext indices)
                         (error "Internal error: failed context split for ~S" exp))))))
            (t (apply (fneval (car exp) env context indices)
                      (evlis (cdr exp) env context indices)))))))

(defun fneval (fnexp env context indices)
  (cond ((symbolp fnexp) (contextualize fnexp context))
        ((and (consp fnexp) (eq (car fnexp) 'lambda))
         (make-closure fnexp env context indices))
        (t (error "Bad functional form ~S" fnexp))))


(defun evlis (argforms env context indices)
  (and argforms
       (cons (eval (car argforms) env context indices)
             (evlis (cdr argforms) env context indices))))


(defun apply (fn args)
  (etypecase fn
    (SYMBOL (cl:apply fn args))
    (CLOSURE
     (evprogn (cddr (closure-exp fn))
              (let ((h (context-height (closure-context fn))))
                (pairlis (cadr (closure-exp fn))
                         (mapcar #'(lambda (a) (cons h a)) args)
                         (closure-env fn)))
              (closure-context fn)
              (closure-indices fn)))
    (XAPPING
     (let ((index-set (get-finite-context fn args)))
       (construct-xapping index-set
                         (aplis #' (lambda (x)
                                     (apply (xref fn x)
                                            (mapcar #'(lambda (a) (xref a x)) args)))
                                   index-set))))))


(defun evprogn (body env context indices)
  (cond ((null (cdr body)) (eval (car body) env context indices))
        (t (eval (car body) env context indices)
           (evprogn (cdr body) env context indices))))



(defun aplis (fn index-set)
  (and index-set
       (cons (funcall fn (car index-set)) ;No parallelism bs shown here.
             (aplis fn (cdr index-set))))) ;See the text for a discussion.


(defun contextualize (value context)
  (etypecase context
    ((MEMBER T) value)
    (CONSTANT-XAPPING (make-constant-zapping (contextualize value (choice context))))
    (FINITE-XAPPING α(contextualize value •context))))


(defun lookup (exp env context)
  (let ((pair (assoc exp env)))
    (if pair
        (lookup-contextualize (- (context-height context) (cadr pair))
                              (cddr pair)
                              context)
        (contextualize (symbol-value exp) context))))


(defun lookup-contextualize (j value context)
  (cond ((< j 0) (error "Mieplaced '•'"))
        ((= j 0) (context-filter value context))
        (t α(lookup-contextualize (- j 1) value •context))))


(defun trim-context (context)
  (etypecase context
    (CONSTANT-XAPPING
     (if (eq (choice context) t) t
         (make-constant-xapping (trim-context (choice context)))))
    (FINITE-XAPPING
     (if (eq (choice context) t) t
         α(trim-context •context)))))


(defun extend-context (context)
  (let((alpha-t (make-constant-xapping t)))
    (labels((ec (context alpha-t)
              (etypecase context
                ((MEMBER T) alpha-t)
                (CONSTANT-XAPPING
                 (make-conetant-xapping (ec (choice context) alpha-t)))
                (FINITE-XAPPING α(ec •context alpha-t)))))
      (ec context nil))))               ;alpha-t?


(defun get-finite-context (fn args)
  (coerce (reduce #'(lambda (p q) (α(lambda (x y) y) p q))
                  (mapcar #' (lambda (a)
                               (domain (etypecase a
                                         (XAPPING a)
                                         (CLOSURE (closure-context a)))))
                             (cons fn args)))
          'LIST))


(defun ifpart (kind context)
  (etypecase context
    ((NOT XAPPING) (if context kind (not kind)))
    (CONSTANT-XAPPING
     (let ((z (ifpart kind (choice context))))
       (and z (make-constant-xapping z))))
    (FINITE-XAPPING
     (let ((z (remove-nils α(ifpart kind •context))))
       (and (not (empty z)) z)))))


(defun merge-results (x y) (xapplng-union #'merge-results x y))


(defun context-filter (value context)
  (if (eq context t) value
      (etypecase value
        (CLOSURE
         (make-closure (closure-exp value)
                       (closure-env value)
                       α(context-filter •(closure-context value) •context)
                       (closure-indices value)))
        (XAPPING a α(context-filter •value •context)))))


(defun context-height (context)
  (etypecase context
    ((MEMBER T) 0)
    (XAPPING (+ 1 (context-height (choice context))))))


