;;;; cmlisp.lisp
#|||
(cl:in-package :cmlisp1-internal)



(in-suite cmlisp1)

;; simple

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








(defun lookup (exp env)
(let ((pair ( ~ o c expenv)))
~ii pair (cddr Pair) (symbol-value exp))))
(defun Ineval (~neXp env indices)
(cond ((syabolp tnexp) fnexp)
((and (coasp fnexp) (eq (car lnexp) !ambda))
(sake-closure fnexp env indices))
(t (error ~Bad functional forl "8" lnexp))))
(de~unavlls ( a r ~ o r u env indices)
(and~oru
(cons (evil (car a r 8 f o r u ) e~v indices)
(evlls (cdr ar~forns) env indice~))~)
(d,efa- apply (in args)
(etypecase in
( S ~ O L (apply In arl~s))
(CLOGURE (evprogn (cddr (closure-lunction fn))
(pairlis (cadr (closura-tuAction fn))argo_ (closure-env fn))
(closure-indices fn)))
(XAPPING a (apply .in • ( l l s t - x a p p l n 8- transpose args ' {-~ () }) ) ) ) )
(defun evpro~n (body env indics.)
(cond ((null (cdr body)) (evil (car hod7) any indices))
(t (evil (car body) env indices)
(evprogn (cdr body) env indices))))
(de~u list-lapping-transpose (list-of-xappings xappins-of-lists)
(if (null list-of-xappings) xapping-of-liats
(list-xappins-transpoSe (cd~ llat-of-xapplngs)
a(cons .(car list-of-xappingp) *xapplng-of-liats))))




#||
(defun eval (exp env context indices)
(etypecaso exp
((OR IIUMBE~STRING) (contextualize exp context))
( S ~ O L (lookup exp env context))
(CONS (case (car exp)
(QUOTE (contextualtze (cadr exp) context))
(FUNCTION (fneval (cadr exp) env ¢ontvx~ l n a l c e 8 ) )
(ALPHA
(oval (cadr exp) env (extond-contoxt context) I n d i c e s ) )
( i f indices
(xref (eval (cadr exp) any context (cdr indices)) (car indices))
( c o n t e x t - f i l t e r (eval (cadr oxp) any ( t r t l - c o n t e x t context) n i l )
context)))
(rF
( l e t ( ( t o o t (eval (cadr exp) env context I n d i c e s ) ) )
(let ((truecontext (tfpart t test))
(falaecontext (ifpart nil test)))
(t~ truecontext
( i f falsecontext
(=ergo-results (eval (¢addr axp) any truecontext indices)
(eva1 (cadddr oxp) any falaecontext indices))
(oval (caddr exp) env truecontext i n d i c e s ) )
(if falsecontext
(oval (cadddr exp) Shy falsocontoxt tnd/cos)
( e r r o r " I n t e r n a l e r r o r : f a i l e d contoxt s p l i t f o r "an e x p ) ) ) ) ) )
( t (apply (fneval (car exp) env context i ~ l i c e a )
( o v l i s (cdr exp) env context t ~ C e S ) ) ) ) ) ) )
(defun fnoval (fnexp env context indices)
(cond ((syabolp fnexp) (contextualize fnexp context))
((and (consp fnexp) (eq (car fnexp) 'lanlxla))
(nke-closure fnexp env context indices))
( t ( e r r o r "Bad functional form "8" fnexp))))
(defun e v l t s (ar~foras env context indices)
(and argforas
(cons (oval (car argforas) env context i n d i c e s )
(evlts (cdr argforas) env context i n d i c e s ) ) ) )
(defun apply (fn args)
(etppecase fn
(SYMBOL (apply fn arks))
(CLOSURE
(evproKn (cddr (closure-oxp fn))
( l e t ((h (context-height (closuro-context f n ) ) ) )
( p a i r l i s (cadr (closure-exp fn))
(aapcar #' (laabda (a) (cons h a)) args)
(closure-env f n ) ) )
(closure-context fn)
(closure-ind$ces f n ) ) )
(lAPPING
( l e t ( ( i n d e x - s e t ( g e t - f i n i t e - c o n t e x t fn a r g s ) ) )
( c o u t r u c t - x a p p t n 8 index-set
(aplis #' (lanbda (x)
(apply (xref fn x)
(aai~:ar #'(laabda (a) (xref a x)) a r g s ) ) )
index-set))))))
Table 2. A Complex and Not Quite So Metacircular Interpreter for Connection Machine Lisp
291
(defun evprogn (body any context Indicee~
(cond ( ( n u l l (cdr body)) (eval (car body) env context i n d i c e s ) )
( t (eval (car body) env context indices)
( e v l ~ p "cdr body) env context i n d i c e s ) ) ) )
(defun a p l l e (fn I n d e x - s e t )
(and Index-set
(cons ( f u n c a l l fn (car I n d e x - s e t ) )
( a p l l e fn (cdr I n d e x - s e t ) ) ) ) )
;No parallelism bs shown here.
;See the text for a d~scuuion.
(defun ¢ontextnslize (value context)
(etypecue context
((MD4BEIt T) value)
(CONSTANT-XAPPING (aake-constant-zapping (contextualize value (choice c o n t e x t ) ) ) )
(FINITE-XAPPING a (contextualize value . c o n t e x t ) ) ) )
(defun lookup (exp env context)
( l e t ( ( p a i r (aseoc exp env)))
(If pair
(lookup-contextualize (* (context-height context) (cadr p a i r ) )
(cddr p a i r )
context)
( c o n t e x t u a l i z e ( e y a b o l - v a l ~ exp) c o n t e x t ) ) ) )
(defun lookup-contextualize (J value context)
(cond ((< J O) ( e r r o r mMieplaced ' * ' " ) )
((= J O) ( c o n t e x t - f i l t e r value context))
( t a(lookup-contexCnslize (- J l) value . c o n t e x t ) ) ) )
(defun t r i m - c o n t e x t (context)
(etypecaae context
(CONSTANT-XAPPING
( i f (eq (choice context) t ) t
(aake-constant-xapping ( t r i a - c o n t e x t (choice c o n t e x t ) ) ) ) )
(FINITE-XAPPING
( i f (eq (choice context) t ) t
a(tria-context .context)))))
(defun extend-context (context)
( l e t ( ( a l p h a - t (make-constant-xapping t ) ) )
( l a b e l s ((ec (context a l p h a - t )
(etypecaee context
((MIDiBER T) a l p h a - t )
(CONSTANT-XAPPING
(aake-conetant-xapping (ec (choice context) a l p h a - t ) ) )
(FINITE-XAPPING a(e¢ -context a l p h a - t ) ) ) ) )
(ec c o n t e x t ) ) ) )
(defun g e t - f i n i t e - c o n t e x t (fn arKe)
(coerce (reduce #' (laabda (p q) (a(laabda (x y) Y) P cA))
(aapcar #' (laabda (a)
(domain (etypecase a
(XAPPING a)
(CLOSURE ( c l o s u r e - c o n t e x t a ) ) ) ) )
(cons fn a r k s ) ) )
•LIST) )
Table 2 (continued).
292
(defun t f p a r t (kind c o n t e x t )
(etypecaae c o n t e x t
((NOT IAPPIIIG) ( i f c o n t e x t kind (not kind)))
(CONSTANT-YAPPINg
( l e t ((z ( i f p a r t kind (choice c o n t e x t ) ) ) )
(and z ( - a k e - c o n s t a n t - x a p p i n g z))))
(FINITE-ZAPPING
( l e t ( ( z ( r e n o v e - n i l s c~(tfpart kind . c o n t e x t ) ) ) )
(and (not (empty z)) z ) ) ) ) )
(def~ m e r g e - r e s u l t s (x y) (xapplng-unlon # ' m e r g e - r e s u l t s x y))
(defun c o n t e x t - f i l t e r (value c o n t e x t )
( i f (eq c o n t e x t t ) value
(etypecaee value
(CLOSURE
(aake-cloaure (cloeure-exp value)
( c l o s u r e - e n v value)
~ ( c o n t e x t - f i l t e r *(cloeure-context value) - c o n t e x t )
(closure-indices value)))
(XAPPING a ( c o n t e x t - f i l t e r .value . c o n t e x t ) ) ) ) )
(defun c o n t e x t - h e i g h t (context)
(etypecaee c o n t e x t
( ( ~
T) O)
(lAPPING (+ 1 (context-height (choice context))))))
||#
|||#