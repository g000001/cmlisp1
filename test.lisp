(cl:in-package :cmlisp1-internal)
(in-readtable :cmlisp)


#|(test read-xector
  (let ((xector (read-from-string "[A B C]")))
    ;; FIXME xector= が必要
    (is (string= (write-to-string xector)
                 "[A B C]"))
    (is (equal (xapping-domain xector)
               '(0 1 2)))))|#

#|(test read-xet
  (let ((xet (read-from-string "{A B C}"))
        (xet2 (read-from-string "{a→a b→b c→c}")))
    ;; FIXME xet= が必要
    (is (string= (write-to-string xet) "{A B C}"))
    (is (string= (write-to-string xet2) "{A B C}"))))|#


(αcons '{a→1 b→2 c→3 d→4 e→5}
        '{b→6 d→7 e→8 f→9})


;=> 
'{b→(2 . 6) d→(4 . 7) f→(5 . 9)}
;=>  {B→(2 . 6) D→(4 . 7) F→(5 . 9)}


