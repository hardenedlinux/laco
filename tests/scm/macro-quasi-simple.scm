(define-syntax test
  (syntax-rules ()
    ((_ x (y z) a)
     `(,x (,y ,z) ,a))))

(display (test 1 (2 3) 4))
