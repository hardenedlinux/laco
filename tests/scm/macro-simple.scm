(define-syntax mtest
  (syntax-rules ()
    ((_ x y) (+ x (+ x y) y))))

(display (mtest 1 2))
