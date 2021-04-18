(define (func x)
  (lambda (y) (+ x y)))

(define z (func 123))

(display (z 5))
