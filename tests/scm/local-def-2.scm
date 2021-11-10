(define (f x)
  (define (b y)
    (+ x y))
  (b 6))

(display (f 7))
