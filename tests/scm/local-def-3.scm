(define (f x)
  (define (b y)
    (+ x y))
  (define (c z)
    (* z z))
  (c (b 6)))

(display (f 7))
