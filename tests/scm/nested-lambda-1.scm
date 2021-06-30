(define (bar x)
  (foo))

(define (foo)
  (lambda (y)
    120))

(display ((bar 10) 20))
(newline)
