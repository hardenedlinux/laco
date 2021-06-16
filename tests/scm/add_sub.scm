(define (add x y)
  (+ x y))
(define (sub x y)
  (- x y))

(display (add 1 2))
(newline)

(display (add 3.3 40))
(newline)

(display (add 50 6.6))
(newline)

(display (add 7.0 8.0))
(newline)

(display (add 0.6666666 1/3))
(newline)

(display (add -5/3 0.6666666))
(newline)

(display (add 7/8 9/10))
(newline)

(display (add 1/3 12))
(newline)

(display (add 12 1/3))
(newline)

(display (add 2147483647 2147483647))
(newline)

(display (add 65534/65533 (add 2147483647 2147483647)))
(newline)

(display (sub 1 2/3))
(newline)

(display (sub 1 -2/3))
(newline)

(display (sub -2147483647 1))
(newline)

(display (sub -2147483647 2)) ;; overflow
(newline)

(display (sub -2147483647 2147483647)) ;; overflow
(newline)
