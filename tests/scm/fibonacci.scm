; fibonacci.scm

(define (fib x)
    (if (<= x 2)
        1
        (+ (fib (- x 1))
           (fib (- x 2)))))

; (display "(fib 1) = ")
(display (fib 1))
(display "\n")

; (display "(fib 2) = ")
(display (fib 2))
(display "\n")

; (display "(fib 3) = ")
(display (fib 3))
(display "\n")

; (display "(fib 12) = ")
(display (fib 12))
(display "\n")