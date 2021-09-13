(define (mul x y)
  (* x y))

(display (mul -65535 -65535))
(newline)

(display (mul 2147483647 -2147483648))
(newline)

(display (mul -2147483648 2147483647))
(newline)

(display (mul -2147483648 -2147483648))
(newline)

(display (mul -1/3 -3))
(newline)

(display (mul -3 -1/3))
(newline)

;; rational rational
(display (mul -3/4 -1/3))
(newline)

;; float integer
(display (mul -0.5 -2))
(newline)

;; integer float
(display (mul -2 -0.5))
(newline)

;; float rational
(display (mul -3.0 -1/3))
(newline)

;; rational float
(display (mul -1/3 -3.0))
(newline)

;; float float
(display (mul 0.5 2.0))
(newline)
