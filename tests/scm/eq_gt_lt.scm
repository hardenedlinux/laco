;; eq_gt_lt.scm

(display (> 10 5))
(newline)
(display (> 10 5.0))
(newline)
(display (> 10 11/2))
(newline)

(display (> 5 10))
(newline)
(display (> 5.0 10))
(newline)
(display (> 11/2 10))
(newline)

(display (< 5 10))
(newline)
(display (< 5.0 10))
(newline)
(display (< 11/2 10))
(newline)

(display (< 10 5))
(newline)
(display (< 10 5.0))
(newline)
(display (< 10 11/2))
(newline)

(display (= 10/2 5))
(newline)
(display (= 5 5.0))
(newline)
(display (= 5 5))
(newline)

(display (= 7.0001 7))
(newline)

(display (= 7/2 3.5))
(newline)

(display (= 3.5 7/2))
(newline)

(display (= 7/2 3))
(newline)

(display (= 3 7/2))
(newline)

(define a 3)
(define b 3)
(define c 4)

(display (= a b))
(newline)

(display (= a c))
(newline)
