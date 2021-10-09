;; numbers.c

(display (floor -4.3)) ;; => -5.0
(newline)
(display (floor -4)) ;; => -4
(newline)
(display (floor 4)) ;; => 4
(newline)
(display (floor 3.5)) ;; => 3.0
(newline)

(display (ceiling -4.3)) ;; => -4.0
(newline)
(display (ceiling -4)) ;; => -4
(newline)
(display (ceiling 4)) ;; => 4
(newline)
(display (ceiling 3.5)) ;; => 4.0
(newline)

(display (truncate -4.3)) ;; => -4.0
(newline)
(display (truncate -4)) ;; => -4
(newline)
(display (truncate 3.5)) ;; => 3.0
(newline)
(display (truncate 3)) ;; => 3.0
(newline)

(display (round -4.3)) ;; => -4.0
(newline)
(display (round 3.5)) ;; => 4.0 ;; inexact
(newline)
(display (round 7/2)) ;; => 4 ;; exact
(newline)
(display (round 7)) ;; => 7
(newline)

(display (round 4.3)) ;; => 4.0
(newline)
(display (round -3.5)) ;; => -4.0 ;; inexact
(newline)
(display (round -7/2)) ;; => -4 ;; exact
(newline)
(display (round -7)) ;; => -7
(newline)

(display (zero? 0))
(newline)
(display (zero? 0.0))
(newline)
(display (zero? -0.0))
(newline)
(display (zero? 0/1))
(newline)
(display (zero? -0/1))
(newline)

(display (positive? 1))
(newline)
(display (positive? 1.0))
(newline)
(display (positive? 0.0))
(newline)
(display (positive? 0))
(newline)
(display (positive? -1))
(newline)
(display (positive? -1.0))
(newline)

(display (negative? 1))
(newline)
(display (negative? 1.0))
(newline)
(display (negative? 0.0))
(newline)
(display (negative? 0))
(newline)
(display (negative? -1))
(newline)
(display (negative? -1.0))
(newline)

(display (odd? -2.0))
(newline)
(display (odd? 2.0))
(newline)
(display (odd? -1.0))
(newline)
(display (odd? 1.0))
(newline)
(display (odd? 0.0))
(newline)
(display (odd? -0.0))
(newline)

(display (odd? -2))
(newline)
(display (odd? 2))
(newline)
(display (odd? -1))
(newline)
(display (odd? 1))
(newline)
(display (odd? 0))
(newline)

(display (even? -2.0))
(newline)
(display (even? 2.0))
(newline)
(display (even? -1.0))
(newline)
(display (even? 1.0))
(newline)
(display (even? 0.0))
(newline)
(display (even? -0.0))
(newline)

(display (even? -2))
(newline)
(display (even? 2))
(newline)
(display (even? -1))
(newline)
(display (even? 1))
(newline)
(display (even? 0))
(newline)
