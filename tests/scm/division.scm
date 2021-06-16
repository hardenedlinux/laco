(define (div x y)
  (/ x y))
;; integer integer

(display (div -3 -2))
(newline)

(display (div -3 -1000000)) ;; underflow
(newline)

(display (div -500/3 -1/30000)) ;; overflow 5000000
(newline)

;; integer rational
(display (div 3000000 1/2)) ;; overflow 6000000
(newline)

;; rational integer
(display (div 1/2 30000))
(newline)

(display (div 1/2 32768)) ;; underflow
(newline)

;; rational rational
(display (div 1/2 1/2))
(newline)

(display (div 1/2 3/4))
(newline)

;; float float
(display (div 5.0 2.5))
(newline)
