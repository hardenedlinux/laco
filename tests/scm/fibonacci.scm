(define (factorial x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

(display (factorial 5))
