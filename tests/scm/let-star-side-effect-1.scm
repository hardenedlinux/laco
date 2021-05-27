(define (test)
  (let* ((x (display "xxx\n"))
         (y (display "yyy\n")))
    (display "Optimized with side-effects!\n")))

(test)
