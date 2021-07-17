(define (main k)
  (let lp ((n 1))
    (display "k = ")
    (display k)
    (display ", n = ")
    (display n)
    (newline)
    (if (= n 10)
        #t
        (lp (+ 1 n)))))

(main 12)
