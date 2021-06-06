(let lp((x 100))
 (if (= x 0) #t (lp (- x 1))))
(display "done\n")
