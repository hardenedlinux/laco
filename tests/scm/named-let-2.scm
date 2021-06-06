(let lp((x 100))
 (display x)
 (if (= x 0) #t (lp (- x 1))))

