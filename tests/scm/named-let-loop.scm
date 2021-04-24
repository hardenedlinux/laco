(let lp((n 1))
  (display n)
  (newline)
  (if (= n 100)
      #t
      (lp (+ 1 n))))
