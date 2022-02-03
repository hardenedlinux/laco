(define (floor2 x)
  (define (floor_lp1 n)
    n)

  (define (floor_lp2 delta)
    (if (< delta 1)
        delta
        (floor_lp2 (/ delta 2))))

  (floor_lp2 4))

(display (floor2 10.2))
