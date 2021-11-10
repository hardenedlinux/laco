(define (floor x)
  (define (floor_lp1 n)
    (if (> n x)
        n
        (floor_lp1 (* 2 n))))

  (define (floor_lp2 delta lower upper)
    (if (< delta 1)
        lower
        (if (> (- upper delta) x)
            (floor_lp2 (/ delta 2) lower (- upper delta))
            (floor_lp2 (/ delta 2) (+ lower delta) upper))))

  (let ((lim (floor_lp1 1)))
    (floor_lp2 (/ lim 4) (/ lim 2) lim)))

(display (floor 10.2))
