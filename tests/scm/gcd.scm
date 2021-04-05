(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(display (gcd 13 39))(newline)
(display (gcd -1 -2))(newline)
(display (gcd -5 35))(newline)
(display (gcd 5 -35))
