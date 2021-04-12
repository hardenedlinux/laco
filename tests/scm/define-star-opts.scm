(define* (test x #:optional (y 2) (z 3))
  (display "x=") (display x) (newline)
  (display "y=") (display y) (newline)
  (display "z=") (display z) (newline))

(test 1) (newline)
(test 1 100) (newline)
(test 1 100 599)
