(define* (test x #:key (y 2) (z 3))
  (display "x=") (display x) (newline)
  (display "y=") (display y) (newline)
  (display "z=") (display z) (newline))

(test 1) (newline)
(test 1 #:y 100 #:z 599)
