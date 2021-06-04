(define (test-function1)
  "(test-function1)")

(define (main)
  (let ((l (test-function1)))
    (display "l = ")
    (display l)
    (newline)))

(display "before test\n")
(main)
(display "after test\n")
