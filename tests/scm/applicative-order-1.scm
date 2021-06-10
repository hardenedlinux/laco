(define (test n)
  (display n)
  n)

(define (main)
  (let* ((b (test 6))
         (c (list b b b)))
    (newline)
    (display c)
    (newline)))

(main)
