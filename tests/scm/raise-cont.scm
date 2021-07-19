(with-exception-handler
    (lambda (e)
      (cond
       ((eq? e 'need-number)
        (display "catch! Let me return a number for you!\n")
        5)
       (else "no")))
  (lambda ()
    (display "1\n")
    (display (+ (raise-continuable 'need-number) 10))
    (display "\n2\n")))
