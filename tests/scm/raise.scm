(with-exception-handler
    (lambda (e)
      (cond
       ((eq? e 'need-number)
        (display "If it's continuable, then you wouldn't see error!\n")
        5)
       (else "no")))
  (lambda ()
    (display "1\n")
    (display (+ (raise 'need-number) 10))
    (display "\n2\n")))
