(define (fun)
  (display "fun\n")
  #f)

(let ((result (fun)))
  (if result
      (display "ok\n")
      (display "no\n")))
