(define (fun x) x)

(define (main)
  (if (fun #t)
      (display (fun 1))
      (display (fun "n\n"))))

(main)
