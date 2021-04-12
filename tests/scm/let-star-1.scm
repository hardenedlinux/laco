(define (ret)
  20480)

(define (fun x)
  (let* ((a x)
         (b (+ 4 (ret))))
    (display (+ a b))))

(fun 12)
