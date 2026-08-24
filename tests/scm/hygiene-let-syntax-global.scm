(define (global-helper x) (* x 2))

(display
  (let-syntax ((choose (syntax-rules () ((_ x) (global-helper x)))))
    (let ((global-helper (lambda (x) 999)))
      (choose 5))))
(newline)
