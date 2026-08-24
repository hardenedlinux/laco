(define (helper x) (* x 2))
(define-syntax use-helper
  (syntax-rules () ((_ x) (helper x))))
(display (let ((helper (lambda (x) 999))) (use-helper 5)))
(newline)
