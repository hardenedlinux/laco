(define-syntax my-or
  (syntax-rules () ((_ a b) (let ((t a)) (if t t b)))))
(display (let ((t 999)) (my-or #f t)))
(newline)
