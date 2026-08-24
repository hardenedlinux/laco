(define-syntax swap!
  (syntax-rules () ((_ a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
(let ((tmp 123) (x 555))
  (swap! tmp x)
  (display tmp) (newline)
  (display x) (newline))
