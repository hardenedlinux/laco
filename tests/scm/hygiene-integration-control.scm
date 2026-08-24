(define-syntax when
  (syntax-rules ()
    ((_ test body ...)
     (if test (begin body ...)))))

(define x 0)
(when (> 2 1)
  (set! x 42))
(display x)
(newline)
