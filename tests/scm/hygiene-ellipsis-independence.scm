(define-syntax hygienic-repeat-tmp
  (syntax-rules ()
    ((_ (x ...))
     (list (let ((tmp x)) tmp) ...))))

(let ((res (hygienic-repeat-tmp (1 2))))
  (display (car res)) (newline)
  (display (car (cdr res))) (newline))
