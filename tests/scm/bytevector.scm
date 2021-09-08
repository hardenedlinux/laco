;; bytevector.scm
(define a #vu8(10 11 12 13 14))
(display a)
(newline)

(define b #vu8(20 21 22 23 24))
(display b)
(newline)

(define c (make-bytevector 5 127))
(display c)
(newline)

(define d (make-bytevector 5 0))
(display d)
(newline)

(display (bytevector-length a))
(newline)

(display (bytevector-u8-ref a 3))
(newline)

(bytevector-u8-set! d 2 88)
(display d)
(newline)

;; FIXME: use this when vm error about global variables fixed
;; (define e (bytevector-copy c 0 5))
(define e (bytevector-copy b 0 5)) ;; OK
(display e)
(newline)

(bytevector-copy! c 1 a 1 3)
(display c)
(newline)

(let ((g (bytevector-append c d)))
  (display g)
  (newline))

;; FIXME: use this when vm error about global variables fixed
;; (define f (bytevector-append a b))
;; (display f)
;; (newline)
