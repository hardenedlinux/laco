;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2021
;;      "Mu Lei" known as "NalaGinrut" <mulei@gnu.org>
;;  Laco is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  Laco is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see <http://www.gnu.org/licenses/>.

(define-module (laco primitives)
  #:use-module (laco utils)
  #:use-module (laco sasm)
  #:use-module (laco types)
  #:use-module (laco records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (symbol->primitive
            is-op-a-primitive?
            primitive-register!

            primitive
            primitive?
            primitive-name
            primitive-arity
            primitive-has-side-effact?
            primitive-impl
            define-primitive
            primitive->number
            print-primitives
            applicable-primitive?

            special-form
            make-special-form:if

            ;; ----------------------------------
            make-prim
            prim?
            prim-name prim-label prim-proc
            is-primitive?))

(define *primitives* (make-hash-table))
(define (symbol->primitive x) (hash-ref *primitives* x))
(define (is-op-a-primitive? x)
  (and (symbol? x)
       (symbol->primitive x)))
(define (primitive-register! p proc) (hash-set! *primitives* p proc))

;; NOTE: The implementation here, should be the generator or caller of
;;       any specific primitive.
(define-typed-record primitive
  (fields
   (name symbol?)
   (arity integer?)
   (has-side-effect? boolean?)
   (impl procedure?)))

(define (new-primitive name arity effect? impl)
  (let ((prim (make-primitive name arity effect? impl)))
    (primitive-register! name prim)
    prim))

(define-syntax define-primitive
  (lambda (x)
    (syntax-case x (:has-side-effect)
      ((_ (name0) func)
       #`(define-primitive name0 #f func))
      ((_ (name1 :has-side-effect) func)
       #`(define-primitive name1 #t func))
      ((_ name side-effect? func)
       #`(define-public
           #,(datum->syntax #'name (symbol-append 'prim: (syntax->datum #'name)))
           (new-primitive
            'name
            (car (procedure-minimum-arity func))
            side-effect?
            func))))))

;; Of course, we can record the primitive number when defining the primitive with
;; a macro. However, an explicit lookup table is useful for debug.
(define *prim-table*
  '(
    ;; basic primitives
    return ; 0
    pop ; 1
    + ; 2
    - ; 3
    * ; 4
    / ; 5
    display ; 6
    apply ; 7
    not ; 8
    = ; 9
    < ; 10
    > ; 11
    <= ; 12
    >= ; 13
    restore ; 14
    reserved-1 ; 15

    ;; extended primitives
    modulo ; 16 + 0
    remainder ; 16 + 1
    for-each ; 16 + 2
    map ; 16 + 3
    list-ref ; 16 + 4
    list-set! ; 16 + 5
    append ; 16 + 6
    eqv? ; 16 + 7
    eq? ; 16 + 8
    equal? ; 16 + 9
    usleep ; 16 + 10
    device-configure! ; 16 + 11
    gpio-set! ; 16 + 12
    gpio-toggle! ; 16 + 13
    get-board-id ; 16 + 14
    cons ; 16 + 15
    car ; 16 + 16
    cdr ; 16 + 17
    read-char ; 16 + 18
    read-string ; 16 + 19
    read-line ; 16 + 20
    list->string ; 16 + 21
    i2c-read-byte! ; 16 + 22
    i2c-write-byte! ; 16 + 23
    null? ; 16 + 24
    pair? ; 16 + 25
    spi-transceive! ; 16 + 26
    i2c-read-list! ; 16 + 27
    i2c-write-list! ; 16 + 28
    with-exception-handler ; 16 + 29
    raise ; 16 + 30
    raise-continuable ; 16 + 31
    error ; 16 + 32
    error-object? ; 16 + 33
    error-object-message ; 16 + 34
    error-object-irritants ; 16 + 35
    read-error? ; 16 + 36
    file-error? ; 16 + 37
    dynamic-wind ; 16 + 38
    list? ; 16 + 39
    string? ; 16 + 40
    char? ; 16 + 41
    keyword? ; 16 + 42
    symbol? ; 16 + 43
    procedure? ; 16 + 44
    primitive? ; 16 + 45
    boolean? ; 16 + 46
    number? ; 16 + 47
    integer? ; 16 + 48
    real? ; 16 + 49
    rational? ; 16 + 50
    complex? ; 16 + 51
    exact? ; 16 + 52
    inexact? ; 16 + 53
    i2c-read-bytevector! ; 16 + 54
    bytevector? ; 16 + 55
    ))

(define (print-primitives)
  (display "--------PRIMITIVES--------\n")
  (pretty-print
   (map cons *prim-table* (iota (length *prim-table*))))
  (display "--------END--------\n"))

(define (gen-error name)
  (throw 'laco-error (symbol-append 'prim: name)
         (format #f "BUG: ~a shouldn't be called in compile time!" name)))

(define (primitive->number p)
  (define (gen-num ll) (- (length *prim-table*) (length ll)))
  (cond
   ((memq (primitive-name p) *prim-table*) => gen-num)
   (else (throw 'laco-error primitive->number "Invalid primitive name `~a'!"
                (primitive-name p)))))

(define *inapplicable-primitive*
  '(halt return display restore usleep device-configure! gpio-set! gpio-toggle!
         get-board-id read-char read-string read-line i2c-read-byte! i2c-write-byte!
         spi-transceive! i2c-read-list! exact? inexact? i2c-read-bytevector! i2c-write-list!
         with-exception-handler raise raise-continuable))

(define (applicable-primitive? p)
  (not (memq (primitive-name p) *inapplicable-primitive*)))

(define (make-pred type)
  (lambda (t)
    (gen-constant (eq? t type))))

;; halt can associate with primitive `halt', its activity is TOS.
(define-primitive (pop)
  (lambda _
    (gen-error 'pop)))

(define-primitive (+)
  (lambda args
    (gen-constant (apply + args))))

(define-primitive (-)
  (lambda (args)
    (gen-constant (apply - args))))

(define-primitive (*)
  (lambda args
    (gen-constant (apply * args))))

(define-primitive (/)
  (lambda args
    (gen-constant (apply / args))))

(define-primitive (return) identity)

(define-primitive (restore)
  (lambda _
    (gen-error 'restore)))

(define-primitive (display)
  (lambda _
    (gen-error 'display)))

(define-primitive (apply)
  (lambda _
    (gen-error 'apply)))

(define-primitive (not)
  (lambda arg
    (gen-constant (not arg))))

(define-primitive (=)
  (lambda args
    (gen-constant (apply = args))))

(define-primitive (<)
  (lambda args
    (gen-constant (apply < args))))

(define-primitive (>)
  (lambda args
    (gen-constant (apply > args))))

(define-primitive (<=)
  (lambda args
    (gen-constant (apply <= args))))

(define-primitive (>=)
  (lambda args
    (gen-constant (apply >= args))))

(define-primitive (modulo)
  (lambda args
    (gen-constant (apply modulo args))))

(define-primitive (remainder)
  (lambda args
    (gen-constant (apply remainder args))))

(define-primitive (for-each)
  (lambda (proc lst . lst*)
    (gen-constant (apply for-each proc lst lst*))))

(define-primitive (map)
  (lambda (proc lst . lst*)
    (gen-constant (apply map proc lst lst*))))

(define-primitive (list-ref)
  (lambda (lst idx)
    (gen-constant (list-ref lst idx))))

(define-primitive (list-set!)
  (lambda (lst idx val)
    (gen-constant (list-set! lst idx val))))

(define-primitive (append)
  (lambda (l1 l2)
    (gen-constant (append l1 l2))))

(define-primitive (eq?)
  (lambda (a b)
    (gen-constant (eq? a b))))

(define-primitive (eqv?)
  (lambda (a b)
    (gen-constant (eqv? a b))))

(define-primitive (equal?)
  (lambda (a b)
    (gen-constant (equal? a b))))

(define-primitive (usleep)
  (lambda _
    (gen-error 'usleep)))

(define-primitive (device-configure!)
  (lambda _
    (gen-error 'device-configure!)))

(define-primitive (gpio-set!)
  (lambda _
    (gen-error 'gpio-set!)))

(define-primitive (gpio-toggle!)
  (lambda _
    (gen-error 'gpio-toggle!)))

(define-primitive (get-board-id)
  (lambda _
    (gen-error 'get-board-id)))

(define-primitive (cons)
  (lambda (x y)
    (gen-constant (cons x y))))

(define-primitive (car)
  (lambda (o)
    (gen-constant (car o))))

(define-primitive (cdr)
  (lambda (o)
    (gen-constant (cdr o))))

(define-primitive (read-char)
  (lambda _
    (gen-error 'read-char)))

(define-primitive (read-string)
  (lambda _
    (gen-error 'read-string)))

(define-primitive (list->string)
  (lambda _
    (gen-error 'list->string)))

(define-primitive (i2c-read-byte!)
  (lambda _
    (gen-error 'i2c-read-byte!)))

(define-primitive (i2c-write-byte!)
  (lambda _
    (gen-error 'i2c-write-byte!)))

(define-primitive (null?) (make-pred 'null))

(define-primitive (pair?)
  (lambda (t)
    (gen-constant (or (eq? t 'pair)
                      (eq? t 'list)))))

(define-primitive (spi-transceive!)
  (lambda _
    (gen-error 'spi-transceive!)))

(define-primitive (i2c-read-list!)
  (lambda _
    (gen-error 'i2c-read-list!)))

(define-primitive (i2c-read-bytevector!)
  (lambda _
    (gen-error 'i2c-read-bytevector!)))

(define-primitive (i2c-write-list!)
  (lambda _
    (gen-error 'i2c-write-list!)))

;; ------ exceptions -------
(define-primitive (with-exception-handler)
  (lambda _
    (gen-error 'with-exception-handler)))

(define-primitive (raise)
  (lambda _
    (gen-error 'raise)))

(define-primitive (raise-continuable)
  (lambda _
    (gen-error 'raise-continuable)))

(define-primitive (error)
  (lambda _
    (gen-error 'error)))

(define-primitive (error-object)
  (lambda _
    (gen-error 'raise-object)))

(define-primitive (error-object-message)
  (lambda _
    (gen-error 'error-object-message)))

(define-primitive (error-object-irritants)
  (lambda _
    (gen-error 'error-object-irritants)))

(define-primitive (read-error?)
  (lambda _
    (gen-error 'read-error)))

(define-primitive (file-error?)
  (lambda _
    (gen-error 'file-error)))

(define-primitive (dynamic-wind)
  (lambda _
    (gen-error 'dynamic-wind)))

(define-primitive (list?) (make-pred 'list))

(define-primitive (string?) (make-pred 'string))

(define-primitive (char?) (make-pred 'char))

(define-primitive (keyword?) (make-pred 'keyword))

(define-primitive (symbol?) (make-pred 'symbol))

(define-primitive (procedure?) (make-pred 'procedure))

(define-primitive (primitive?) (make-pred 'primitive))

(define-primitive (boolean?) (make-pred 'boolean))

(define-primitive (number?) number?)

(define-primitive (integer?) (make-pred 'integer))

(define-primitive (real?) real?)

(define-primitive (rational?) (make-pred 'rational))

(define-primitive (complex?) (make-pred 'complex))

(define-primitive (bytevector?) (make-pred 'bytevector))
