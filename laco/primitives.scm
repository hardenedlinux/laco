;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020
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
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((rnrs) #:select (define-record-type))
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
      ((_ (name0 args0 ...) body0 ...)
       #`(define-primitive
           name0 #f
           (lambda (args0 ...) body0 ...)))
      ((_ (name1 args0 ...) :has-side-effect body0 ...)
       #`(define-primitive name1 #t (lambda (args0 ...) body0 ...)))
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
    and ; 14
    or ; 15

    ;; extended primitives
    modulo ; 16 + 0
    remainder ; 16 + 1
    foreach ; 16 + 2
    map ; 16 + 3
    list-ref ; 16 + 4
    list-set! ; 16 + 5
    ))

(define (print-primitives)
  (display "--------PRIMITIVES--------\n")
  (pretty-print
   (map cons *prim-table* (iota (length *prim-table*))))
  (display "--------END--------\n"))

(define (primitive->number p)
  (define (gen-num ll) (- (length *prim-table*) (length ll)))
  (cond
   ((memq (primitive-name p) *prim-table*) => gen-num)
   (else (throw 'laco-error primitive->number "Invalid primitive name `~a'!"
                (primitive-name p)))))

(define *inapplicable-primitive*
  '(halt return display))

(define (applicable-primitive? p)
  (not (memq (primitive-name p) *inapplicable-primitive* )))

;; halt can associate with primitive `halt', its activity is TOS.
(define-primitive (pop args ...)
  (throw 'laco-error 'prim:pop "BUG: shouldn't be called in compile time!"))

(define-primitive (+ args ...)
  (gen-constant (+ args ...)))

(define-primitive (- args ...)
  (gen-constant (- args ...)))

(define-primitive (* args ...)
  (gen-constant (* args ...)))

(define-primitive (/ args ...)
  (gen-constant (/ args ...)))

(define-primitive (return x)
  x)

(define-primitive (display x)
  (throw 'laco-error 'prim:display "BUG: shouldn't be called in compile time!"))

(define-primitive (apply f args)
  (throw 'laco-error 'prim:apply "BUG: shouldn't be called in compile time!"))

(define-primitive (not arg)
  (gen-constant (not arg)))

(define-primitive (= args ...)
  (gen-constant (= args ...)))

(define-primitive (< args ...)
  (gen-constant (< args ...)))

(define-primitive (> args ...)
  (gen-constant (> args ...)))

(define-primitive (<= args ...)
  (gen-constant (<= args ...)))

(define-primitive (>= args ...)
  (gen-constant (>= args ...)))

(define-primitive (and args ...)
  (gen-constant (and args ...)))

(define-primitive (or args ...)
  (gen-constant (or args ...)))

(define-primitive (modulo args ...)
  (gen-constant (modulo args ...)))

(define-primitive (remainder args ...)
  (gen-constant (remainder args ...)))

(define-primitive (foreach proc lst lst* ...)
  (gen-constant (foreach proc lst lst* ...)))

(define-primitive (map proc lst lst* ...)
  (gen-constant (map proc lst lst* ...)))

(define-primitive (list-ref lst idx)
  (gen-constant (list-ref lst idx)))

(define-primitive (list-set! lst idx val)
  (gen-constant (list-set! lst idx val)))
