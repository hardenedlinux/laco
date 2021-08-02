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

(define-module (laco pass delta-reduction)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define (cps-integer? expr)
  (match expr
    (($ constant/k _ ($ constant _ _ 'integer)) #t)
    (else #f)))

;; primitive -> cps-list -> cps
(define (prim-apply p args)
  (apply (primitive-impl p)
         (map (lambda (x) (constant-val (constant/k-value x))) args)))

(define (constant-integer? x)
  (match x
    (($ constant _ 'integer v) #t)
    (else #f)))

(define *int->int*
  '(+ - * /))

(define *preds*
  '(number? symbol? keyword? procedure? primitive?
            boolean? char? integer? rational? real? complex?))

(define *collection-preds*
  '(null? list? vector? pair?))

(define (make-applicable-checker what)
  (lambda (p)
    (and (applicable-primitive? p)
         (memq (primitive-name p) what))))

(define is-prim:int->int? (make-applicable-checker *int->int*))
(define is-prim:pred? (make-applicable-checker *preds*))
(define is-prim:collection-pred? (make-applicable-checker *collection-preds*))

;; NOTE: fold-constant must be applied before, otherwise it doesn't work.
;; FIXME: Only pure-functional primitives can be reduced.
(define (delta expr)
  (define (prim-fold p args)
    (and (every constant-integer? args)
         (prim-apply p args)))
  (match expr
    (($ app/k _ ($ lambda/k _ v body) e)
     (lambda/k-body-set! (app/k-func expr) (delta body))
     (app/k-args-set! expr (map delta e))
     expr)
    (($ lambda/k _ v body)
     (lambda/k-body-set! expr (delta body))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (delta (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (delta (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map delta exprs))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (delta e))
     expr)
    (($ app/k _ (? id? f) args)
     (app/k-func-set! expr f)
     (app/k-args-set! expr (map delta args))
     expr)
    (($ app/k _ (? primitive? p) args)
     (let ((al (map delta args)))
       ;; TODO: Add more patterns
       (cond
        ((and (every cps-integer? al) (is-prim:int->int? p))
         (new-constant/k (prim-apply p al)))
        ((and (is-prim:pred? p) (every constant/k? al))
         (new-constant/k (prim-apply p (map constant/k-value al))))
        ((and (is-prim:collection-pred? p)
              (or (collection/k? (car al)) (constant/k? (car al))))
         (when (not (= 1 (length al)))
           (throw 'laco-error "Pred `~a' only accepts one argument! (~a)"
                  (primitive-name p) (cps->expr al)))
         (let* ((e (car al))
                (result
                 (cond
                  ((collection/k? e)
                   (let ((type (collection/k-type e)))
                     (if (eq? type 'list)
                         (if (null? (collection/k-value e))
                             ((primitive-impl p) 'null)
                             ((primitive-impl p) type))
                         ((primitive-impl p) type))))
                  ((constant/k? e)
                   (gen-constant #f))
                  (else
                   (throw
                    'laco-error
                    "BUG: only accepts collection or constant, not `~a'" e)))))
           (new-constant/k result)))
        (else
         (app/k-args-set! expr al)
         expr))))
    (else expr)))

(define-pass delta-reduction expr (delta expr))
