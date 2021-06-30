;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2021
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

(define-module (laco pass effect-analysis)
  #:use-module (laco cps)
  #:use-module (laco types)
  #:use-module (laco utils)
  #:use-module (laco pass)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (body-has-effect?))

;; NOTE:
;; 1. Tag all variables that was operated by functions had side-effects

(define *effect-funcs*
  '(set! list-set! vector-set! set-car! set-cdr! string-set!))
(define (proc-has-effect? v) (memq v *effect-funcs*))

(define (all-effect-vars lst)
  (filter-map (lambda (v) (and (is-effect-var? v) v)) lst))

(define (body-has-effect? expr)
  (and=> (assoc-ref (cps-attr expr) 'effect-vars)
         (compose not null?)))

(define (ea expr)
  (match expr
    (($ app/k _ f args)
     (when (and (proc-has-effect? (id-name f)) (id? (car args)))
       (effect-var-register! (id-name (car args))))
     (app/k-func-set! expr (ea f))
     (app/k-args-set! expr (map ea args))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (ea (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (ea (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ea exprs))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ea cnd))
     (branch/k-tbranch-set! expr (ea b1))
     (branch/k-fbranch-set! expr (ea b2))
     expr)
    (($ lambda/k _ _ body)
     (let ((checked-body (ea body))
           (all-vars (map id-name (all-ref-vars body))))
       (cps-property-set! expr 'effect-vars (all-effect-vars all-vars))
       (lambda/k-body-set! expr checked-body)
       expr))
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map ea value))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (ea e))
     expr)
    (else expr)))

(define-pass effect-analysis expr (ea expr))
