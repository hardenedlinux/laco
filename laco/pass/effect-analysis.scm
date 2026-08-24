;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2021-2026
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

;; NOTE: plain variable assignment is represented as its own `assign/k'
;; node (handled separately below), never as an `app/k' call to a
;; `set!' primitive, so the `set!' entry here never actually matches
;; anything in practice. Left in place defensively in case some other
;; front-end path ever lowers `set!' to a primcall instead.
(define *effect-funcs*
  '(set! list-set! vector-set! set-car! set-cdr! string-set!))
(define (proc-has-effect? v) (memq v *effect-funcs*))

(define (all-effect-vars lst)
  (filter-map (lambda (v) (and (is-effect-var? v) v)) lst))

;; NOTE: This used to read a cached 'effect-vars attribute that was
;; precomputed once (in the lambda/k case of `ea') and stashed on a
;; specific node object. That cache had no invalidation mechanism: if
;; any later pass reconstructed an equivalent-but-different node
;; instead of mutating the original in place, the cache silently
;; disappeared and this would quietly report "no effect".
;;
;; This is now a thin wrapper around the single shared predicate
;; `any-effect-var?' (in (laco cps)), so `normalize''s beta-reduction
;; and `elre''s case-5 both ask the exact same question the exact same
;; way, instead of each maintaining its own copy of this check.
(define (body-has-effect? expr)
  (any-effect-var? (all-ref-vars expr)))

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
     (lambda/k-body-set! expr (ea body))
     expr)
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map ea value))
     expr)
    (($ assign/k _ v e)
     ;; NOTE: This is the plain-`set!' path, e.g. `(set! v e)'. It was
     ;; previously only recursing into `e' and never registering `v'
     ;; as an effect-var at all. That meant a variable mutated *only*
     ;; via plain `set!' (as opposed to `vector-set!'/`set-car!'/etc.,
     ;; which go through the app/k case below) was never recognized as
     ;; having a side effect anywhere in the compiler -- silently
     ;; breaking any optimization pass (elre's case-5 beta-reduction in
     ;; particular) that relies on `body-has-effect?'/`is-effect-var?'
     ;; to decide whether a binding is safe to eliminate/substitute.
     (when (id? v) (effect-var-register! (id-name v)))
     (assign/k-expr-set! expr (ea e))
     expr)
    (else expr)))

(define-pass effect-analysis expr (ea expr))
