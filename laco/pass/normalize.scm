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

(define-module (laco pass normalize)
  #:use-module (laco cps)
  #:use-module (laco types)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (cfs
            normalize/preserving
            beta-reduction/preserving))

;; NOTE: After normalize, there's no ((lambda ...) args), all the applications will
;;       be (id args). This is useful in codegen.
;;
;; NOTE: We can even perform normalization for applicative-order language like
;;       Scheme after CPS transformation, since the result of all the expressions
;;       had been bounded by the continuation, which guarantees applicative-order.
;;       That's why we only perform normalize/preserving to preserve the binding
;;       of continuations: letval/letcont/letfun...etc, say:
;;       (let ((x (+ 1 2))) (+ x x))
;;       ==>
;;       (letcont ((j (lambda (K)
;;                     ((lambda (k1)
;;                       ((lambda (k2)
;;                         ((lambda (k3) (k1 k3 k2))
;;                          K))
;;                        K))
;;                      +))))
;;        (letval ((y 2))
;;          (letval ((z 1))
;;            (j (+ y z)))))
;;       Please notice that K is the variable which bounds the result of (+ 1 2),
;;       after CPS transformation it's (+ y z), and it was applied in two places
;;       that K appeared. So (+ 1 2) was evaluated just once.
;;       If we don't preserve them, then the program was converted to normal-order.
;;       ==> (+ (+ 1 2) (+ 1 2))
;;       The (+ 1 2) was substituted to two places, and (+ 1 2) will be evaluated
;;       twice. So it's typical normal-order.
;;
;; NOTE: Please notice that the normalize pass is actually normalize/preserving,
;;       since it only substitutes the regular let bindings, and preserving
;;       letval/letcont/letfun...etc, which are useful for CPS optimizing.
;;
;; NOTE: We also provide normalize function which substitute any kinds of let
;;       bindings for debugging.

;; capture free substitute
(define (cfs expr args el)
  (define (substitute e)
    (cond
     ((list-index (lambda (x) (node-eq? x e)) args)
      => (lambda (i) (list-ref el i)))
     (else e)))
  (when (not (= (length args) (length el)))
    (throw 'laco-error cfs
           "BUG: expr: ~a, args: ~a, el: ~a~%" expr args (map cps->expr el)))
  (match expr
    (($ lambda/k ($ cps _ kont name attr) fargs body)
     ;;(format #t "cfs 0 ~a~%" expr)
     (lambda/k-body-set! expr (cfs body args el))
     expr)
    (($ app/k ($ cps _ kont name attr) func fargs)
     ;;(format #t "cfs 1 ~a~%" expr)
     (app/k-func-set! expr (cfs func args el))
     (app/k-args-set! expr (map (lambda (ee) (cfs ee args el)) fargs))
     expr)
    (($ seq/k ($ cps _ kont name attr) exprs)
     ;;(format #t "cfs 2 ~a~%" expr)
     (seq/k-exprs-set! expr (map (lambda (ee) (cfs ee args el)) exprs))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr (cfs (bind-special-form/k-value expr) args el))
     (bind-special-form/k-body-set!
      expr (cfs (bind-special-form/k-body expr) args el))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (cfs cnd args el))
     (branch/k-tbranch-set! expr (cfs b1 args el))
     (branch/k-fbranch-set! expr (cfs b2 args el))
     expr)
    (($ assign/k _ v e)
     (assign/k-var-set! expr (cfs v args el))
     (assign/k-expr-set! expr (cfs e args el))
     expr)
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map (lambda (e) (cfs e args el)) value))
     expr)
    (($ primitive?)
     (substitute expr))
    (($ id _ name _)
     (substitute expr))
    (($ lvar ($ id _ name _) _)
     (substitute expr))
    (else expr)))

(define (beta-reduction expr)
  (match expr
    (($ app/k _ ($ lambda/k _ params body) args)
     ;;(display "beta 0\n")
     (beta-reduction
      (cfs (beta-reduction body)
           params
           (beta-reduction args))))
    (($ app/k _ f args)
     (app/k-args-set! expr (map beta-reduction args))
     expr)
    (($ lambda/k _ args body)
     ;;(display "beta 2\n")
     (lambda/k-body-set! expr (beta-reduction body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (beta-reduction cnd))
     (branch/k-tbranch-set! expr (beta-reduction b1))
     (branch/k-fbranch-set! expr (beta-reduction b2))
     expr)
    (($ seq/k _ e)
     (seq/k-exprs-set! expr (map beta-reduction e))
     expr)
    ((? bind-special-form/k? bsf)
     (beta-reduction
      (new-app/k (make-lambda/k (list (cps-kont bsf) (cps-name bsf) (cps-attr bsf))
                                (list (bind-special-form/k-var bsf))
                                (bind-special-form/k-body bsf))
                 (beta-reduction (bind-special-form/k-value bsf)))))
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map beta-reduction value))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (beta-reduction e))
     expr)
    (else expr)))

(define (beta-reduction/preserving expr)
  (match expr
    (($ app/k _ ($ lambda/k _ params body) args)
     ;;(display "beta 0\n")
     (beta-reduction/preserving
      (cfs (beta-reduction/preserving body)
           params
           (beta-reduction/preserving args))))
    (($ app/k _ f args)
     (app/k-args-set! expr (map beta-reduction/preserving args))
     expr)
    (($ lambda/k _ args body)
     ;;(display "beta 2\n")
     (lambda/k-body-set! expr (beta-reduction/preserving body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (beta-reduction/preserving cnd))
     (branch/k-tbranch-set! expr (beta-reduction/preserving b1))
     (branch/k-fbranch-set! expr (beta-reduction/preserving b2))
     expr)
    (($ seq/k _ e)
     (seq/k-exprs-set! expr (map beta-reduction/preserving e))
     expr)
    (($ collection/k _ var type size value)
     (collection/k-value-set! expr (beta-reduction/preserving value))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr (beta-reduction/preserving (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set!
      expr (beta-reduction/preserving (bind-special-form/k-body expr)))
     expr)
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map beta-reduction/preserving value))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (beta-reduction/preserving e))
     expr)
    (else expr)))

(define (eta-reduction expr)
  (match expr
    ;; FIXME: deal with args
    (($ lambda/k _ arg ($ app/k _ f ((? (lambda (a) (id-eq? a arg))))))
     ;;(display "eta-0\n")
     (eta-reduction f))
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (eta-reduction cnd))
     (branch/k-tbranch-set! expr (eta-reduction b1))
     (branch/k-fbranch-set! expr (eta-reduction b2))
     expr)
    (($ seq/k ($ cps _ kont name attr) exprs)
     ;;(display "eta-1\n")
     (seq/k-exprs-set! expr (map eta-reduction exprs))
     expr)
    (($ collection/k _ var type size value)
     (collection/k-value-set! expr (eta-reduction value))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr (eta-reduction (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set!
      expr (eta-reduction (bind-special-form/k-body expr)))
     expr)
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (eta-reduction value))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (eta-reduction e))
     expr)
    (else expr)))

(define (normalize expr)
  (fold (lambda (f p) (f p))
        expr
        (list beta-reduction
              eta-reduction)))

(define (normalize/preserving expr)
  (fold (lambda (f p) (f p))
        expr
        (list beta-reduction/preserving
              eta-reduction)))

(define-pass normalize cps (normalize/preserving cps))
