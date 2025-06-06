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

(define-module (laco pass fold-const)
  #:use-module (laco cps)
  #:use-module (laco types)
  #:use-module (laco utils)
  #:use-module (laco pass)
  #:use-module (laco pass normalize)
  #:use-module (ice-9 match))

;; NOTE: If we have supported assignment, then the modified variable has to change
;;       its fold value, or even can't be folded.
;; TODO: Fold global instant.

;; 1. (if 1 2 3)   k->   (if 1 2 3)
;; 2. (if 1 2 (+ 3 4))  k->  (letcont ((k (return (+ 4 3)))) (if 1 2 k))
(define (fc expr)
  (match expr
    (($ letval/k ($ bind-special-form/k _ v e body))
     (=> failed!)
     (cond
      ((is-effect-var? (id-name v))
       (failed!))
      (else
       (fc (cfs (fc body) (list v) (list (fc e)))))))
    (($ app/k _ f args)
     (app/k-func-set! expr (fc f))
     (app/k-args-set! expr (map fc args))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (fc (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (fc (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map fc exprs))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (fc cnd))
     (branch/k-tbranch-set! expr (fc b1))
     (branch/k-fbranch-set! expr (fc b2))
     expr)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (fc body))
     expr)
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map fc value))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (fc e))
     expr)
    (else expr)))

(define-pass fold-constant expr (fc expr))
