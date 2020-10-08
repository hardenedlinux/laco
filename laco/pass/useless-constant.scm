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

(define-module (laco pass useless-constant)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco env)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; Yes, we can backtrace constant from letcont/k in CPS-tree, however, it's better
;; never backtrace. We can always use lookup table.

(define *const-lookup* (make-hash-table))
(define (const-register! kname) (hash-set! *const-lookup* kname #t))
(define (is-const? expr)
  (match expr
    (($ id _ kname _)
     (if (hash-ref *const-lookup* kname)
         #f
         expr))
    (else expr)))

(define (uct expr)
  (match expr
    (($ letcont/k ($ bind-special-form/k _ jname
                     ($ letval/k ($ bind-special-form/k _
                                    var-1 (? constant/k?)
                                    ($ app/k _ ($ primitive _ 'return _ _ _)
                                       (var-2))))
                     body))
     (=> back!)
     (when (kont-eq? var-1 var-2)
       (const-register! (id-name jname)))
     (back!))
    (($ seq/k _ exprs)
     (let ((new-exprs `(,@(filter-map is-const? (const-useless-position exprs))
                        ,@(tail-position exprs))))
       (seq/k-exprs-set! expr (map uct new-exprs)))
     expr)
    (($ app/k _ f args)
     (app/k-func-set! expr (uct f))
     (app/k-args-set! expr (map uct args))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (uct (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (uct (bind-special-form/k-body expr)))
     expr)
    (($ app/k _ func args)
     (app/k-func-set! expr (uct func))
     (app/k-args-set! expr (map uct args))
     expr)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (uct body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (uct cnd))
     (branch/k-tbranch-set! expr (uct b1))
     (branch/k-fbranch-set! expr (uct b2))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (uct e))
     expr)
    (else expr)))

(define-pass useless-constant expr (uct expr))
