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

(define-module (laco pass elre)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (ice-9 match))

;; Eliminate all the redundant code:
;; NOTE:
;; 1. We're not going to eliminate tail-call (return ...) in thie pass.
;;    The left `return' should imply tail-call after elre, since `return` in non
;;    tail-calls are all eliminated in case-2.
;; 2. FIXME: Make sure all the left `return' are tail-calls, we need them for
;;    low-level TCO stack tweaking in LIR.
(define (elre expr)
  (match expr
    (($ seq/k _ (($ seq/k _ _)))
     ;; case-1: (begin (begin body ...)) -> (begin body ...)
     ;;
     ;; Of course we can make sure no redundant seq was introduced in the parser,
     ;; however, you have to consider if users do it in their code, such like:
     ;; (let ((...)) (begin body ...))
     (elre (car (seq/k-exprs expr))))
    (($ seq/k _ exprs)
     (cond
      ((= (length exprs) 1)
       (let ((e (car exprs)))
         (match e
           (($ app/k _ ($ primitive _ 'return _ _ _) (arg))
            ;; case-2: (begin (return single-expr)) -> single-expr
            (elre arg))
           (else
            ;; case-3: (begin single-expr) -> single-expr
            (elre (car exprs))))))
      (else
       (let ((ne (map elre exprs)))
         (seq/k-exprs-set! expr ne)
         expr))))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (elre (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (elre (bind-special-form/k-body expr)))
     expr)
    (($ app/k _ func args)
     (app/k-args-set! expr (map elre args))
     expr)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (elre body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (elre cnd))
     (branch/k-tbranch-set! expr (elre b1))
     (branch/k-fbranch-set! expr (elre b2))
     expr)
    (else expr)))

(define-pass eliminate-redundant expr (elre expr))
