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

(define-module (laco pass primitive-conversion)
  #:use-module (laco env)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (ice-9 match))

(define (pc expr)
  (define (convert e)
    (cond
     ((and (primitive? e) (not (eq? (primitive-name e) 'return)))
      (let* ((fname (new-id "#func-"))
             (k (new-id "karg-"))
             (args (new-id "vargs-"))
             (new-expr (new-letfun/k
                        fname
                        (new-lambda/k
                         (list k args)
                         (new-app/k k (new-app/k prim:apply (list e args) #:kont k)
                                    #:kont k)
                         #:kont k)
                        (new-app/k prim:return fname))))
        (lambda-has-vargs! (id-name fname) 1)
        new-expr))
     (else (pc e))))
  (match expr
    (($ app/k _ (? primitive? p) args)
     (app/k-args-set! expr (map pc args))
     expr)
    (($ app/k _ f (k args ...))
     ;; NOTE:
     ;; 1. The primitive in args of function must do primitive-conversion.
     ;;    But not for the args of primitive.
     ;; 2. In CPS, the first arg k is the continuation, if the primitive appears
     ;;    in k-position, then we don't convert it to yet another nested CPS.
     (app/k-func-set! expr (pc f))
     (app/k-args-set! expr (cons (pc k) (map convert args)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map pc exprs))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (pc (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (pc (bind-special-form/k-body expr)))
     expr)
    (($ lambda/k _ _ body)
     (parameterize ((current-kont (cps-kont expr)))
       (lambda/k-body-set! expr (pc body)))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (pc cnd))
     (branch/k-tbranch-set! expr (pc b1))
     (branch/k-fbranch-set! expr (pc b2))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (pc e))
     expr)
    (else expr)))

(define-pass primitive-conversion expr (pc expr))
