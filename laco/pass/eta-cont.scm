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

(define-module (laco pass eta-cont)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco pass normalize)
  #:use-module (ice-9 match))

(define (ec expr)
  (match expr
    (($ letcont/k ($ bind-special-form/k _ j (? app/k? app) body))
     (ec (cfs body (list j) (list app))))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr
      (ec (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set!
      expr
      (ec (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ec exprs))
     expr)
    (($ app/k _ func args)
     (app/k-func-set! expr (ec func))
     (app/k-args-set! expr (map ec args))
     expr)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (ec body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ec cnd))
     (branch/k-tbranch-set! expr (ec b1))
     (branch/k-fbranch-set! expr (ec b2))
     expr)
    (else expr)))

(define-pass eta-cont expr (ec expr))
