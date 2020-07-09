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

(define-module (laco pass eta-func)
  #:use-module (laco cps)
  #:use-module (laco types)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (laco pass normalize)
  #:use-module (ice-9 match))

;; TODO:
;; eta-func and eta-cont, it's ok for normal-order to inline directly, but how about
;; applicative-order?


;; Eliminate all anonymouse functions
(define (ef expr)
  (match expr
    (($ letfun/k ($ bind-special-form/k _
                    f ($ lambda/k _ args ($ app/k _ g _)) body))
     (cfs body (list g) (list f)))
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ef exprs))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr
      (ef (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set!
      expr
      (ef (bind-special-form/k-body expr)))
     expr)
    (($ app/k _ func args)
     (app/k-args-set! expr (map ef args))
     expr)
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (ef body))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ef cnd))
     (branch/k-tbranch-set! expr (ef b1))
     (branch/k-fbranch-set! expr (ef b2)))
    (else expr)))

(define-pass eta-function expr (ef expr))
