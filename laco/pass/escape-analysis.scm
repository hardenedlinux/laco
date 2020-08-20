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

(define-module (laco pass escape-analysis)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco env)
  #:use-module (laco primitives)
  #:use-module (laco pass)
  #:use-module (ice-9 match))

;; Escaping analysis

;; Anonymous function
;; Created in the same scope with call-site.
;;
;; Check:
;; 1. Pass as argument -> escape
;; 2. Return -> escape
;; 3. Call as a function in its born scope-> non-escape

(define (tag-escape! expr)
  (cps-property-set! expr 'escape #t)
  expr)

(define (ea expr)
  (match expr
    (($ lambda/k _ _ body)
     (lambda/k-body-set! expr (ea body))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ea (map tag-escape! exprs)))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (ea (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (ea (bind-special-form/k-body expr)))
     expr)
    (($ app/k _ func args)
     (app/k-func-set! expr (ea func))
     (app/k-args-set! expr (map ea (map tag-escape! args)))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ea cnd))
     (branch/k-tbranch-set! expr (ea b1))
     (branch/k-fbranch-set! expr (ea b2))
     expr)
    (else expr)))

(define-pass escape-analysis expr (ea expr))
