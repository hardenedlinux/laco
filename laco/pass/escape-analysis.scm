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
;; Created in the same scope of call-site.
;;
;; Check:
;; 1. Pass as non-kont argument -> escape
;; 2. Return -> escape
;; 3. Call as a function in its born scope-> non-escape

(define (tag-escape! expr)
  (cps-property-set! expr 'escape #t)
  expr)

(define (ea expr)
  (match expr
    (($ seq/k _ exprs)
     (let ((tail (car (reverse exprs))))
       (when (lambda/k? tail)
         (tag-escape! tail)))
     (seq/k-exprs-set! expr (map ea exprs))
     expr)
    (($ app/k _ func args)
     (app/k-func-set! expr (ea func))
     (app/k-args-set!
      expr
      (cons
       ;; NOTE:
       ;; We call the continuation that added by CPS compiler the syn-kont, and the
       ;; users created continuation the user-kont.
       ;; The syn-kont is never an escaped-in closure, but could be an excape-out.
       (ea (car args))
       (map (lambda (e)
              (when (lambda/k? e)
                (tag-escape! e))
              (ea e))
            (cdr args))))
     expr)
    (($ lambda/k _ _ body)
     (when (lambda/k? body)
       (tag-escape! body))
     (lambda/k-body-set! expr (ea body))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (ea (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (ea (bind-special-form/k-body expr)))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (ea e))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ea cnd))
     (branch/k-tbranch-set! expr (ea b1))
     (branch/k-fbranch-set! expr (ea b2))
     expr)
    (else expr)))

(define-pass escape-analysis expr (ea expr))
