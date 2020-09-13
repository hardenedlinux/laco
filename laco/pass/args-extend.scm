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

(define-module (laco pass args-extend)
  #:use-module (laco env)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (ice-9 match))

;; NOTE: This pass must be after primitive-conversion.

;; extend optional args:
;; (define fun (lambda (e . args) ...))
;; (fun 1 2 3) => (fun 1 '(2 3))
;;
;; extend keyword bound arguments:
;; TODO:
;; (define fun (lambda* (x #:keys (y 1)) ...))
;; case-1:
;; (fun 1 #:y 2) => rebind y
;; case-2:
;; (fun 1) => y is unspecified, use default value 1

(define (ae expr)
  (match expr
    (($ app/k _ f args)
     (cond
      ((lambda-has-vargs? (id-name f))
       => (lambda (opt-index)
            (when (>= opt-index (length args))
              (throw 'laco-error 'args-extend "Invalid opt-index `~a'!" opt-index))
            (let* ((cargs (list-head args opt-index))
                   (varg (list-tail args opt-index))
                   (lst (new-collection/k
                         (new-id "#c-")
                         'list
                         (length varg)
                         varg))
                   (extended-args `(,@cargs ,lst)))
              (app/k-args-set! expr (map ae extended-args))
              expr)))
      (else
       (app/k-args-set! expr (map ae args))
       expr)))
    (($ lambda/k ($ cps _ _ name _) args body)
     (lambda/k-body-set! expr (ae body))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ae exprs))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (ae (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (ae (bind-special-form/k-body expr)))
     expr)
    (($ app/k _ func args)
     (app/k-func-set! expr (ae func))
     (app/k-args-set! expr (map ae args))
     expr)
    (($ lambda/k _ _ body)
     (parameterize ((current-kont (cps-kont expr)))
       (lambda/k-body-set! expr (ae body)))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ae cnd))
     (branch/k-tbranch-set! expr (ae b1))
     (branch/k-fbranch-set! expr (ae b2))
     expr)
    (else expr)))

(define-pass args-extend expr (ae expr))
