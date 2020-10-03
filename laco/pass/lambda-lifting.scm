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

(define-module (laco pass lambda-lifting)
  #:use-module (laco utils)
  #:use-module (laco env)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco pass normalize)
  #:use-module (ice-9 match))

;; NOTE: This pass must be after normalize and closure-conversion
;;
;; After closure-conversion, some lambdas have no free variables, so we can lift
;; them to be a top-level defined function:
;; 1. simple lambda: no deeper function definition inside.
;; 2. lambdas that is no escaping closures, in this case we can lift the inner
;;    lambdas recursively.
;; This pass maybe useful to simplify the free variable analysis.


;; Remove useless free-vars
;; After closure-conversion, there's no letcont/k, so we can remove all
;; the useless "letcont/k-" id. Otherwise it would interfere the sorting of
;; free-vars.
(define (eliminate-unused-vars! used-vars env)
  (env-frees-set! env (list->queue used-vars)))

(define lift-name (make-parameter #f))

(define (ll expr)
  (match expr
    (($ letfun/k ($ bind-special-form/k _ fname func body))
     (cond
      ((eq? (current-kont) 'global)
       (bind-special-form/k-value-set! expr (ll func))
       (bind-special-form/k-body-set! expr (ll body))
       expr)
      (else
       (top-level-set! (id-name fname) (ll func))
       (parameterize ((lift-name (id->string fname)))
         (ll body)))))
    (($ closure/k  ($ cps _ _ _ attr) env body)
     (eliminate-unused-vars! (free-vars (new-lambda/k (env->args env) body) #t) env)
     (cond
      ((no-free-var? env)
       ;; If the closure capture nothing, then lift it as a global function
       (let ((cname (new-id "#lifted-")))
         (top-level-set! (id-name cname)
                         (new-lambda/k (env->args env) body #:attr attr))
         (new-gvar cname)))
      (else
       (closure/k-body-set! expr (ll body))
       expr)))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (ll (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (ll (bind-special-form/k-body expr)))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (ll cnd))
     (branch/k-tbranch-set! expr (ll b1))
     (branch/k-fbranch-set! expr (ll b2))
     expr)
    (($ app/k _ f args)
     (app/k-func-set! expr (ll f))
     (app/k-args-set! expr (map ll args))
     expr)
    (($ lambda/k _ args body)
     (lambda/k-body-set! expr (ll body))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map ll exprs))
     expr)
    (($ lvar ($ id _ name _) _)
     (if (equal? (lift-name) (symbol->string name))
         (new-gvar (new-id (lift-name) #f))
         expr))
    (else expr)))

;; Lambda-lifting does two things:
;; 1. Lifting free variables to parameters
;; 2. Lifting functions to higher scoping as possible
(define-pass lambda-lifting expr (ll expr))
