;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2021-2022
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

(define-module (laco pass closure-lifting)
  #:use-module (laco utils)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco types)
  #:use-module (laco env)
  #:use-module (laco primitives)
  #:use-module (laco pass normalize)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; Lifting closure from application operands
;; This can fix the issue of capturing inside prelude.
;; Usually, the closures have already been lifted in CPS transformation, however they may be
;; introduced by the previous pass.
;;
;; (f ... (closure ...) ...)
;; ==> (let ((c (closure ...)))
;;       (f ... c ...))

;; NOTE:
;; We re-init binding table in this pass to make sure all the bindings are confirmed before
;; closure lifting. Because other passes may introduce new bindings, so it's not safe to
;; do it outside of this pass.

(define *bindings-table* (make-hash-table))
(define (binding-register! name-id cexpr)
  (hash-set! *bindings-table* name-id cexpr))
(define (binding-id->cps name-id)
  (hash-ref *bindings-table* name-id))

;; The CPS here should be immutable
(define (init-binding-table expr)
  (match expr
    ((? bind-special-form/k?)
     (let ((v (bind-special-form/k-var expr))
           (e (bind-special-form/k-value expr))
           (body (bind-special-form/k-body expr)))
       (binding-register! v e)
       (init-binding-table e)
       (init-binding-table body)
       expr))
    (($ app/k _ func args)
     (init-binding-table func)
     (map init-binding-table args)
     expr)
    (($ seq/k _ exprs)
     (map init-binding-table exprs)
     expr)
    (($ branch/k _ cnd b1 b2)
     (init-binding-table cnd)
     (init-binding-table b1)
     (init-binding-table b2)
     expr)
    (($ assign/k _ v e)
     (init-binding-table e)
     expr)
    (($ lambda/k _ _ body)
     (init-binding-table body)
     expr)
    (else expr)))

(define (is-closure? e)
  (define (check-closure? e)
    (and (lambda/k? e)
         (let ((fv (free-vars e)))
           (and (not (null? fv)) e))))
  (cond
   ((check-closure? e) => identity)
   ((id? e) (and=> (binding-id->cps e) check-closure?))
   (else #f)))

;; NOTE: We need to skip the binding-encoded-in-kont, since we'll flatten it
;;       in later closure-conversion. Otherwise the closure-conversion generates
;;       wrong code.
(define (need-to-skip-binding-in-kont? args)
  (match args
    ((($ lambda/k ($ cps _ _ _ attr) _ _) rest ...)
     (assoc-ref attr 'binding))
    (else #f)))

(define (is-return? p)
  (and (primitive? p)
       (eq? 'return (primitive-name p))))

(define (cl expr)
  (match expr
    ;; (($ assign/k _ v ($ app/k _ (? is-return?) ((? lambda/k? closure))))
    ;;  (=> failed!)
    ;;  (cond
    ;;   ((toplevel? (current-env))
    ;;    (failed!))
    ;;   (else
    ;;    (let ((closure-id (new-id "#closure-assign-lift-")))
    ;;      (cps-property-set! closure 'closure-lifted #t)
    ;;      (assign/k-expr-set! expr closure-id)
    ;;      (new-letcont/k closure-id (cl closure) expr)))))
    (($ app/k ($ cps _ kont _ _) func args)
     (cond
      ((primitive? func)
       ;; Ignore the primitive or global func application, since there's no new stack frame
       ;; generated.
       (app/k-args-set! expr (map cl args))
       expr)
      ((toplevel? (current-env))
       (app/k-func-set! expr (cl func))
       (app/k-args-set! expr (map cl args))
       expr)
      (else
       (let* ((need-lift? #f)
              (fname (new-id "#closure-func-lift-"))
              (flag (need-to-skip-binding-in-kont? args))
              (k (if flag (list (car args)) '()))
              (lst (if flag (cdr args) args))
              (ev (append k
                          (map (lambda (e)
                                 (cond
                                  ((is-closure? e)
                                   (or need-lift? (set! need-lift? #t))
                                   (new-id "#closure-expr-lift-"))
                                  (else e)))
                               lst))))
         (cond
          (need-lift?
           (let ((new-expr
                  (append k
                          (fold-right
                           (lambda (v e p)
                             (cond
                              ((is-closure? e)
                               => (lambda (ee)
                                    (cps-property-set! ee 'closure-lifted #t)
                                    (new-letcont/k v ee p #:kont kont)))
                              (else p)))
                           (begin
                             (app/k-args-set! expr ev)
                             expr)
                           ev lst))))
             (cond
              ((lambda/k? func)
               (app/k-func-set! expr fname)
               (new-letcont/k fname func new-expr #:kont kont))
              (else new-expr))))
          (else
           (app/k-func-set! expr (cl func))
           (app/k-args-set! expr (map cl args))
           expr))))))
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr
      (cl (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set!
      expr
      (cl (bind-special-form/k-body expr)))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map cl exprs))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (cl cnd))
     (branch/k-tbranch-set! expr (cl b1))
     (branch/k-fbranch-set! expr (cl b2))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (cl e))
     expr)
    (($ lambda/k _ _ body)
     ;;(lambda/k-body-set! expr (cl body))
     (parameterize ((current-env 'fake-env))
       (lambda/k-body-set! expr (cl body)))
     expr)
    (else expr)))

(define-pass closure-lifting expr
  (begin
    (init-binding-table expr)
    (cl expr)))
