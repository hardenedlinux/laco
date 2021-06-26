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

(define-module (laco pass dce)
  #:use-module (laco utils)
  #:use-module (laco pass effect-analysis)
  #:use-module (laco env)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define (is-referenced? expr v)
  (let ((free (free-vars expr)))
    (match v
      ((? id?)
       (any (lambda (x) (id-eq? x v)) free))
      ((? id-list?)
       (let ((ret (insec free v)))
         (if (null? ret)
             #f
             ret)))
      (else (throw 'laco-error is-referenced? "BUG: Invalid pattern `~a'!" v)))))

;; keep same order of new if the element appears in the old
(define (keep-referenced old referenced)
  (let ((rl (map id-name referenced)))
    (fold-right (lambda (x p)
                  (if (memq (id-name x) rl) (cons x p) p))
                '() old)))

;; TODO:
;; 1. The effectless and useless constant expression should be eliminated, say:
;;    (begin
;;     (list 0 1 2)
;;     123)
;;    The useless constant (list 0 1 2) should be eliminated.
;;    * This optimization may need fallback from the letval/k that found the
;;      constant. And check the letval/k of the elements one by one.
;;    * If all elements are confirmed that effectless, then eliminate it.
;;    * If n of m elments have effect, then convert the n elements to be a
;;      seq/k with n exprs. The effectless elements should be eliminated.

(define (dve expr)
  (match expr
    ((? bind-special-form/k? sf)
     (cond
      ((is-referenced? (bind-special-form/k-body sf) (bind-special-form/k-var sf))
       (bind-special-form/k-value-set! sf (dve (bind-special-form/k-value sf)))
       (bind-special-form/k-body-set! sf (dve (bind-special-form/k-body sf)))
       expr)
      (else (dve (bind-special-form/k-body sf)))))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (seq/k-exprs-set! expr (map dve exprs))
     expr)
    (($ app/k _ ($ lambda/k _ v body) e)
     ;; TODO: Here we just keep the variable which is referenced in the body,
     ;;       however, it is possible to further optimize the body so that the
     ;;       referencing can be eliminated.
     ;;       A better way is to do it again after all optimizings.
     (cond
      ((is-referenced? body v)
       (lambda/k-body-set! (app/k-func expr) (dve body))
       (app/k-args-set! expr (map dve e))
       expr)
      (else
       ;; There could be side-effects, so the args should never be dropped.
       (seq/k-exprs-set! expr `(,@e ,@(seq/k-exprs body)))
       (dve expr))))
    (($ lambda/k _ args body)
     (let ((refereced-args (keep-referenced args (all-ref-vars body))))
       (lambda/k-args-set! expr refereced-args)
       (lambda/k-body-set! expr (dve body))
       expr))
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (dve cnd))
     (branch/k-tbranch-set! expr (dve b1))
     (branch/k-fbranch-set! expr (dve b2))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (dve e))
     expr)
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (map dve exprs))
     expr)
    (else expr)))

;; This includes dead-continuation and dead-variable elimination
(define-pass dead-variable-elimination expr (dve expr))

;; NOTE: Please notice that we've converted local function binding to let-binding
;;       during AST building step, so the top-level function definition is the only
;;       thing that I need to take care. Because the local binding will be handled
;;       by dead-variable-eliminate.
;;       That's what we concern in dead-fun and fun-inline

;; Removes a function definition if it has no applied occurrences outside
;; its ownbody.
(define-pass dead-function-elimination expr
  (cond
   ((not (eq? (current-kont) 'global))
    (let ((funcs (top-level->body-list))
          (fv (free-vars expr)))
      (for-each (lambda (k)
                  (when (not (appears-in-globals k))
                    (remove-fvs! (map id-name (free-vars (top-level-ref k))))
                    (top-level-delete! k)))
                (lset-difference eq? funcs (map id-name fv)))
      expr))
   (else expr)))
