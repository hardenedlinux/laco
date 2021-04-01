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

(define-module (laco pass tco)
  #:use-module (laco types)
  #:use-module (laco env)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; TCO has to be put before closure-converion, otherwise, the func name of app/k
;; would be converted to the offset of the frame, then we lose the information to
;; detect the proper-tail-recursion.

(define* (tco expr #:optional (tail-body? #f))
  (define (tag-tail-call! exprs)
    (when (not (null? exprs))
      (let* ((len (1- (length exprs)))
             (tail (car (list-tail exprs len))))
        `(,@(map tco (list-head exprs len))
          ,(match tail
             (($ app/k ($ cps _ kont _ _) f (k _ ...))
              (=> failed!)
              (cond
               ((and tail-body? (not (kont-eq? k prim:return)) (kont-eq? kont k))
                (cps-property-set! tail 'tail-call #t)
                (tco tail #t))
               (else (failed!))))
             (else (tco tail)))))))
  (match expr
    (($ lambda/k _ _ (or ($ app/k _ k2 _) ($ seq/k _ (($ app/k _ k2 _)))))
     (let* ((e (match (lambda/k-body expr)
                 ((? app/k? a) a)
                 (($ seq/k _ ((? app/k? a))) a)
                 (else (throw 'laco-error 'tco
                              "Invalid body pattern `~a'!"
                              (cps->expr (lambda/k-body expr))))))
            (tail? (kont-eq? (cps-kont expr) k2)))
       ;; CASE: (lambda (k args ...) (k ...)) -> tail-call
       (when tail?
         (cps-property-set! e 'tail-call #t))
       (lambda/k-body-set! expr (tco e tail?))
       expr))
    (($ seq/k _ exprs)
     (seq/k-exprs-set! expr (tag-tail-call! exprs))
     expr)
    (($ app/k ($ cps _ kont _ _) f (($ seq/k _ exprs)))
     (=> failed!)
     (when (null? exprs)
       (throw 'laco-error tco "BUG: empty seq/k!"))
     (let ((tail (car (list-tail exprs (1- (length exprs))))))
       (match tail
         (($ app/k _ _ (k _ ...))
          (cond
           ((and (kont-eq? f kont) (kont-eq? kont k))
            (seq/k-exprs-set! expr (map tco exprs))
            (tco (car (app/k-args expr)) tail-body?))
           (else (failed!))))
         (else (failed!)))))
    (($ app/k ($ cps _ kont _ _) f args)
     (cond
      ((and (current-def) tail-body? (kont-eq? (car args) kont))
       ;; (pk "case-1" (cps->expr expr))
       ;; (pk "current-def" (current-def))
       ;; (pk "cps->name" (cps->name f))
       ;; CASE:
       ;; 1. tail-body
       ;; 2. k is current-kont
       ;; 3. (f k args ...) -> tail-call or tail-rec
       ;; 4. (f (lambda ... g) ...) -> g is in closure, so g is not a tail-call
       ;; NOTE: If f is not recursive, then we don't tag it for TCO to not create
       ;;       a new stack frame, since the callee can't compute correct local-var
       ;;       offset because the information of the callee is unknown to caller.
       ;;       Fortunately, we can rely on function inlining to achive the same
       ;;       optimizing to avoid a stack frame.
       (when (eq? (current-def) (cps->name f))
         (tag-proper-tail-recursion! expr)))
      ((and tail-body? (kont-eq? kont f))
       ;; (pk "case-2" (cps->expr expr))
       ;; CASE (k args ...) -> tail-call
       (cps-property-set! expr 'tail-call #t)))
     (app/k-func-set! expr (tco f))
     (app/k-args-set! expr (map tco args))
     expr)
    (($ letfun/k ($ bind-special-form/k _ var value body))
     (parameterize ((current-def (id-name var)))
       (bind-special-form/k-value-set! expr (tco value tail-body?)))
     (bind-special-form/k-body-set! expr (tco body #t))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (tco (bind-special-form/k-value expr) tail-body?))
     (bind-special-form/k-body-set! expr (tco (bind-special-form/k-body expr) #t))
     expr)
    (($ lambda/k _ _ body)
     (cond
      ((and (seq/k? body) (not (null? (seq/k-exprs body))))
       (let* ((el (reverse (seq/k-exprs body)))
              (tail (car el))
              (rest (cdr el)))
         (seq/k-exprs-set! expr (cons (reverse (map tco rest))
                                      (tco tail #t)))))
      (else
       (lambda/k-body-set! expr (tco body #t))))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (tco cnd))
     (branch/k-tbranch-set! expr (tco b1 #t))
     (branch/k-fbranch-set! expr (tco b2 #t))
     expr)
    (($ assign/k _ v e)
     (assign/k-expr-set! expr (tco e))
     expr)
    (else expr)))

(define-pass tail-call-optimizing expr (tco expr))
