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

(define-module (laco pass tco)
  #:use-module (laco utils)
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

(define (no-proc-in-list? lst)
  (every (lambda (e)
           (not (or (lambda/k? e) (letfun/k? e) (closure/k? e))))
         lst))

(define (is-global-defined-func)
  (eq? (current-kont 'global)))

(define inside-closure? (make-parameter #f))

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
               ((and tail-body?
                     ;; NOTE: tail-call must be the leaf node
                     (no-proc-in-list? (app/k-args tail))
                     (not (kont-eq? k prim:return)) (kont-eq? kont k)
                     (not (inside-closure?))
                     (not (is-ptc? (cps->name f))))
                (cps-property-set! tail 'tail-call #t)
                (tco tail #t))
               (else (failed!))))
             (else (tco tail)))))))
  ;;(pk "expr" (cps->expr expr))
  (match expr
    (($ lambda/k _ _ (or ($ app/k _ k2 args) ($ seq/k _ (($ app/k _ k2 args)))))
     (let* ((e (match (lambda/k-body expr)
                 ((? app/k? a) a)
                 (($ seq/k _ ((? app/k? a))) a)
                 (else (throw 'laco-error 'tco
                              "Invalid body pattern `~a'!"
                              (cps->expr (lambda/k-body expr))))))
            (tail? (kont-eq? (cps-kont expr) k2)))
       ;; CASE: (lambda (k args ...) (k ...)) -> tail-call
       (when (and tail? (not (is-ptc? (cps->name e)))
                  ;; NOTE: tail-call must be the leaf node
                  (no-proc-in-list? args)
                  (not (inside-closure?)))
         (cps-property-set! e 'tail-call #t))
       (cond
        ((closure-was-named-let? expr)
         => (lambda (def)
              (let ((renamed-def (after-rename def)))
                (parameterize ((current-def renamed-def))
                  ;; (pk "-----000000000000000" def renamed-def)
                  ;; (read)
                  (lambda/k-body-set! expr (tco e #t))))))
        (else
         (lambda/k-body-set! expr (tco e tail?))))
       expr))
    (($ lambda/k _ _ body)
     (=> failed!)
     (cond
      ((closure-was-named-let? expr)
       => (lambda (def)
            (let ((renamed-def (after-rename def)))
              (parameterize ((current-def renamed-def))
                ;; (pk "-----000000000000000" def renamed-def)
                ;; (read)
                (lambda/k-body-set! expr (tco body #t))))))
      ((and (seq/k? body) (not (null? (seq/k-exprs body))))
       (let* ((el (reverse (seq/k-exprs body)))
              (tail (car el))
              (rest (cdr el)))
         (seq/k-exprs-set! expr (cons (reverse (map tco rest))
                                      (tco tail #t)))))
      (else (lambda/k-body-set! expr (tco body #t))))
     expr)
    (($ seq/k _ exprs)
     (if (and tail-body?
              (not (inside-closure?)))
         (seq/k-exprs-set! expr (tag-tail-call! exprs))
         (seq/k-exprs-set! expr (map tco exprs)))
     expr)
    (($ app/k ($ cps _ kont _ _) f (($ seq/k _ exprs)))
     (=> failed!)
     (when (null? exprs)
       (throw 'laco-error tco "BUG: empty seq/k!"))
     (when (and tail-body?
                (eq? (current-def) (cps->name f))
                (not (inside-closure?))
                (not (is-ptc? (cps->name f))))
       (cps-property-set! expr 'tail-call #t))
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
      ((and tail-body? (eq? (current-def) (cps->name f)))
       ;; CASE:
       ;; (let lp (...)
       ;;   (lp args ...))
       (ptc-register! (cps->name f))
       (tag-proper-tail-recursion! expr))
      ((and (current-def) tail-body? (kont-eq? (car args) kont))
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
         (ptc-register! (cps->name f))
         (tag-proper-tail-recursion! expr)))
      ((and tail-body? (kont-eq? kont f))
       ;; (pk "case-2" (cps->expr expr))
       ;; CASE (k args ...) -> tail-call
       ;; TODO: There're only limited cases that can set tail-call.
       ;;       1. No any locals
       ;;       2. In tail body
       ;;(cps-property-set! expr 'tail-call #t)
       #t))
     (app/k-func-set! expr (tco f))
     (app/k-args-set! expr (map tco args))
     expr)
    (($ letfun/k ($ bind-special-form/k _ var value body))
     (parameterize ((current-def (id-name var))
                    (inside-closure? #t))
       (bind-special-form/k-value-set! expr (tco value tail-body?))
       (bind-special-form/k-body-set! expr (tco body #t)))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (tco (bind-special-form/k-value expr) tail-body?))
     (bind-special-form/k-body-set! expr (tco (bind-special-form/k-body expr) #t))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (tco cnd))
     (branch/k-tbranch-set! expr (tco b1 #t))
     (branch/k-fbranch-set! expr (tco b2 #t))
     expr)
    (($ assign/k _ v e)
     ;; NOTE: The local def will definitely be converted to letrec/letrec*, so it
     ;;       appears in the operand of assignment. We can quickly confirm if the
     ;;       tail context is inside a closure.
     ;; NOTE: We prevent to emit tail-call inside a closure since it's a little hard
     ;;       to confirm if it's a leaf. But PTC is relatively easier to emit from
     ;;       a closure.
     (parameterize ((inside-closure? #f))
       (assign/k-expr-set! expr (tco e)))
     expr)
    (else expr)))

(define-pass tail-call-optimizing expr (tco expr))
