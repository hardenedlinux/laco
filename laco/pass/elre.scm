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

(define-module (laco pass elre)
  #:use-module (laco types)
  #:use-module (laco env)
  #:use-module (laco cps)
  #:use-module (laco pass)
  #:use-module (laco pass normalize)
  #:use-module (laco pass closure-conversion)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; Eliminate all the redundant code:
;; NOTE:
;; 1. We're not going to eliminate tail-call (return ...) in this pass.
;;    The left `return' should imply tail-call after elre, since `return` in non
;;    tail-calls are all eliminated in case-2.
;; 2. FIXME: Make sure all the left `return' are tail-calls, we need them for
;;    low-level TCO stack tweaking in LIR.

(define (eliminate-non-tail-return exprs)
  (let ((kont (current-kont)))
    (fold (lambda (x p)
            (match x
              (($ app/k _ f (arg))
               (=> failed!)
               (if (kont-eq? f kont)
                   (cons arg p)
                   (failed!)))
              (else (cons x p))))
          '() exprs)))

(define (elre expr)
  (match expr
    (($ seq/k _ (($ seq/k _ _)))
     ;; case-1: (begin (begin body ...)) -> (begin body ...)
     ;;
     ;; Of course we can make sure no redundant seq was introduced in the parser,
     ;; however, you have to consider if users do it in their code, such like:
     ;; (let ((...)) (begin body ...))
     (elre (car (seq/k-exprs expr))))
    (($ seq/k _ exprs)
     (cond
      ((= (length exprs) 1)
       (let ((e (car exprs)))
         (match e
           (($ app/k _ f (arg))
            (=> failed!)
            ;; case-2: (begin (kont single-expr)) -> single-expr
            (if (kont-eq? (current-kont) f)
                (elre arg)
                (failed!)))
           (else
            ;; case-3: (begin single-expr) -> single-expr
            (elre (car exprs))))))
      (else
       (parameterize ((current-kont (cps-kont expr)))
         (let ((ne (map elre (eliminate-non-tail-return (reverse! exprs)))))
           (seq/k-exprs-set! expr ne)
           expr)))))
    #;
    (($ app/k ($ cps _ kont _ _) k ((? branch/k? b)))
    ;; case-4: (k branch/k) -> branch/k
    (=> failed!)
    (cond
    ((kont-eq? kont k)
    (elre b))
    (else (failed!))))
    (($ app/k ($ cps _ kont name _) ($ lambda/k _ args1 body) args2)
     ;; case-5: ((lambda params body) args) -> body[params/args]
     (when (not (= (length args1) (length args2)))
       (throw 'laco-error elre "Arguments list isn't equal in lambda apply"))
     ;; NOTE:
     ;; 1. We eliminate lambda/k, so free-vars have to tweak.
     ;; 2.
     (elre (cfs body args1 (map elre args2))))
    ;; (($ app/k ($ cps _ kont _ _) f args)
    ;;  ;; case-5: (ret expr-contains-ret) -> (ret expr-without-ret)
    ;;  ;; Make sure every function has only one ret instruction
    ;;  ;; NOTE: There's only one argument for ret
    ;;  (=> failed!)
    ;;  (cond
    ;;   ((kont-eq? kont f)
    ;;    (if (reduce-ret?)
    ;;        (app/k-args-set! expr (map elre (cdr args)))
    ;;        (parameterize ((reduce-ret? #t))
    ;;          ))))
    ;;  )
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set! expr (elre (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set! expr (elre (bind-special-form/k-body expr)))
     expr)
    (($ app/k _ func args)
     (app/k-func-set! expr (elre func))
     (app/k-args-set! expr (map elre args))
     expr)
    (($ lambda/k _ _ body)
     (parameterize ((current-kont (cps-kont expr)))
       (lambda/k-body-set! expr (elre body)))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (elre cnd))
     (branch/k-tbranch-set! expr (elre b1))
     (branch/k-fbranch-set! expr (elre b2))
     expr)
    (else expr)))

(define-pass eliminate-redundant expr (elre expr))
