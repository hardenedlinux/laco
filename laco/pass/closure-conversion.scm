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

(define-module (laco pass closure-conversion)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco env)
  #:use-module (laco primitives)
  #:use-module (laco pass normalize)
  #:use-module (laco pass)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold-right))
  #:use-module ((rnrs) #:select (define-record-type)))

;; NOTE:
;; 1. We only perform CC after DCE, so there's no unused binding.
;; 2. We distinct local bindings and free vars. Both of them are ordered in a
;;    queue. This is useful when we perform linearization for codegen.
;; 3. We use flat-closure design, which put all free variables with values in
;;    a single record of the function. This can save some RAMs for embedded
;;    system.
;; 4. According to Appel's book, we must perform heap-exhausted test. However,
;;    we leave this duty to the VM when it calls procedure each time. This may
;;    save some RAMs compared to the code injection.
;; 5. CPS has no explicit loops, this may cause redundant heap-exhausted test.
;;    We may do specific optimizings for tail call in the future.
;; 6. Different from the passes, we use CPS constructor here for taking advantage of
;;    type checking in record type.


;; NOTE: Filter global var
(define (fix-fv fl)
  (fold-right (lambda (x p)
                (if (top-level-ref (id-name x))
                    p
                    (cons x p)))
              '() fl))

(define* (cc expr #:optional (mode 'normal))
  (match expr
    (($ lambda/k ($ cps _ kont name attr) args body)
     (let ((env (new-env args (fix-fv (free-vars expr)))))
       (pk "fix-fv" (map id-name (queue-slots (env-frees env))))
       (extend-env! (current-env) env)
       (closure-set! (id-name name) env)
       (parameterize ((current-env env)
                      (current-kont kont))
         (make-lambda/k (list kont name attr) args (cc body))))
     ;; TODO:
     ;; 1. recording the current bindings by the label to lookup table
     ;; 2. replacing all the appeared free variable to `fvar' by label and order num
     ;; 3. counting frame size for each closure env in lir
     )
    ;; (($ closure/k ($ cps _ kont name attr) env body)
    ;;  ;; TODO: The escaping function will be converted to closure/k.
    ;;  ;;       This will need escaping analysis or liveness analysis.
    ;;  (closure-set! (id-name name) (current-env))
    ;;  (parameterize ((current-env env))
    ;;    (make-closure/k (list kont name attr) (current-env) (cc body))))
    (($ branch/k ($ cps _ kont name attr) cnd b1 b2)
     (make-branch/k (list kont name attr)
                    (cc cnd)
                    (cc b1)
                    (cc b2)))
    (($ collection/k ($ cps _ kont name attr) var type size value)
     (env-local-push! (current-env) var)
     (make-collection/k (list kont name attr)
                        var type size
                        (cc value)))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (make-seq/k (list kont name attr) (map cc exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname func body))
     (env-local-push! (current-env) fname)
     (make-letfun/k (list kont name attr)
                    fname
                    (cc func)
                    (cc body)))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
     ;; 1. In closure-conversion, we eliminate all letcont/k, the bindings should be
     ;;    merged into the current-env.
     ;; 2. For non-escaping situation, we perform inline to eliminate letcont/k.
     ;;    For escaping, we will convert the escaped closure to closure/k.
     ;; 3. For side-effect cases, we perform assignment-elimination (TODO).
     ;; 4. Although we can set bindings to global, it's safe because of
     ;;    alpha-renaming, however, it can't be released when the scope came to end.
     (let ((env (if (toplevel? (current-env))
                    (new-env '() (free-vars expr))
                    (current-env))))
       (when (toplevel? (current-env))
         (extend-env! (current-env) env)
         (closure-set! (id-name name) env))
       (env-local-push! env jname)
       (parameterize ((current-env env))
         (cc (cfs body
                  (list jname)
                  (list (cc jcont)))))))
    (($ letval/k ($ bind-special-form/k ($ cps _ kont name attr) var value body))
     (env-local-push! (current-env) var)
     (make-letval/k (list kont name attr)
                    var
                    (cc value)
                    (cc body)))
    (($ app/k ($ cps _ kont name attr) f args)
     (make-app/k (list kont name attr)
                 (cc f)
                 (map cc args)))
    ((? id? id)
     (let ((env (current-env))
           ;; FIXME: deal with it when current-kont is 'global
           (label (cps->name-string (current-kont)))
           (name (id-name id)))
       (cond
        ((top-level-ref name) (new-gvar id))
        ((not (toplevel? env))
         (cond
          ((bindings-index env id)
           => (lambda (offset)
                (new-lvar id offset)))
          ((and (not (eq? label 'global)) (frees-index env id))
           => (lambda (index)
                (new-fvar id label index)))
          (else (throw 'laco-error cc "Undefined local variable `~a'!" label))))
        (else (throw 'laco-error cc "Undefined global variable `~a' in `~a'!"
                     name label)))))
    (else expr)))

(define-pass closure-conversion expr (cc expr))
