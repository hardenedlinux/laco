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

(define-module (laco pass closure-conversion)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco env)
  #:use-module (laco primitives)
  #:use-module (laco pass normalize)
  #:use-module (laco pass)
  #:use-module (laco records)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold-right)))

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

;; Closure design
;; Closures will try to capture all free-vars into env.
;;
;; Closure on stack
;; Instr -> create-closure-object
;; Free-vars are not actually captured, they’re referenced from stack.
;; The closure doesn’t escape.
;;
;; Closure on heap
;; Instr -> capture-closure-object
;; Free-vars are captured and stored into the heap.
;; The closure has escaped:
;; 1. Pass as argument to non-primitive (except for ret)
;; 2. Return from the scope where it was created
;;
;; Optimizing
;; 1. We can name the anonymous function, and lift it as a global in lambda-lifting.
;;    So that we don’t have to capture closure on heap. If we want to do this, all
;;    free-vars must be confirmed statically, say, all free-vars are not parameters
;;    of any upper closure. For closure returned from closure, it’s impossible to be
;;    confirmed statically.
;; 2. For function that is not passed as an argument, we've known all the call sites
;;    of it, this is called `known function'. We can pass all free-vars as arguments
;;    to it, so it's not a closure anymore.

(define *env-table* (make-hash-table))
(define (env-ref name) (hash-ref *env-table* name 'global))
(define (env-set! name e) (hash-set! *env-table* name e))

(define* (cc expr #:optional (mode 'normal) #:key (finish? #f))
  ;;(pk "expr" (cps->expr expr)) (read)
  (match expr
    (($ app/k _ f (($ lambda/k ($ cps _ kont name attr) (k) body) args ...))
     (=> failed!)
     ;; Flatten args in application
     (let* ((frees (fix-fv (free-vars expr #t)))
            (env (new-env (id-name name) (list k) frees)))
       (extend-env! (current-env) env)
       (parameterize ((current-env env)
                      (current-kont name))
         (env-set! (id-name name) env)
         (cond
          ((assoc-ref attr 'binding)
           ;;(clean-tail-call! expr)
           (env-local-push! (current-env) k)
           (let ((value (new-app/k (cc f) (map cc `(,prim:restore ,@args))
                                   #:attr '((keep-result? #t)))))
             #;(make-seq/k (list kont name attr) `(,value ,(cc body)))
             (cc (cfs body (list k) (list value)))))
          (else (failed!))))))
    (($ lambda/k ($ cps _ kont name attr) args body)
     (let* ((frees (fix-fv (free-vars expr #t)))
            (env (new-env (id-name name) args frees))
            (def (assoc-ref attr 'def)))
       (extend-env! (current-env) env)
       (closure-set! (id-name name) env)
       (parameterize ((current-env env)
                      (current-kont name)
                      (current-def (or (has-renamed? def) def (current-def))))
         (env-set! (id-name name) env)
         (case mode
           ((normal)
            (if (is-escaped? expr)
                (make-closure/k (list kont name attr) env (cc body 'closure))
                (make-lambda/k (list kont name attr) args (cc body))))
           ((closure closure-in-pcall)
            ;; NOTE:
            ;; 1. Counting frame size for each closure env in lir
            ;; 2. For 'closure mode, fvars must be converted to lvar
            (make-closure/k (list kont name (if (eq? mode 'closure-in-pcall)
                                                (assoc-set! attr 'closure-in-call #t)
                                                attr))
                            env (cc body 'closure)))
           (else (throw 'laco-error cc "Invalid cc mode `~a'~%" mode))))))
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
     (let ((env (if (toplevel? (current-env))
                    (new-env (id-name name) '() (fix-fv (free-vars expr)))
                    (current-env))))
       (make-collection/k (list kont name attr)
                          var type size
                          (map cc value))))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (make-seq/k (list kont name attr) (map cc exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname func body))
     (let ((env (if (toplevel? (current-env))
                    (new-env (id-name name) '() (fix-fv (free-vars expr)))
                    (current-env))))
       (env-local-push! env fname)
       (parameterize ((current-env env))
         (env-set! (id-name name) env)
         (make-letfun/k (list kont name attr)
                        fname
                        (cc func)
                        (cc body)))))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
     ;; 1. In closure-conversion, we eliminate all letcont/k, the bindings will be
     ;;    merged into the current-env.
     ;; 2. For non-escaping situation, we perform inline to eliminate letcont/k.
     ;;    For escaping, we will convert the escaped closure to closure/k.
     ;; 3. I was considering to perform assignment-elimination to transform all
     ;;    local assignments to bindings. However, it's not good for embedded
     ;;    system, since every assignment increases stack. So we still make
     ;;    assignment an instruction.
     ;; 4. Although we can set bindings to global, and it's safe because of
     ;;    alpha-renaming, however, it can't be recycled by GC when the scope ends.
     ;; 5. Don't create env here.

     ;; cases:
     ;; 1. let-binding:
     ;;    (letcont/k ((j (lambda (x) jbody)))
     ;;      (j const-or-value-form))
     ;;    ==> (begin
     ;;          const-or-nonfunc as new-local
     ;;          jbody[k/new-local])
     ;;   NOTE: For function binding, we have letfun/k
     ;;   E.g: (let ((x (list-ref '(1 2 3) 0)))
     ;;          (display x))
     ;;        ==> (letcont/k ((j (lambda (x0) (display x0))))
     ;;              (j (list-ref '(1 2 3) 0)))
     ;;        ==> (begin
     ;;              (list-ref '(1 2 3) 0) as local-0
     ;;              (display local-0))
     ;;   NOTE: This conversion is necessary for appplicable-order before CFS.
     ;;
     ;; 2. let-binding evaluated value of a func
     ;;    (letcont/k ((j (lambda (x0) jbody)))
     ;;      (func[k] j))
     ;;   E.g: (let ((x (func)))
     ;;         (display x))
     ;;        ==> (letcont/k ((j (lambda (x0) (display x0))))
     ;;              (func[k] j))
     ;;   NOTE: func[k] means CPS-converted func
     ;;
     ;; 3. common CPS
     ;;    (letcont/k ((j (lambda (k) jbody))) (_ j args))
     ;;
     ;; 4. Sequence
     ;;    (letcont/k ((j (begin jbody))) (begin j exprs))

     (match expr
       (($ letcont/k ($ bind-special-form/k _ jname
                        ($ lambda/k _ (jargs) jbody)
                        ($ app/k _ _ ((? (lambda (v)
                                           (and (not (id-eq? v jname))
                                                (not (lambda/k? v))))
                                         arg)))))
        ;; For let bindings
        (let ((def (current-def))
              (tmpvar (if (toplevel? (current-env))
                          (new-id "#global-tmp-")
                          (new-id "#local-tmp-"))))
          (when (not (toplevel? (current-env)))
            (env-local-push! (current-env) tmpvar)
            (when def
              ;; If the current-def was registered as a named-let var, then update
              ;; its local name
              (renamed-update! (after-rename def) (id-name tmpvar))
              (set! def (after-rename def))))
          (parameterize ((current-def def))
            (cc
             (make-seq/k
              (list kont name attr)
              (list
               arg
               (cfs jbody
                    (list jargs)
                    (list tmpvar))))))))
       (else (cc (cfs body (list jname) (list jcont))))))
    (($ letval/k ($ bind-special-form/k ($ cps _ kont name attr) var value body))
     (env-local-push! (current-env) var)
     (make-letval/k (list kont name attr)
                    var
                    (cc value)
                    (cc body)))
    (($ app/k _ ($ lambda/k ($ cps _ kont name attr) args body) es)
     (cond
      ((null? args)
       (cc (make-seq/k (list kont name attr) (append es (list body)))))
      ((is-effect-var? (id-name (car args)))
       (let ((env (if (toplevel? (current-env))
                      (new-env (id-name name) '() (fix-fv (free-vars body)))
                      (current-env))))
         (extend-env! (current-env) env)
         (env-local-push! env (car args))
         (parameterize ((current-env env))
           (cc (make-seq/k (list kont name (assoc-set! attr 'env env))
                           (append es (list body)))))))
      (else (cc (cfs body args es)))))
    (($ app/k _ ($ lambda/k _ args ($ seq/k ($ cps _ kont name attr) exprs)) es)
     (cond
      ((is-effect-var? (id-name (car args)))
       (let ((env (new-env (id-name name) '() (fix-fv (free-vars expr)))))
         (env-local-push! env (car args))
         (parameterize ((current-env env))
           (make-seq/k
            (list kont name attr)
            ;; TODO: Seperate args and locals, and when we push locals,
            ;;       we have to fix the vm->sp.
            `(,(new-local (car args) (cc (car es)))
              ,@(map cc exprs))))))
      (else
       (cc (cfs (lambda/k-body (app/k-func expr)) args es)))))
    (($ app/k ($ cps _ kont name attr) f args)
     (let ((new-attr (if (kont-eq? kont f)
                         (assoc-set! attr 'keep-result? #t)
                         attr))
           (def (after-rename (current-def))))
       (when (and def
                  (eq? def (id-name f))
                  (is-tmp-var? (id-name f)))
         ;; If these conditions are met, then it's a recursive named-let call:
         ;; 1. The var is inside a closure
         ;; 2. The var is equal to the current-def
         ;; 3. The var was renamed as a local-tmp-var
         ;; NOTE: We have to record it here, since it should be kept as free-var in
         ;;       closure-capture-fv.
         (named-let-register! (id-name f)))
       (make-app/k (list kont name new-attr)
                   (cc f)
                   (map (lambda (e) (cc e (if (and (primitive? f)
                                                   (lambda/k? e))
                                              'closure-in-pcall
                                              'closure)))
                        args))))
    (($ assign/k _ v e)
     (assign/k-var-set! expr (cc v))
     (assign/k-expr-set! expr (cc e))
     expr)
    (else expr)))

(define (var-conversion expr)
  (match expr
    ((? id? id)
     (let* ((env (current-env))
            (current-kont-label (cps->name-string (current-kont)))
            ;; FIXME: deal with it when current-kont is 'global
            (name (id-name id)))
       (pk "vc kont" (current-kont) (id-name (current-kont)) current-kont-label) (read)
       (cond
        ((top-level-ref name) (new-gvar id)) ; check if it's global
        ((not (toplevel? env)) ; check if it's local var
         (cond
          ((bindings-index env id)
           => (lambda (offset)
                (new-lvar id offset)))
          ((and (not (string=? current-kont-label "global"))
                ;; check if it's free var
                (is-free-var? env id)
                (frees-index env id))
           values => (lambda (scope index)
                       (new-fvar id (env->name-string scope) index)))
          (else (throw 'laco-error var-conversion
                       "Undefined local variable `~a'!" name))))
        (else (throw 'laco-error var-conversion
                     "Undefined global variable `~a' in `~a'!"
                     name current-kont-label)))))
    (($ closure/k _ env body)
     (parameterize ((current-env (env-ref (cps->name expr)))
                    (current-kont (cps-kont expr)))
       ;; (pk "current-name" (id-name (cps-name expr))) (read)
       ;; (pk "current-kont" (id-name (current-kont))) (read)
       (closure/k-body-set! expr (var-conversion body)))
     expr)
    (($ app/k _ f args)
     (app/k-func-set! expr (var-conversion f))
     (app/k-args-set! expr (map var-conversion args))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr (var-conversion (bind-special-form/k-value expr)))
     (bind-special-form/k-body-set!
      expr (var-conversion (bind-special-form/k-body expr)))
     expr)
    (($ seq/k ($ cps _ _ _ attr) exprs)
     (cond
      ((assoc-ref attr 'env)
       => (lambda (env)
            (parameterize ((current-env env))
              (scoped-label! (cps->name-string expr))
              (seq/k-exprs-set! expr (map var-conversion exprs)))))
      (else (seq/k-exprs-set! expr (map var-conversion exprs))))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (var-conversion cnd))
     (branch/k-tbranch-set! expr (var-conversion b1))
     (branch/k-fbranch-set! expr (var-conversion b2))
     expr)
    (($ lambda/k _ _ body)
     (parameterize ((current-env (env-ref (cps->name expr)))
                    (current-kont (cps-kont expr)))
       ;; (pk "111current-name" (id-name (cps-name expr))) (read)
       ;; (pk "111current-kont" (id-name (current-kont))) (read)
       (lambda/k-body-set! expr (var-conversion body)))
     expr)
    (($ collection/k _ _ _ _ value)
     (collection/k-value-set! expr (map var-conversion value))
     expr)
    (($ assign/k _ v e)
     (assign/k-var-set! expr (var-conversion v))
     (assign/k-expr-set! expr (var-conversion e))
     expr)
    ((? id?) expr)
    (else expr)))

(define-pass closure-conversion expr (var-conversion (cc expr)))
