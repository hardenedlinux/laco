;;  Copyright (C) 2020-2021
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (laco env)
  #:use-module (laco utils)
  #:use-module (laco ast)
  #:use-module (laco types)
  #:use-module (laco primitives)
  #:use-module (laco records)
  #:export (env
            env?
            env-name env->name-string
            env-remove-fvar!
            env-remove-lvar!
            env-bindings env-bindings-set!
            env-prev env-prev-set!
            env-frees env-frees-set!

            toplevel?
            top-level-ref
            top-level-set!
            top-level-delete!
            top-level-for-each
            top-level->body-list

            extend-env!
            bindings-index
            frees-index
            binding-exists?
            is-free-var?

            *top-level*
            new-env
            env-local-push!
            env->args

            closure-set!
            closure-ref

            register-as-recursive!
            is-recursive?

            lambda-has-vargs!
            lambda-has-vargs?

            no-free-var?))

;; NOTE:
;; 1. Only toplevel is used for storing actual value.
;; 2. We use closures to manage bindings, the env is defined for conversion.
;; 3. You can check existance in env, not referring the value from it.

(define-typed-record env
  (fields
   (name symbol?)
   (prev env? not)
   (bindings queue? not)
   (frees queue? not)))

(define-record-type toplevel (parent env) (fields definition))
(define (new-toplevel)
  (make-toplevel 'global #f #f #f (make-hash-table)))

(define* (new-env name #:optional (params '()) (frees '()))
  ;;(pk "new-env" name (map id-name params) (map id-name frees)) (read)
  (let ((bindings (list->queue params))
        (frees (list->queue frees)))
    (make-env name #f bindings frees)))

(define (env->name-string env)
  (symbol->string (env-name env)))

(define (env-remove-var! env var getter setter)
  (let ((vl (queue-slots (getter env))))
    (setter
     env
     (list->queue (filter (lambda (v) (not (id-eq? v var))) vl)))))

(define (env-remove-fvar! env fv)
  (env-remove-var! env fv env-frees env-frees-set!))

(define (env-remove-lvar! env lv)
  (env-remove-var! env lv env-bindings env-bindings-set!))

(define *top-level* (new-toplevel))

(define* (top-level->body-list #:optional (proc (lambda (v _) v)))
  (hash-map->list proc (toplevel-definition *top-level*)))

(define (top-level-ref k)
  (hash-ref (toplevel-definition *top-level*) k))

(define (top-level-set! k v)
  (hash-set! (toplevel-definition *top-level*) k v))

(define (top-level-delete! k)
  (hash-remove! (toplevel-definition *top-level*) k))

(define (top-level-for-each proc)
  (hash-for-each proc (toplevel-definition *top-level*)))

(define (extend-env! prev new)
  (env-prev-set! new prev))

(define (id-index env ref id)
  (and (env? env)
       (slot-index (ref env) (lambda (x) (id-eq? x id)))))

(define (bindings-index env k)
  ;; (pk "env name" (env-name env))
  ;; (pk "bindings" (map (lambda (x) ((if (id? x) id-name primitive-name) x)) (car (env-bindings env))) (id-name k))
  ;; (read)
  (id-index env env-bindings k))

(define (is-free-var? env k)
  ;;(pk "frees" (map (lambda (x) ((if (id? x) id-name primitive-name) x)) (car (env-frees env))) (id-name k))
  (id-index env env-frees k))

(define (frees-index env k)
  (let ((prev (env-prev env)))
    (cond
     ((and prev (not (toplevel? prev)))
      (cond
       ((bindings-index prev k)
        => (lambda (i) (values prev i)))
       (else (frees-index prev k))))
     (else #f))))

(define (binding-exists? env id)
  (let ((bindings (env-bindings env))
        (prev (env-prev env))
        (pred (lambda (x) (id-eq? x id))))
    (or (and bindings (slot-index bindings id))
        (and prev (binding-exists? prev id))
        (top-level-ref id))))

(define (env-ref env id)
  (binding-exists? env id))

(define (env-local-push! env id)
  (let ((bindings (env-bindings env)))
    (cond
     (bindings (queue-in! bindings id))
     (else
      (throw 'laco-error env-local-push! "Invalid local var `~a' in `~a'!"
             (id-name id) bindings)))))

(define (env->args env)
  ;;(hash-map->list (lambda (k _) k) (env-bindings env))
  (queue-slots (env-bindings env)))

(define *closure-lookup-table* (make-hash-table))
(define (closure-set! label bindings)
  (hash-set! *closure-lookup-table* label bindings))
(define (closure-ref label)
  ;; FIXME: Shouldn't create new env
  (hash-ref *closure-lookup-table* label (new-env label '())))

;; NOTE: We record all recursive functions in this table, rather than tag it in attr
;;       of each CPS node, since the CPS node may be eliminated
(define *recursive-table* (make-hash-table))
(define (register-as-recursive! sym)
  (hash-set! *recursive-table* sym #t))
(define (is-recursive? sym)
  (hash-ref *recursive-table* sym))

(define *lambda-table* (make-hash-table))
(define (lambda-has-vargs! sym val)
  (hash-set! *lambda-table* sym val))
(define (lambda-has-vargs? sym)
  (hash-ref *lambda-table* sym))

(define (no-free-var? env)
  (queue-empty? (env-frees env)))
