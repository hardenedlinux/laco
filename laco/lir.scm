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

(define-module (laco lir)
  #:use-module (laco utils)
  #:use-module (laco env)
  #:use-module (laco cps)
  #:use-module (laco types)
  #:use-module (laco object)
  #:use-module (laco primitives)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (laco records)
  #:export (make-insr
            insr?

            insr-proc insr-proc?
            make-insr-proc
            insr-proc-proc
            insr-proc-label insr-proc-label-set!
            insr-proc-env
            insr-proc-arity
            insr-proc-body insr-proc-body-set!

            insr-lit insr-lit?
            make-insr-lit
            insr-lit-val

            insr-ref insr-ref?
            make-insr-ref
            insr-ref-var

            insr-set insr-set?
            make-insr-set
            insr-set-var

            insr-pcall insr-pcall?
            make-insr-pcall
            insr-pcall-op

            insr-prelude insr-prelude?
            make-insr-prelude
            insr-prelude-proc
            insr-prelude-mode

            insr-fjump insr-fjump?
            make-insr-fjump
            insr-fjump-label insr-fjump-label-set!

            insr-jump insr-jump?

            insr-proc-call insr-proc-call?
            make-insr-proc-call
            insr-proc-call-proc insr-proc-call-proc-set!
            insr-proc-call-label insr-proc-call-label-set!

            insr-label insr-label?
            make-insr-label
            insr-label-label insr-label-label-set!
            insr-label-proc insr-label-proc-set!
            insr-label-body insr-label-body-set!

            insr-branch-end insr-branch-end?

            insr-local insr-local?
            make-insr-local
            insr-local-label
            insr-local-name
            insr-local-offset insr-local-offset-set!
            insr-local-mode insr-local-mode-set!

            insr-free insr-free?
            make-insr-free
            insr-free-label insr-free-label-set!
            insr-free-offset insr-free-offset-set!
            insr-free-name insr-free-name-set!

            insr-global insr-global?
            make-insr-global
            insr-global-name
            insr-global-mode

            insr-global-call insr-global-call?
            make-insr-global-call
            insr-global-call-name
            insr-global-call-label

            insr-closure insr-closure?
            make-insr-closure
            insr-closure-label insr-closure-label-set!
            insr-closure-arity
            insr-closure-frees
            insr-closure-body insr-closure-body-set!
            insr-closure-mode

            insr-assign insr-assign?
            make-insr-assign
            insr-assign-var insr-assign-var-set!
            insr-assign-expr insr-assign-expr-set!

            *proc-return*

            label-ref
            cps->lir
            cps->lir/g
            lir->expr
            lir->expr/g))

;; Instruction (insr) is a simple low-level IR, which is a instruction set of
;; ACM (Abstract Continuation Machine).
;; The ACM program is a linear sequence of instructions, labels, and literal data.
;; It is essentially an assembly-language program.
;;
;; TODO: The ACM is stackVM. We hope to refactor it as registerVM in the future.
;;
;; The features of insr-ir:
;; 1. All continuations are mapping to labels which is actually kont-name.
;; 2. No seq abstract, it's merged into procedure.
;; 3. Branches are replaced with comparison and jumping.
;; 4. No ref abstract, its value is stored into a lookup table, and replace with
;;    lookup by offset.
;; 5. The inner defined functions are all sorted as closures.
;; 6. The procedure must be defined in toplevel.
;; 7. No recursive, the CPS tree is converted to flat instruction queue by the invoking order.
;;
;;
;; The optimizings in LIR:
;; 1. Integer unboxing
;;    TODO: The independent const integer can be unboxed from object struct,
;;          however, we have to substitute all the references of that const integer.
;; 2. Memory layout optimizing
;; 3. Stackwise optimizing
;; 4. Collection/struct access optimizing
;; 5. Combine redundant labels

(define-record-type insr)

(define (valid-insr-list? lst)
  (make-object-list-pred lst (lambda (x) (or (insr? x) (object? x)))))

(define-typed-record insr-proc (parent insr)
  (fields
   (proc string? not)
   (label string?) ; entry should be a label
   (env env?)
   (arity integer?)
   (body valid-insr-list?)))

(define-typed-record insr-label (parent insr)
  (fields
   (proc string? not)
   (label string?)
   (body valid-insr-list?)))

(define-typed-record insr-branch-end (parent insr)
  (fields
   (label string?)))

(define-record-type insr-lit (parent insr) (fields val)) ; literal
(define-record-type insr-set (parent insr) (fields var)) ; var assignment

;; Push value to TOS
;; NOTE: the value must be encoded to an integer first.
(define-typed-record insr-push (parent insr)
  (fields
   (value integer?)))

;; Pop ss[offset] to TOS
(define-typed-record insr-local (parent insr)
  (fields
   (mode symbol?)
   (name symbol?)
   (offset integer? list?)
   (keep? boolean?)))

;; Pop ss[offset] to TOS
(define-typed-record insr-free (parent insr)
  (fields
   ;; We convert the label to string since we will generate label pattern in codegen
   (label string?)
   (name symbol?)
   (mode symbol?)
   (offset integer?)
   (keep? boolean?)))

;; Global variables are stored in a special area
(define-typed-record insr-global (parent insr)
  (fields
   (name symbol?)
   (label string?)))

;; Call global variable
(define-typed-record insr-global-call (parent insr)
  (fields
   (name symbol?)
   (label string?)))

;; Jump if TOS is false
(define-typed-record insr-fjump (parent insr)
  (fields
   (label string?)))

;; Jump without condition
(define-typed-record insr-jump (parent insr)
  (fields
   (label string?)))

;; call proc
(define-typed-record insr-proc-call (parent insr)
  (fields
   (proc string? not)
   (label string?)
   (keep? boolean?)))

;; This is for primitive calling
(define-typed-record insr-pcall (parent insr)
  (fields
   (op primitive?)
   (keep? boolean?)))

;; TODO:
;; 1. low 2bits should be mode
;; 2. high 6bits should be arity
(define-typed-record insr-prelude (parent insr)
  (fields
   (proc string?)
   (label string?)
   (mode integer?)
   (arity integer?)))

(define-typed-record insr-closure (parent insr)
  (fields
   (label string?)
   (arity integer?)
   (frees hash-table?)
   (body valid-insr-list?)
   (mode symbol?)))

(define-typed-record insr-assign (parent insr)
  (fields
   (var insr-free? insr-local? insr-global?)
   (expr insr? object?)))

(define (get-global-offset name)
  ;; TODO: compute the offset of the specified global var name
  0)

(define *labels* (make-hash-table))
(define (label-register! label lexpr)
  (hash-set! *labels* label lexpr))
(define (label-ref label)
  (hash-ref *labels* label))

;; NOTE:
;; 1. We use prim:restore for proc-return instruction, and the arity is 0.
;; 2. prim:return is used to tag tail-call, not for restoring context.
(define *proc-return* (make-insr-pcall '() prim:restore #t))

;; insr -> string
(define (insr->label insr)
  (match insr
    (($ insr-proc-call _ _ label _) label)
    (($ insr-free _ label _ _ _ _) label)
    (($ insr-local _ label _ _ _) (symbol->string label))
    (($ insr-global _ _ label) label)
    (($ insr-global-call _ _ label) label)
    (($ insr-label _ _ label _) label)
    (($ lambda/k ($ cps _ _ label _) _ _) (id->string label))
    (else "unknown")))

;; list -> hash-table
(define (frees->lookup-table frees)
  (let ((ht (make-hash-table))
        (len (length frees)))
    (for-each (lambda (x i)
                (hash-set! ht (id-name x) i))
              frees (iota len))
    ht))

(define* (cps->lir expr #:key (cur-def #f) (mode 'push) (keep-ret-context? #f))
  (match expr
    (($ lambda/k ($ cps _ kont name attr) args body)
     (let ((env (closure-ref (id-name name)))
           (label (id->string name)))
       (when (not env)
         (throw 'laco-error cps->lir
                "lambda/k: the closure label `~a' doesn't have an env!" label))
       (make-insr-proc
        '() cur-def label env (length args)
        (list (cps->lir
               body
               ;; If a lambda has closure-in-pcall flag, then it's a lifted lambda.
               ;; So we have to keep its return value on stack.
               ;; Please note that not all lifted-lambdas need to keep return value,
               ;; Only the lifted closure-in-pcall.
               #:keep-ret-context? (assoc-ref attr 'closure-in-pcall))
              *proc-return*))))
    (($ closure/k ($ cps _ kont name attr) env body)
     (let* ((mode (if (is-escaped? expr) 'heap 'stack))
            (locals (env-bindings env))
            (frees (frees->lookup-table (queue-slots (env-frees env))))
            (arity (queue-length (env-bindings env))))
       (make-insr-closure '()
                          (id->string name)
                          arity
                          frees
                          (list (cps->lir body) *proc-return*)
                          mode)))
    (($ branch/k ($ cps _ kont name attr) cnd b1 b2)
     (define (->b b)
       (make-insr-label '() #f (cps->name-string b) (list (cps->lir b))))
     (let* ((ce (cps->lir cnd #:keep-ret-context? #t))
            (bt (if (constant/k? b1) (->b b1) (cps->lir b1)))
            (bf (if (constant/k? b2) (->b b2) (cps->lir b2)))
            (label (id->string name))
            (end-label (new-label "fjump-end-"))
            (jump-to-branch-end (make-insr-jump '() end-label))
            (branch-end (make-insr-branch-end '() end-label)))
       (make-insr-label
        '()
        #f
        label
        (list
         ce
         (make-insr-fjump '() (cps->name-string b2))
         bt
         jump-to-branch-end
         bf
         branch-end))))
    (($ collection/k ($ cps _ kont name attr) var type size value)
     (create-collection-object type size (map cps->lir value)))
    (($ seq/k ($ cps _ kont name attr) exprs)
     (make-insr-label '() #f (id->string name) (map cps->lir exprs)))
    (($ letfun/k ($ bind-special-form/k ($ cps _ kont name attr) fname fun body))
     ;; NOTE:
     ;; 1. For common function, after lambda-lifting, the function must be lifted to
     ;;    a function which can be looked up from top-level.
     ;; 2. For escaping function, there must be a closure. So we will take advantage
     ;;    of the specific instruction of the VM.
     (let* ((label (id->string name))
            (cont (cps->lir body))
            (insr (make-insr-label '() (id->string fname) label (cps->lir fun))))
       (cond
        ((insr? cont)
         (label-register! label (make-insr-label '() #f label (list insr cont))))
        ((list? cont)
         (label-register! label (make-insr-label '() #f label `(,insr ,@cont))))
        (else (throw 'laco-error cps->lir "Invalid cont `~a' in letfun/k!" cont)))
       ;; TODO:
       ;; Don't forget this is based on lambda-lifting that we haven't done.
       ))
    (($ letcont/k ($ bind-special-form/k ($ cps _ kont name attr) jname jcont body))
     ;; NOTE: All the inside-defined bindings are flattened, and pushed into the env
     ;;       frame of the function.
     ;; TODO: The ref checking should be in closure-conversion.
     (throw 'laco-error cps->lir "letcont hasn't implemented yet!" expr))
    (($ letval/k ($ bind-special-form/k
                    ($ cps _ kont name attr) var ($ constant/k _ value) body))
     ;; NOTE: value is constant type.
     ;; NOTE: char and boolean shouldn't be unboxed.
     (let ((obj (create-constant-object value))
           (cont (cps->lir body))
           (label (id->string name)))
       ;; TODO: substitute all the var reference to the ref-number
       (cond
        ((insr? cont)
         (label-register! label (make-insr-label '() #f label (list obj cont))))
        ((list? cont)
         (label-register! label (make-insr-label '() #f label `(,obj ,@cont))))
        (else (throw 'laco-error cps->lir "Invalid cont `~a' in letval/k!" cont)))))
    (($ app/k ($ cps _ kont name attr) func args)
     ;; NOTE: After normalize, the func never be anonymous function, it must be
     ;;       an id.
     (let* ((keep? (if (assoc-ref attr 'keep-result?) #t keep-ret-context?))
            (f (cps->lir func #:mode 'call #:keep-ret-context? keep?))
            (e (map (lambda (e) (cps->lir e #:keep-ret-context? #t)) args))
            (env (closure-ref (id-name name)))
            (label (id->string name))
            (arity (length args))
            (func-name (cps->name-string func))
            (prelude (lambda (mode)
                       (let ((f-label (insr->label f)))
                         (when (= mode *normal-call*)
                           (normal-call-register! f-label))
                         (make-insr-prelude '() (cps->name-string func)
                                            f-label
                                            mode arity)))))
       (when (not env)
         (throw 'laco-error cps->lir
                "app/k: the closure label `~a' doesn't have an env!" label))
       (if (insr-pcall? f)
           (make-insr-label '() #f label `(,@e ,f))
           (cond
            ((is-proper-tail-recursion? expr)
             (make-insr-label '() #f label `(,(prelude *tail-rec*) ,@e ,f)))
            ((is-tail-call? expr)
             (make-insr-label '() #f label `(,(prelude *tail-call*) ,@e ,f)))
            (else
             (make-insr-label '() #f label `(,(prelude *normal-call*) ,@e ,f)))))))
    (($ assign/k _ v e)
     (make-insr-assign '() (cps->lir v) (cps->lir e)))
    (($ constant/k _ value)
     (create-constant-object value))
    (($ local ($ id _ name _) value)
     (cps->lir value))
    (($ lvar ($ id _ name _) offset)
     (make-insr-local '() name mode offset
                      (and (eq? mode 'call) keep-ret-context?)))
    (($ fvar ($ id _ name _) label offset)
     (make-insr-free '() label name mode offset
                     (and (eq? mode 'call) keep-ret-context?)))
    (($ gvar ($ id _ name _))
     (cond
      ((top-level-ref name)
       => (lambda (g)
            (let ((label (insr->label g)))
              (case mode
                ((push) (make-insr-global '() name label))
                ((call) (make-insr-global-call '() name label))))))
      (else
       (throw 'laco-error cps->lir "Missing global var `~a'!" name))))
    ((? primitive? p)
     (case mode
       ((push) (make-prim-object '() p))
       ((call) (make-insr-pcall '() p keep-ret-context?))))
    (else (throw 'laco-error cps->lir "Invalid cps `~a'!" (id-name expr)))))

(define (cps->lir/g expr)
  (parameterize ((current-kont 'global))
    (top-level-for-each
     (lambda (v e)
       (top-level-set! v (cps->lir e #:cur-def (symbol->string v))))))
  (make-insr-proc '() "____principio" "#____principio"
                  (current-env) 0 (list (cps->lir expr))))

(define (lir->expr lexpr)
  (match lexpr
    (($ insr-closure _ label _ _ body mode)
     `(,(symbol-append 'closure-on- mode) ,label ,@(map lir->expr body)))
    (($ insr-proc _ proc label _ arity lexprs)
     `(proc ,proc ,label ,arity ,(map lir->expr lexprs)))
    (($ insr-label _ proc label exprs)
     `((label proc ,label)
       ,@(map lir->expr exprs)))
    (($ insr-pcall _ p keep?)
     `(prim-call ,(primitive-name p) ,(primitive->number p)
                 ,(if keep? 'keep 'clean)))
    (($ insr-prelude _ proc label mode arity)
     `(prelude ,proc ,label ,(mode->name mode) ,arity))
    (($ insr-proc-call _ proc label keep?)
     `(call-proc ,proc ,label ,(if keep? 'keep 'clean)))
    (($ insr-fjump _ label)
     `(fjump ,label))
    (($ insr-branch-end _ label)
     `(branch-end ,label))
    (($ insr-jump _ label)
     `(jump ,label))
    (($ insr-global _ name)
     `(global ,name))
    (($ insr-global-call _ name)
     `(global-call ,name))
    (($ insr-local _ name mode offset keep?)
     `(local ,(if (list? offset) (car offset) offset)
        ,name ,mode ,(cond
                      ((eq? mode 'push) 'object)
                      (keep? 'keep)
                      (else 'clean))))
    (($ insr-free _ label name mode offset keep?)
     `(free-var ,name in ,label ,mode ,offset ,(cond
                                                ((eq? mode 'push) 'object)
                                                (keep? 'keep)
                                                (else 'clean))))
    (($ insr-assign _ v e)
     `(assign ,(lir->expr v) ,(lir->expr e)))
    (($ integer-object _ value) `(integer ,value))
    (($ real-object _ value) `(real ,value))
    (($ rational-object _ value) `(rational ,value))
    (($ complex-object _ value) `(complex ,value))
    (($ string-object _ value) `(string ,value))
    (($ keyword-object _ value) `(keyword ,value))
    (($ char-object _ value) `(char ,value))
    (($ symbol-object _ value) `(symbol ,value))
    (($ boolean-object _ value) `(boolean ,(if value 'true 'false)))
    (($ prim-object _ p) `(primitive ,(primitive-name p)))
    (($ proc-object _ name arity entry) `(proc ,name ,arity ,entry))
    (($ list-object _ size value) `(list ,@(map lir->expr value)))
    (($ vector-object _ size value) `(vector ,@(map lir->expr value)))
    (($ pair-object _ _ vals) `(pair ,@(map lir->expr vals)))
    (else (throw 'laco-error lir->expr "Invalid lir `~a'!" lexpr))))

(define (lir->expr/g lexpr)
  `(,@(top-level->body-list (lambda (_ v) (lir->expr v)))
    ,(lir->expr lexpr)))
