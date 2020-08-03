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

(define-module (laco lir)
  #:use-module (laco utils)
  #:use-module (laco env)
  #:use-module (laco cps)
  #:use-module (laco types)
  #:use-module (laco object)
  #:use-module (laco primitives)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((rnrs) #:select (define-record-type))
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
            insr-prelude-arity

            insr-fjump insr-fjump?
            make-insr-fjump
            insr-fjump-label insr-fjump-label-set!

            insr-jump insr-jump?
            make-insr-jump
            insr-jump-proc insr-jump-proc-set!
            insr-jump-label insr-jump-label-set!

            insr-closure insr-closure?
            make-insr-closure
            insr-closure-env insr-closure-code

            insr-label insr-label?
            make-insr-label
            insr-label-label insr-label-label-set!
            insr-label-proc insr-label-proc-set!
            insr-label-body insr-label-body-set!

            insr-local insr-local?
            make-insr-local
            insr-local-label
            insr-local-offset

            insr-free insr-free?
            make-insr-free
            insr-free-label-set!

            insr-global insr-global?
            make-insr-global
            insr-global-name

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
;; 7. No recursive, the CPS tree is converted to flat instruction queue by invoking order.
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
   (offset integer?)))

;; Pop ss[offset] to TOS
(define-typed-record insr-free (parent insr)
  (fields
   ;; We convert the label to string since we will generate label pattern in codegen
   (label string?)
   (mode symbol?)
   (offset integer?)))

;; Global variables are stored in a special area
(define-typed-record insr-global (parent insr)
  (fields
   (mode symbol?)
   (offset integer?)))

;; jump if TOS is false
(define-typed-record insr-fjump (parent insr)
  (fields
   (label string?)))

;; jump without condition
(define-typed-record insr-jump (parent insr)
  (fields
   (proc string? not)
   (label string?)))

;; This is for primitive calling
(define-typed-record insr-pcall (parent insr)
  (fields
   (op primitive?)))

;; prelude will prepare the arity
(define-typed-record insr-prelude (parent insr)
  (fields
   (proc string?)
   (arity integer?)))

;; closure
(define-typed-record insr-closure (parent insr)
  (fields
   (env env?)
   (code insr?)))

(define (get-global-offset name)
  ;; TODO: compute the offset of the specified global var name
  0)

(define *labels* (make-hash-table))
(define (label-register! label lexpr)
  (hash-set! *labels* label lexpr))
(define (label-ref label)
  (hash-ref *labels* label))

;; NOTE:
;; We reuse prim:return for proc-return instruction, so the arity is 0.
(define *proc-return* (make-insr-pcall '() prim:return))

(define* (cps->lir expr #:optional (mode 'push))
  (match expr
    (($ lambda/k ($ cps _ kont name attr) args body)
     (let ((env (closure-ref (id-name name)))
           (label (id->string name)))
       (when (not env)
         (throw 'laco-error cps->lir
                "lambda/k: the closure label `~a' doesn't have an env!" label))
       (make-insr-proc '() (current-def) label env (length args)
                       (list (cps->lir body) *proc-return*))))
    #;
    (($ closure/k ($ cps _ kont name attr) env body) ;
    )
    (($ branch/k ($ cps _ kont name attr) cnd b1 b2)
     (let ((ce (cps->lir cnd))
           (bt (cps->lir b1))
           (bf (cps->lir b2))
           (label (id->string name)))
       (make-insr-label
        '()
        #f
        label
        (list
         (make-insr-push '() ce)
         (make-insr-fjump '() (cps-name bf))
         bt
         bf))))
    #;
    ;; TODO                               ;
    (($ collection/k ($ cps _ kont name attr) var type size value body) ; ;
    )
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
     (let ((f (cps->lir func 'call))
           (e (map cps->lir args))
           (env (closure-ref (id-name name)))
           (label (id->string name))
           (prelude (lambda (m) (make-insr-prelude '() (id->string func) m))))
       (when (not env)
         (throw 'laco-error cps->lir
                "app/k: the closure label `~a' doesn't have an env!" label))
       (if (insr-pcall? f)
           (make-insr-label '() #f label `(,@e ,f))
           (cond
            ((is-proper-tail-recursion? expr)
             (make-insr-label '() #f label `(,(prelude 1) ,@e ,f)))
            ((is-tail-call? expr)
             (make-insr-label '() #f label `(,(prelude 0) ,@e ,f)))
            (else
             (make-insr-label '() #f label `(,(prelude 2) ,@e ,f)))))))
    (($ constant/k _ value)
     (create-constant-object value))
    (($ lvar _ offset)
     (make-insr-local '() mode offset))
    (($ fvar _ label offset)
     (make-insr-free '() (id->string label) mode offset))
    (($ gvar ($ id _ name _))
     (let ((id-str (symbol->string name)))
       (match (top-level-ref name)
         (($ insr-proc _ proc label _ arity)
          (case mode
            ((push) (make-proc-object '() id-str arity label))
            ((call) (make-insr-jump '() proc label))
            (else
             (throw 'laco-error cps->lir "gvar: proc has invalid mode `~a'!"
                    mode))))
         (($ lambda/k ($ cps _ _ label attr) args _)
          (when (not (is-recursive? name))
            (throw 'laco-error cps->lir
                   "BUG: Invalid global or wrong recursive! `~a', `~a'"
                   name (cps->expr (top-level-ref name))))
          (case mode
            ((push) (make-proc-object '() id-str (length args) (id->string label)))
            ((call) (make-insr-jump '() id-str (id->string label)))
            (else
             (throw 'laco-error cps->lir "gvar: proc has invalid mode `~a'!"
                    mode))))
         ((? object? obj) obj)
         (#f (throw 'laco-error cps->lir "Missing global var `~a'!" name))
         (else (throw 'laco-error cps->lir "Invalid global var `~a'!" name)))))
    ((? primitive? p)
     (case mode
       ((push) (make-prim-object '() p))
       ((call) (make-insr-pcall '() p))))
    (else (throw 'laco-error cps->lir "Invalid cps `~a'!" (id-name expr)))))

(define (cps->lir/g expr)
  (parameterize ((current-kont 'global))
    (top-level-for-each
     (lambda (v e)
       (parameterize ((current-def (symbol->string v)))
         (top-level-set! v (cps->lir e))))))
  (make-insr-proc '() "____principio" "#principio"
                  (current-env) 0 (list (cps->lir expr))))

(define (lir->expr lexpr)
  (match lexpr
    (($ insr-proc _ proc label _ arity lexprs)
     `(proc ,proc ,label ,arity ,(map lir->expr lexprs)))
    (($ insr-label _ proc label exprs)
     `((label proc ,label)
       ,@(map lir->expr exprs)))
    (($ insr-pcall _ p)
     `(prim-call ,(primitive-name p) ,(primitive->number p)))
    (($ insr-prelude _ proc arity)
     `(prelude proc ,arity))
    (($ insr-jump _ proc label)
     `(jump ,proc ,label))
    (($ insr-local _ mode offset)
     `(local ,mode ,offset))
    (($ insr-free _ label mode offset)
     `(free-var ,label ,mode ,offset))
    (($ integer-object _ value) `(integer ,value))
    (($ string-object _ value) `(string ,value))
    (($ prim-object _ p) `(primitive ,(primitive-name p)))
    (else (throw 'laco-error lir->expr "Invalid lir `~a'!" lexpr))))

(define (lir->expr/g lexpr)
  `(,@(top-level->body-list (lambda (_ v) (lir->expr v)))
    ,(lir->expr lexpr)))
