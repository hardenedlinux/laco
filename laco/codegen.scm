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

(define-module (laco codegen)
  #:use-module (laco primitives)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco env)
  #:use-module (laco lir)
  #:use-module (laco sasm)
  #:use-module (laco object)
  #:use-module (laco assembler)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (lir->sasm-string
            lir->sasm
            codegen))

(define (gen-binding-frame env)
  (let lp ((next (env-bindings env)) (ret '()))
    (match next
      (() (reverse ret))
      (((? id? id) rest ...)
       (lp (cdr next) (cons (bindings-index env id) ret)))
      (else (lp cdr next) ret))))

(define (gen-closure-frame closure-label)
  ;; 1. If the value is integer, then it means the free-var was converted to local
  ;;    in fv-lifting, so we just ignore it.
  ;; 2. If it's lifted in fv-lifting, then it means it's closure-on-stack, so we
  ;;    don't actually capture it.
  (map
   (lambda (x)
     (match x
       ((_ ($ insr-free _ label name _ offset keep?) fixed-offset)
        ;; NOTE: Since the offset will add the size of the captures, so we have to
        ;;       use the fixed-offset here.
        ;;(pk "capture free" label name fixed-offset)
        (make-insr-free '() label name 'push fixed-offset #t))
       (else (throw 'laco-error gen-closure-frame "Invalid item `~a'!" x))))
   (get-ordered-frees closure-label)))

;; lir -> unspecified
(define (emit-sasm lir)
  (match lir
    (($ insr-closure _ label arity frees body mode)
     ;; NOTE: We don't use frees in insr-closure, we have to use ordered frees
     (define frame (gen-closure-frame label))
     (define end-label (new-label "closure-end-"))
     (for-each emit-sasm frame)
     (emit-closure mode arity (length frame) label)
     (emit-jump end-label)
     (sasm-label-begin #f label)
     (for-each emit-sasm body)
     (sasm-closure-end end-label)
     (sasm-label-end #f label))
    (($ insr-proc _ proc label _ _ lexprs)
     (sasm-label-begin proc label)
     (map emit-sasm lexprs)
     (sasm-label-end proc label))
    (($ insr-prelude _ proc label mode arity)
     (emit-prelude proc label mode arity))
    (($ insr-pcall _ p keep?)
     (when (not (zero? (primitive->number p)))
       (emit-prim-call p keep?)))
    (($ integer-object _ i)
     ;; TODO: how to associate the variable?
     (emit-integer-object i))
    (($ real-object _ r)
     (emit-real-object r))
    (($ rational-object _ r)
     (emit-rational-object r))
    (($ complex-object _ c)
     (emit-complex-object c))
    (($ string-object _ s)
     (emit-string-object s))
    (($ keyword-object _ s)
     (emit-keyword-object s))
    (($ symbol-object _ s)
     (emit-symbol-object s))
    (($ boolean-object _ b)
     (emit-boolean b))
    (($ pair-object _ _ vals)
     (for-each emit-sasm vals)
     (emit-pair-object))
    (($ list-object _ size lst)
     (for-each emit-sasm lst)
     (emit-list-object size))
    (($ vector-object _ size lst)
     (for-each emit-sasm (reverse lst))
     (emit-vector-object size))
    (($ proc-object _ proc arity entry)
     (emit-proc-object proc arity (lambda-has-vargs? (string->symbol proc)) entry))
    (($ prim-object _ p)
     (emit-prim-object p))
    (($ insr-label _ proc label insrs)
     (sasm-label-begin proc label)
     (for-each emit-sasm insrs)
     (sasm-label-end proc label))
    (($ insr-fjump _ label)
     (emit-fjump label))
    (($ insr-branch-end _ label)
     (sasm-branch-end label))
    (($ insr-jump _ label)
     (emit-jump label))
    (($ insr-proc-call _ proc label keep?)
     (emit-proc-call proc label keep?))
    (($ insr-assign _ var value)
     (emit-sasm value)
     (match var
       (($ insr-local _ _ name offset _)
        (emit-local-assign name offset))
       (($ insr-free _ label name _ offset _)
        (emit-free-assign label name offset))
       (($ insr-global _ name)
        (emit-global-assign name))
       (else (throw 'laco-error emit-sasm
                    "BUG: invalid var `~a' in assign!" var))))
    (($ insr-local _ name mode offset keep?)
     (emit-local (symbol->string name) mode offset keep?))
    (($ insr-free _ label _ mode offset keep?)
     (emit-free label mode offset keep?))
    (($ insr-global _ name _)
     (emit-global name))
    (($ insr-global-call _ name _)
     (emit-global-call name))
    (else (throw 'laco-error emit-sasm "Invalid lir `~a'!" lir))))

(define (emit-sasm-memory lir)
  (emit-intern-symbol-table)
  (sasm-main))

(define (emit-sasm-clean lir)
  (sasm-nop))

(define (gen-sasm lir)
  (sasm-memory-begin)
  (emit-sasm-memory lir)
  (sasm-memory-end)

  (sasm-global-begin)
  (top-level-for-each
   (lambda (k v)
     (match v
       ((? object?)
        (global-label-register! k "object")
        (emit-sasm v))
       (($ insr-proc _ proc label _ arity _)
        (global-label-register! k label)
        (emit-sasm (make-proc-object '() (symbol->string k) arity label)))
       (else (throw 'laco-error gen-sasm "BUG: Invalid global `~a'" v)))))
  (sasm-global-end)

  (sasm-program-begin)
  (top-level-for-each
   (lambda (_ v)
     (when (or (insr-proc? v) (insr-label? v))
       (emit-sasm v))))
  (emit-sasm lir)
  (sasm-program-end)

  (sasm-clean-begin)
  (emit-sasm-clean lir)
  (sasm-clean-end))

;; debug helper function
(define (lir->sasm-string lir)
  (gen-sasm lir)
  (call-with-output-string sasm-output))

(define (lir->sasm lir)
  ;; TODO:
  ;; 1. generate globals
  ;; 2. compute globals' offset
  ;; 3. create global table in LEF
  (call-with-input-string (lir->sasm-string lir) read))

(define (codegen outfile sasm)
  (let ((out (open-file outfile "w")))
    (assembler out sasm)
    (close out)))
