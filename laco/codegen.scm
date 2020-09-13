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

(define (gen-closure-frame frees)
  ;; 1. If the value is integer, then it means the free-var was converted to local
  ;;    in fv-lifting, so we just ignore it.
  ;; 2. If it's lifted in fv-lifting, then it means it's closure-on-stack, so we
  ;;    don't actually capture it.
  (filter-map
   (lambda (x)
     (match x
       ((? integer? x) #f)
       ((($ insr-free _ label _ offset keep?) . (? insr-local?))
        (make-insr-free '() label 'push offset #t))
       (else (throw 'laco-error gen-closure-frame "Invalid item `~a'!" x))))
   (hash-map->list (lambda (_ x) x) frees)))

;; lir -> unspecified
(define (emit-sasm lir)
  (match lir
    (($ insr-closure _ label arity frees body mode)
     (define frame (gen-closure-frame frees))
     (define end-label (new-label "closure-end-"))
     (map emit-sasm frame)
     (emit-closure mode arity (length frame) label)
     (emit-jump end-label)
     (sasm-label-begin #f label)
     (emit-sasm body)
     (sasm-closure-end end-label)
     (sasm-label-end #f label))
    (($ insr-proc _ proc label _ _ lexprs)
     (sasm-label-begin proc label)
     (map emit-sasm lexprs)
     (sasm-label-end proc label))
    (($ insr-prelude _ proc mode arity)
     (emit-prelude proc mode arity))
    (($ insr-pcall _ p keep?)
     (when (not (zero? (primitive->number p)))
       (emit-prim-call p keep?)))
    (($ integer-object _ i)
     ;; TODO: how to associate the variable?
     (emit-integer-object i))
    (($ string-object _ s)
     (emit-string-object s))
    (($ boolean-object _ b)
     (emit-boolean b))
    (($ list-object _ size lst)
     (for-each emit-sasm (reverse lst))
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
    (($ insr-proc-call _ proc label keep?)
     (emit-proc-call proc label keep?))
    (($ insr-local _ mode offset keep?)
     (emit-local mode offset keep?))
    (($ insr-free _ label mode offset keep?)
     (emit-free label mode offset keep?))
    (else (throw 'laco-error emit-sasm "Invalid lir `~a'!" lir))))

(define (emit-sasm-memory lir)
  (sasm-main))

(define (emit-sasm-clean lir)
  (sasm-nop))

(define (gen-sasm lir)
  (sasm-memory-begin)
  (emit-sasm-memory lir)
  (sasm-memory-end)

  (sasm-program-begin)
  (top-level-for-each (lambda (_ v) (emit-sasm v)))
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
  (call-with-input-string (lir->sasm-string lir) read))

(define (codegen outfile sasm)
  (let ((out (open-file outfile "w")))
    (assembler out sasm)
    (close out)))
