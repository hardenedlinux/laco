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

(define-module (laco sasm)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (sasm-emit
            get-all-sasm
            sasm-output))

(define *sasm-queue* (new-queue))
(define (get-all-sasm) (queue-slots *sasm-queue*))
(define (drop-hash label)
  (substring/shared label 1))

(define (sasm-output port)
  (define level 1)
  (define* (indent-spaces #:optional (mode 'stay))
    (define (gen-spaces)
      (fold (lambda (x p) (string-append " " p)) "" (iota level)))
    (match mode
      ('stay (gen-spaces))
      ('in (set! level (1+ level)))
      ('out (set! level (1- level)))))
  (format port "(lef~%")
  (indent-spaces 'in)
  (for-each
   (lambda (pattern)
     (match pattern
       (('label-begin label)
        (indent-spaces 'in)
        (format port "~a(label ~a) ; Label ~a begin~%" (indent-spaces)
                (drop-hash label) (drop-hash label))
        (indent-spaces 'in))
       (('label-end label)
        (indent-spaces 'out)
        (format port "~a;; Label ~a end~%" (indent-spaces) (drop-hash label))
        (indent-spaces 'out))
       ('prog-begin
        (format port "~a(program~%" (indent-spaces))
        (indent-spaces 'in))
       ('prog-end
        (indent-spaces 'out)
        (format port "~a(halt)~%" (indent-spaces))
        (format port "~a) ; Program end~%~%" (indent-spaces))
        (indent-spaces 'out))
       ('memory-begin
        (format port "~a(memory~%" (indent-spaces))
        (indent-spaces 'in))
       ('memory-end
        (format port "~a) ; Memory end~%~%" (indent-spaces))
        (indent-spaces 'out))
       ('clean-begin
        (format port "~a(clean~%" (indent-spaces))
        (indent-spaces 'in))
       ('clean-end
        (format port "~a) ; Clean end~%~%" (indent-spaces))
        (indent-spaces 'out))
       (('closure-prelude argc)
        (format port "~a) ; Closure ~a~%" (indent-spaces) argc)
        (indent-spaces 'out))
       ((('push-string-object s) . descp)
        (format port "~a(push-string-object ~s) ; ~a~%" (indent-spaces) s descp))
       ((('jump label) . descp)
        (format port "~a(jump ~a) ; ~a~%" (indent-spaces) (drop-hash label) descp))
       ((insr . descp)
        (format port "~a~a ; ~a~%" (indent-spaces) insr descp))
       (() #t)
       (else (throw 'laco-error 'sasm-output "Invalid pattern `~a'!" pattern))))
   (get-all-sasm))
  (format port ") ; End LEF~%"))

(define (sasm-emit expr) (queue-in! *sasm-queue* expr))

(define-public (sasm-nop)
  (sasm-emit '()))

(define-public (sasm-main)
  (sasm-emit '((main-entry) . "")))

(define-public (sasm-true)
  (sasm-emit
   '((push-boolean-true) . "Boolean true")))

(define-public (sasm-false)
  (sasm-emit
   '((push-boolean-false) . "Boolean false")))

(define-public (emit-constant type i)
  (if (integer-check i type)
      (sasm-emit `((push-4bit-const ,i) . ,(format #f "Constant 0x~X" i)))
      (throw 'laco-error emit-constant "Invalid integer value!" i)))

(define-public (emit-integer-object i)
  (sasm-emit `((push-integer-object ,i) . "")))

(define-public (emit-string-object s)
  (sasm-emit `((push-string-object ,s) . "")))

(define-public (emit-proc-object proc entry)
  (sasm-emit `((push-proc-object ,entry)
               . ,(format #f "Push Proc ~a in `~a'" proc entry))))

(define-public (emit-prim-object p)
  (sasm-emit `((push-prim-object ,(primitive->number p))
               . ,(format #f "Push primitive `~a'" (primitive-name p)))))

(define-public (emit-boolean b)
  (if b
      (sasm-true)
      (sasm-false)))

(define-public (emit-char c)
  (if (char? c)
      (sasm-emit
       `((push-char-const ,(char->integer c)) . ,(format #f "Char `~a'" c)))
      (throw 'laco-error emit-char "Invalid char value!" c)))

(define-public (emit-integer i)
  (emit-constant (detect-minimum-range i) i))

;; This is only for const unboxing
;; constant -> unspecified
(define-public (emit-const-imm x)
  (cond
   ((is-integer-node? x) (emit-integer (constant-val x)))
   ((is-boolean-node? x) (emit-boolean (constant-val x)))
   ((is-char-node? x) (emit-char (constant-val x)))
   (else (throw 'laco-error emit-const-imm "Invalid immediate `~a`!" x))))

(define-public (emit-prelude proc arity)
  (sasm-emit `((prelude ,arity) . ,(format #f "Prelude for ~a" proc))))

(define-public (emit-proc-return)
  (sasm-emit `((ret) . "")))

(define-public (emit-prim-call p)
  ;; NOTE: `return' is useful for optimizing analysis, but it's useless for codegen,
  ;;       since the result to return is already in the TOS.
  (sasm-emit `((prim-call ,(primitive->number p))
               . ,(format #f "Call primitive `~a'" (primitive-name p)))))

(define-public (emit-fjump label)
  (sasm-emit `((fjump ,label) . "")))

(define-public (emit-jump proc label)
  (let ((where (if proc (string-append "#" proc) label)))
    (sasm-emit
     `((jump ,where) . ,(format #f "Jump to ~a" (drop-hash where))))))

(define-public (emit-local mode offset)
  (case mode
    ((push) (sasm-emit `((local ,offset) . "")))
    ((call) (sasm-emit `((call-local ,offset) . "")))
    (else (throw 'laco-error emit-local "Invalid mode `~a'!" mode))))

(define-public (emit-free label mode offset)
  (case mode
    ((push) (sasm-emit `((free ,label ,offset) . "")))
    ((call) (sasm-emit `((call-free ,label ,offset) . "")))
    (else (throw 'laco-error emit-free "Invalid mode `~a'!" mode))))

(define-public (sasm-program-begin)
  (sasm-emit 'prog-begin))

(define-public (sasm-program-end)
  (sasm-emit 'prog-end))

(define-public (sasm-memory-begin)
  (sasm-emit 'memory-begin))

(define-public (sasm-memory-end)
  (sasm-emit 'memory-end))

(define-public (sasm-clean-begin)
  (sasm-emit 'clean-begin))

(define-public (sasm-clean-end)
  (sasm-emit 'clean-end))

(define-public (sasm-label-begin label)
  (sasm-emit `(label-begin ,label)))

(define-public (sasm-label-end label)
  (sasm-emit `(label-end ,label)))

(define-public (sasm-closure-prelude argc)
  (sasm-emit '((pop-16bit-const ,argc) . ,(format #f "Pop ~a args" argc))))
