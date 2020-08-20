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
       (('label-begin proc label)
        (indent-spaces 'in)
        (format port "~a(label ~a) ; ~a~%" (indent-spaces)
                (drop-hash label)
                (if proc
                    (format #f "Proc `~a' begin" (drop-hash proc))
                    (format #f "Label `~a' begin" (drop-hash label))))
        (indent-spaces 'in))
       (('label-end proc label)
        (indent-spaces 'out)
        (format port "~a;; ~a~%"
                (indent-spaces)
                (if proc
                    (format #f "Proc `~a' end" (drop-hash proc))
                    (format #f "Label `~a' end" (drop-hash label))))
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
       ((('closure-end label) . descp)
        (format port "~a(closure-end ~a)" (indent-spaces) label))
       ((('closure mode arity frame-size entry-label) . descp)
        (format port "~a(closure ~a ~a ~a ~a) ; ~a~%"
                (indent-spaces) mode arity frame-size (drop-hash entry-label)
                descp))
       ((('push-string-object s) . descp)
        (format port "~a(push-string-object ~s) ; ~a~%" (indent-spaces) s descp))
       ((('free label offset) . descp)
        (format port "~a(free ~a ~a) ; ~a~%" (indent-spaces) (drop-hash label)
                offset descp))
       ((('call-free label offset keep?) . descp)
        (format port "~a(call-free ~a ~a ~a) ; ~a~%"
                (indent-spaces) (drop-hash label) offset keep? descp))
       ((('call-proc label keep?) . descp)
        (format port "~a(call-proc ~a ~a) ; ~a~%"
                (indent-spaces) (drop-hash label) keep? descp))
       ((('fjump label) . descp)
        (format port "~a(fjump ~a) ; ~a~%" (indent-spaces) (drop-hash label) descp))
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

;; NOTE: we may use arity in the future
(define-public (emit-proc-object proc arity entry)
  (sasm-emit `((push-proc-object ,(drop-hash entry))
               . ,(format #f "Push Proc `~a' in `~a'"
                          (drop-hash proc) (drop-hash entry)))))

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

(define-public (emit-closure mode arity frame-size entry-label)
  (sasm-emit `((closure ,mode ,arity ,frame-size ,entry-label)
               . "")))

(define-public (emit-prelude proc mode arity)
  (sasm-emit `((prelude ,(mode->name mode) ,arity)
               . ,(format #f "Prelude for `~a'" (drop-hash proc)))))

(define-public (emit-proc-return)
  (sasm-emit `((ret) . "")))

(define-public (emit-prim-call p keep?)
  ;; NOTE: `return' is useful for optimizing analysis, but it's useless for codegen,
  ;;       since the result to return is already in the TOS.
  (sasm-emit `((prim-call ,(primitive->number p) ,keep?)
               . ,(format #f "Call primitive `~a'" (primitive-name p)))))

(define-public (emit-fjump label)
  (sasm-emit `((fjump ,label)
               . ,(format #f "Jump to ~a when TOS is false" (drop-hash label)))))

(define-public (emit-jump label)
  (sasm-emit `((jump ,label) . "")))

(define-public (emit-proc-call proc label keep?)
  (let ((where (if proc proc label)))
    (sasm-emit
     `((call-proc ,label ,keep?) . ,(format #f "Proc call ~a" (drop-hash where))))))

(define-public (emit-local mode offset keep?)
  (case mode
    ((push) (sasm-emit `((local ,offset) . "")))
    ((call) (sasm-emit `((call-local ,offset ,keep?) . "")))
    (else (throw 'laco-error emit-local "Invalid mode `~a'!" mode))))

(define-public (emit-free label mode offset keep?)
  (case mode
    ((push) (sasm-emit `((free ,label ,offset) . "")))
    ((call) (sasm-emit `((call-free ,label ,offset ,keep?) . "")))
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

(define-public (sasm-label-begin proc label)
  (label-in! label)
  (sasm-emit `(label-begin ,proc ,label)))

(define-public (sasm-label-end proc label)
  (label-out!)
  (sasm-emit `(label-end ,proc ,label)))

(define-public (sasm-closure-end end-label)
  (sasm-emit `((closure-end ,end-label) . "")))

(define-public (sasm-closure-capture frame-size end-label)
  (sasm-emit '((closure) . ,(format #f "Capture with ~a free-vars till ~a"
                                    frame-size (drop-hash end-label)))))
