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

(define-module (laco assembler sasm)
  #:use-module ((rnrs) #:select (make-bytevector
                                 bytevector-u8-set!
                                 bytevector-u32-set!))
  #:use-module (laco assembler encode)
  #:use-module (laco utils)
  #:export (main-entry
            call-proc
            fjump
            jump))

(define *label-table* (make-hash-table))
(define (label-register! name)
  (hash-set! *label-table* name (label-counter 0)))
(define-syntax-rule (label-ref name)
  (hash-ref *label-table* name))

(define (vm-stack-pop)
  (primitive-encode/basic 1))

(define (main-entry)
  (let ((bv (make-bytevector 4 0))
        (main (label-ref '____principio)))
    (when (not main)
      (throw 'laco-error main-entry "BUG: main entry is missing!"))
    (bytevector-u32-set! bv 0 main 'big)
    bv))

(define-public (label name)
  (label-register! name)
  #u8())

(define-public (closure-end label)
  (label-register! label)
  #u8())

;; ------- single encoding -----------------
(define-public (local i)
  (cond
   ((and (>= i 0) (< i 16))
    (single-encode 0 i))
   ((> i 15)
    (single-encode 1 i))
   (else (throw 'laco-error local "Invalid offset `~a'!" i))))

(define-public (call-local i keep?)
  (define (gen)
    (cond
     ((and (>= i 0) (< i 16))
      (single-encode #b0100 i))
     ((> i 15)
      (single-encode #b0101 i))
     (else (throw 'laco-error call-local "Invalid offset `~a'!" i))))
  (cond
   (keep? (gen))
   (else
    (list (gen)
          (vm-stack-pop)))))

;; --------- special double encoding ----------
(define-public (free label i)
  (let ((frame (make-bytevector 1 0))
        (f (label-back-index label)))
    (when (and (< i 0) (>= i 64))
      (throw 'laco-error free "Invalid free offset `~a'" i))
    (when (and (< f 0) (>= i 64))
      (throw 'laco-error free "Invalid free frame back index `~a'" f))
    (bytevector-u8-set! frame 0 (logior (ash (logand i #b11) 6) f))
    (label-counter 1)
    (list
     (single-encode #b0010 (ash (logand i #b111100) -2))
     frame)))

(define-public (call-free label i keep?)
  (define (gen)
    (let ((frame (make-bytevector 1 0))
          (f (label-back-index label)))
      (when (and (< i 0) (>= i 64))
        (throw 'laco-error free "Invalid free offset `~a'" i))
      (when (and (< f 0) (>= i 64))
        (throw 'laco-error free "Invalid free frame back index `~a'" f))
      (bytevector-u8-set! frame 0 (logior (ash (logand i #b11) 6) f))
      (label-counter 1)
      (list
       (single-encode #b0011 (ash (logand i #b111100) -2))
       frame)))
  (cond
   (keep? (gen))
   (else
    (list (gen)
          (vm-stack-pop)))))

;; --------- double encoding -----------
(define-public (prelude mode-name arity)
  (let ((b (logior (ash arity 2) (name->mode mode-name))))
    (double-encode #b0000 b)))

;; --------- triple encoding -----------
(define-public (vec-ref offset i)
  (throw 'laco-error vec-ref "Haven't implemented yet!"))

(define* (call-proc label keep? #:optional (count? #t))
  (define (gen)
    (let ((offset (label-ref label)))
      (cond
       (offset
        (triple-encode #b0000
                       (ash (logand offset #xff00) -8)
                       (logand offset #xff)))
       (else
        (label-counter 3)
        `#((call-proc ,label ,keep? #f))))))
  (cond
   (keep? (gen))
   (else
    (list (gen)
          (vm-stack-pop)))))

(define* (fjump label #:optional (count? #t))
  (let ((offset (label-ref label)))
    (cond
     (offset
      (triple-encode #b0001
                     (ash (logand offset #xff00) -8)
                     (logand offset #xff)
                     count?))
     (else
      (label-counter 3)
      `#((fjump ,label #f))))))

(define* (jump label #:optional (count? #t))
  (let ((offset (label-ref label)))
    (cond
     (offset
      (triple-encode #b0010
                     (ash (logand offset #xff00) -8)
                     (logand offset #xff)
                     count?))
     (else
      (label-counter 3)
      `#((jump ,label #f))))))

;; --------- quadruple encode -----------
(define-public (vec-set! offset i v)
  (throw 'laco-error vec-set! "Haven't implemented yet!"))

(define* (closure-on-heap arity frame-size entry #:optional (count #t))
  (quadruple-encode #b0001
                    (logior (ash arity 4) frame-size)
                    (ash (logand entry #xff00) -8)
                    (logand entry #xff)
                    count))

(define* (closure-on-stack arity frame-size entry #:optional (count #t))
  (quadruple-encode #b0010
                    (logior (ash arity 4) frame-size)
                    (ash (logand entry #xff00) -8)
                    (logand entry #xff)
                    count))

(define* (closure mode arity frame-size entry-label #:optional (count #t))
  (let ((entry (label-ref entry-label)))
    (cond
     (entry
      (case mode
        ((stack) (closure-on-stack arity frame-size entry count))
        ((heap) (closure-on-heap arity frame-size entry count))
        (else (throw 'laco-error closure "Invalid mode `~a'!" mode))))
     (else
      (label-counter 4)
      `#((closure ,mode ,arity ,frame-size ,entry-label #f))))))

;; --------- special encode -----------
;; TODO: detect if it's primitive/extend
(define-public (prim-call pn keep?)
  (cond
   (keep?
    (primitive-encode/basic pn))
   (else
    (list (primitive-encode/basic pn)
          (vm-stack-pop)))))

(define-public (primitive/extend pn)
  (primitive-encode/extend pn))

(define-public (special-encode i)
  (primitive-encode/extend i))

(define-public (halt)
  #u8(255))

;; ----------- object creation -----------
(define-public (push-integer-object i)
  (integer-obj-encode i))

(define-public (push-string-object s)
  (string-obj-encode s))

(define-public (push-list-object size)
  (collection-obj-encode 7 size))

(define-public (push-vector-object size)
  (collection-obj-encode 5 size))

;; TODO:
;; 1. add opt-index
;; 2. detect opt-index and convert vargs to list in vm.c
;; 3. patch local-call and free-call
(define* (push-proc-object entry arity opt-index #:optional (count #t))
  (let ((offset (label-ref entry))
        ;; If there's no opt-index, then we set it to the last index of args
        (opt (or opt-index arity)))
    (cond
     (offset (proc-obj-encode offset arity opt count))
     (else
      (label-counter 6)
      `#((push-proc-object ,entry #f))))))

(define-public (push-prim-object pn)
  (prim-obj-encode pn))

(define-public (push-boolean-false)
  (boolean-obj-encode 0))

(define-public (push-boolean-true)
  (boolean-obj-encode 1))
