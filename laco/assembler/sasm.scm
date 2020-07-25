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
  #:export (main-entry))

(define *label-table* (make-hash-table))
(define (label-register! name)
  (hash-set! *label-table* name (label-counter 0)))
(define-syntax-rule (label-ref name)
  (or (hash-ref *label-table* name)
      (throw 'laco-error 'label-ref "Missing label `~a'!" name)))

(define (main-entry)
  (let ((bv (make-bytevector 4 0))
        (main (label-ref 'main)))
    (when (not main)
      (throw 'laco-error main-entry "BUG: main entry is missing!"))
    (bytevector-u32-set! bv 0 main 'big)
    bv))

(define-public (label name)
  (label-register! name)
  #u8())

;; ------- single encoding -----------------
(define-public (local i)
  (cond
   ((and (>= i 0) (< i 16))
    (single-encode 0 i))
   ((> i 15)
    (single-encode 1 i))
   (else (throw 'laco-error local "Invalid offset `~a'!" i))))

(define-public (call-local i)
  (cond
   ((and (>= i 0) (< i 16))
    (single-encode #b0100 i))
   ((> i 15)
    (single-encode #b0101 i))
   (else (throw 'laco-error call-local "Invalid offset `~a'!" i))))

;; --------- special double encoding ----------
(define-public (free label mode i)
  (let ((frame (make-bytevector 1 0))
        (f (label-ref label)))
    (bytevector-u8-set! frame 0 f)
    (label-counter 1)
    (cond
     ((and (> i 0) (< 16))
      (list
       (single-encode #b0010 i)
       frame))
     ((> i 15)
      (list
       (single-encode #b0011 i)
       frame))
     (else (throw 'laco-error free "Invalid offset `~a'!" i)))))

(define-public (call-free label mode i)
  (let ((frame (make-bytevector 1 0))
        (f (label-ref label)))
    (bytevector-u8-set! frame 0 f)
    (label-counter 1)
    (cond
     ((and (> i 0) (< 16))
      (list
       (single-encode #b0110 i)
       frame))
     ((> i 15)
      (list
       (single-encode #b0111 i)
       frame))
     (else (throw 'laco-error call-free "Invalid offset `~a'!" i)))))

;; --------- double encoding -----------

(define-public (push-8bit-const i)
  (double-encode 0 i))

(define-public (long-jump offset)
  (double-encode 1 offset))

(define-public (long-jump-tos-false offset)
  (double-encode 2 offset))

(define-public (make-closure entry-offset)
  (double-encode 3 entry-offset))

(define-public (ss-pop-8bit-const offset)
  (double-encode 4 offset))

;; --------- triple encoding -----------
(define-public (call-proc label arity)
  (let ((offset (label-ref label)))
    (triple-encode 0 arity offset)))

(define-public (push-16bit-const arity i)
  (double-encode 1 i))

(define-public (vec-ref offset i)
  (double-encode 2 (logior (ash offset 8) i)))

;; --------- quadruple encode -----------
(define-public (vec-set! offset i v)
  (double-encode 0 (logior (ash offset 16) (ash i 8) v)))

;; --------- special encode -----------
;; TODO: detect if it's primitive/extend
(define-public (prim-call pn)
  (primitive-encode/basic pn))

(define-public (primitive/extend pn)
  (primitive-encode/extend pn))

(define-public (special-encode i)
  (primitive-encode/extend i))

(define-public (halt)
  #u8(255))

;; ----------- object creation -----------
(define-public (push-integer-object i)
  (integer-encode i))

(define-public (push-string-object s)
  (string-encode s))

(define-public (push-proc-object arity entry)
  (let ((offset (label-ref entry)))
    (proc-encode arity offset)))

(define-public (push-prim-object pn)
  (prim-encode pn))

(define-public (push-boolean-false)
  (boolean-encode 0))

(define-public (push-boolean-true)
  (boolean-encode 1))
