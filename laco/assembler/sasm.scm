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
  #:use-module (laco assembler encode))

(define *label-table* (make-hash-table))
(define (label-register! name)
  (hash-set! *label-table* name (label-counter 0)))

(define-public (label name)
  (label-register! name)
  #u8())

;; ------- single encoding -----------------
(define-public (push-4bit-const i)
  (single-encode 0 i))

(define-public (ss-load-4bit-const i)
  (single-encode 0 1))

(define-public (global-ref i)
  (single-encode 2 i))

(define-public (global-set! i)
  (single-encode 3 i))

(define-public (call-closure offset)
  (single-encode 4 offset))

(define-public (jump-closure offset)
  (single-encode 5 offset))

(define-public (jump offset)
  (single-encode 6 offset))

(define-public (jump-tos-false offset)
  (single-encode 7 offset))

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
(define-public (call-proc arity offset)
  (double-encode 0 (logior (ash arity 8) offset)))

(define-public (push-16bit-const arity i)
  (double-encode 1 i))

(define-public (vec-ref offset i)
  (double-encode 2 (logior (ash offset 8) i)))

;; --------- quadruple encode -----------
(define-public (vec-set! offset i v)
  (double-encode 0 (logior (ash offset 16) (ash i 8) v)))

;; --------- special encode -----------
(define-public (primitive pn)
  (primitive-encode/basic pn))

(define-public (primitive/extend pn)
  (primitive-encode/extend pn))

(define-public (special-encode i)
  (primitive-encode/extend i))

(define-public (halt)
  #u8(255))

;; NOTE: return is useful for optimizing analysis, but it's useless for codegen,
;;       since the result to return is already in the TOS.
(define-public (return x)
  #u8())

;; ----------- object creation -----------
(define-public (push-integer-object i)
  (general-object-encode 0 i))
