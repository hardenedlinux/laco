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

(define-module (laco assembler encode)
  #:use-module (laco utils)
  #:use-module (ice-9 iconv)
  #:use-module ((rnrs) #:select (make-bytevector
                                 bytevector-u8-set!
                                 bytevector-s32-set!
                                 define-record-type))
  #:export (label-counter
            single-encode
            double-encode
            triple-encode
            quadruple-encode
            primitive-encode/basic
            primitive-encode/extend
            special-encode
            general-integer-encode
            general-string-encode
            boolean-encode))

(define label-counter (new-counter))

(define (single-encode type data)
  (when (or (< type 0) (> type #b0111))
    (throw 'laco-error single-encode
           "Invalid type, should be #b0100 ~ #b1001!" type))
  (when (or (< data 0) (> data 15))
    (throw 'laco-error single-encode "Invalid data, should be 0 ~ 15" data))
  (let ((bv (make-bytevector 1 0)))
    (bytevector-u8-set! bv 0 (logior (ash type 4) data))
    (label-counter 1)
    bv))

(define (double-encode type data)
  (when (or (< type 0) (> type #b1111))
    (throw 'laco-error double-encode
           "Invalid type, should be #b10100000 ~ #b10101111!"
           (number->string (logior #b10100000 type) 2)))
  (when (or (< data 0) (> data #xff))
    (throw 'laco-error double-encode "Invalid data, should be 0 ~ 255" data))
  (let ((bv (make-bytevector 2 0)))
    (bytevector-u8-set! bv 0 (logior #b1010 type))
    (bytevector-u8-set! bv 1 data)
    (label-counter 2)
    bv))

(define (triple-encode type data)
  (when (or (< type 0) (> type #b1111))
    (throw 'laco-error double-encode
           "Invalid type, should be #b10110000 ~ #b10111111!"
           (number->string (logior #b1011 type) 2)))
  (when (or (< data 0) (> data #xffff))
    (throw 'laco-error double-encode "Invalid data, should be 0 ~ 65535" data))
  (let ((bv (make-bytevector 3 0)))
    (bytevector-u8-set! bv 0 (logior #b10100000 type))
    (bytevector-u8-set! bv 1 (ash (logand #xff00 data) -8))
    (bytevector-u8-set! bv 2 (logand #xff data))
    (label-counter 3)
    bv))

(define (quadruple-encode type data)
  (when (or (< type 0) (> type #b1000))
    (throw 'laco-error double-encode
           "Invalid type, should be #b10110000 ~ #b10111111!"
           (number->string (logior #b1011 type) 2)))
  (when (or (< data 0) (> data #xffffff))
    (throw 'laco-error double-encode "Invalid data, should be 0 ~ 16777215" data))
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u8-set! bv 0 (logior #b10100000 type))
    (bytevector-u8-set! bv 1 (ash (logand #xff0000 data) -16))
    (bytevector-u8-set! bv 2 (ash (logand #xff00 data) -8))
    (bytevector-u8-set! bv 3 (logand #xff data))
    (label-counter 4)
    bv))

(define (primitive-encode/basic pn)
  (when (or (< pn 0) (> pn 15))
    (throw 'laco-error primitive-encode/basic "Invalid data, should be 0 ~ 15" pn))
  (let ((bv (make-bytevector 1 0)))
    (bytevector-u8-set! bv 0 (logior #b11000000 pn))
    (label-counter 1)
    bv))

(define (primitive-encode/extend pn)
  (when (or (< pn 0) (> pn 4095))
    (throw 'laco-error primitive-encode/basic
           "Invalid data, should be 0 ~ 4095" pn))
  (let ((bv (make-bytevector 2 0)))
    (bytevector-u8-set! bv 0 (logior #b11010000 (ash pn -8)))
    (bytevector-u8-set! bv 1 (logand #xff pn))
    (label-counter 2)
    bv))

(define (general-integer-encode data)
  (when (or (< data (- (1- (expt 2 31)))) (> data (1- (expt 2 31))))
    (throw 'laco-error general-integer-encode
           "Invalid integer object `0x~a', should be 32bit!"
           (number->string data 16)))
  (let ((bv (make-bytevector 6 0))
        (d (if (>= data 0) data (- data))))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 0)
    (bytevector-s32-set! bv 2 data 'big)
    (label-counter 6)
    bv))

;; NOTE: String is mapped to C string exactly.
(define (general-string-encode str)
  (when (not (string? str))
    (throw 'laco-error general-string-encode
           "Invalid object `~a', should be a string!" str))
  (let ((bv (make-bytevector 2 0))
        ;; NOTE: We don't support UTF-8 for performance
        (sbv (string->bytevector str "iso8859-1")))
    (bytevector-u8-set! bv 0 #b11100110)
    ;; encoding length = header + string + '\0'
    (label-counter (+ 1 (string-length str) 1))
    (list bv sbv)))

(define (boolean-encode value)
  (when (or (< value 1) (> value 15))
    (throw 'laco-error boolean-encode "Invalid boolean value `~a'!" value))
  (let ((bv (make-bytevector 1 0)))
    (bytevector-u8-set! bv 0 (logior (ash #b1110 4) value))
    (label-counter 1)
    bv))
