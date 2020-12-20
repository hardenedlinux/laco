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
                                 bytevector-s16-set!
                                 bytevector-s32-set!
                                 bytevector-u16-set!
                                 bytevector-u32-set!
                                 bytevector-ieee-single-set!
                                 define-record-type))
  #:export (label-counter
            single-encode
            double-encode
            triple-encode
            quadruple-encode
            primitive-encode/basic
            primitive-encode/extend
            special-encode
            collection-obj-encode
            integer-obj-encode
            real-obj-encode
            rational-obj-encode
            complex-obj-encode
            string-obj-encode
            boolean-obj-encode
            symbol-encode
            prim-obj-encode
            proc-obj-encode))

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

(define* (double-encode type data #:optional (count? #t))
  (when (or (< type 0) (> type #b1111))
    (throw 'laco-error double-encode
           "Invalid type, should be #b10100000 ~ #b10101111!"
           (number->string (logior #b10100000 type) 2)))
  (when (or (< data 0) (> data #xff))
    (throw 'laco-error double-encode "Invalid data, should be 0 ~ 255" data))
  (let ((bv (make-bytevector 2 0)))
    (bytevector-u8-set! bv 0 (logior #b10100000 type))
    (bytevector-u8-set! bv 1 data)
    (when count? (label-counter 2))
    bv))

(define* (triple-encode type data1 data2 #:optional (count? #t))
  (when (or (< type 0) (> type #b1111))
    (throw 'laco-error double-encode
           "Invalid type, should be #b10110000 ~ #b10111111!"
           (number->string (logior #b1011 type) 2)))
  (when (or (< data1 0) (> data1 #xffff))
    (throw 'laco-error double-encode "Invalid data1, should be 0 ~ 65535" data1))
  (when (or (< data2 0) (> data2 #xffff))
    (throw 'laco-error double-encode "Invalid data1, should be 0 ~ 65535" data2))
  (let ((bv (make-bytevector 3 0)))
    (bytevector-u8-set! bv 0 (logior #b10110000 type))
    (bytevector-u8-set! bv 1 data1)
    (bytevector-u8-set! bv 2 data2)
    (when count? (label-counter 3))
    bv))

(define* (quadruple-encode type data1 data2 data3 #:optional (count? #t))
  (when (or (< type 0) (> type #b1000))
    (throw 'laco-error double-encode
           "Invalid type, should be #b10110000 ~ #b10111111!"
           (number->string (logior #b1011 type) 2)))
  (when (or (< data1 0) (> data1 #xffffff))
    (throw 'laco-error quadruple-encode
           "Invalid data, should be 0 ~ 16777215" data1))
  (when (or (< data2 0) (> data2 #xffffff))
    (throw 'laco-error quadruple-encode
           "Invalid data, should be 0 ~ 16777215" data2))
  (when (or (< data3 0) (> data3 #xffffff))
    (throw 'laco-error quadruple-encode
           "Invalid data, should be 0 ~ 16777215" data3))
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u8-set! bv 0 (logior #b10000000 type))
    (bytevector-u8-set! bv 1 data1)
    (bytevector-u8-set! bv 2 data2)
    (bytevector-u8-set! bv 3 data3)
    (when count? (label-counter 4))
    bv))

(define (primitive-encode/basic pn)
  (when (or (< pn 0) (> pn 15))
    (throw 'laco-error primitive-encode/basic "Invalid data, should be 0 ~ 15" pn))
  (let ((bv (make-bytevector 1 0)))
    (bytevector-u8-set! bv 0 (logior #b11000000 pn))
    (label-counter 1)
    bv))

(define (primitive-encode/extend pn)
  (when (or (< pn 16) (> pn 4095))
    (throw 'laco-error primitive-encode/basic
           "Invalid data, should be 0 ~ 4095" pn))
  (let ((fixed-pn (- pn 16))
        (bv (make-bytevector 2 0)))
    (bytevector-u8-set! bv 0 (logior #b11010000 (ash fixed-pn -8)))
    (bytevector-u8-set! bv 1 (logand #xff fixed-pn))
    (label-counter 2)
    bv))

(define (integer-obj-encode data)
  (when (or (< data (- (1- (expt 2 31)))) (> data (1- (expt 2 31))))
    (throw 'laco-error integer-obj-encode
           "Invalid integer object `~a', should be 32bit!" data))
  (let ((bv (make-bytevector 6 0)))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 0)
    (bytevector-s32-set! bv 2 data 'big)
    (label-counter 6)
    bv))

;; In LambdaChip, Real is IEEE754 in 32bit
(define *IEEE754-32bit-max* 3.4028235e38)
(define *IEEE754-32bit-min* (* 1.0 (expt 2 -149)))
(define (real-obj-encode data)
  (when (or (< data *IEEE754-32bit-min*) (> data *IEEE754-32bit-max*))
    (throw 'laco-error real-obj-encode
           "Invalid real object `~a', should be 32bit!" data))
  (let ((bv (make-bytevector 6 0)))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 13)
    (bytevector-ieee-single-set! bv 2 data 'big)
    (label-counter 6)
    bv))

(define (rational-obj-encode data)
  (let ((bv (make-bytevector 6 0))
        (type (if (positive? data) 14 15))
        (n (numerator data))
        (d (denominator data)))
    (when (or (< n (- (1- (expt 2 15)))) (> data (1- (expt 2 15))))
      (throw 'laco-error rational-obj-encode
             "Rational number has invalid numerator object `~a', should be 16bit!"
             (number->string n)))
    (when (or (< n (- (1- (expt 2 15)))) (> data (1- (expt 2 15))))
      (throw 'laco-error rational-obj-encode
             "Rational number has invalid denominator object `~a', should be 16bit!"
             (number->string d)))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 type)
    (bytevector-u16-set! bv 2 n 'big)
    (bytevector-u16-set! bv 4 d 'big)
    (label-counter 6)
    bv))

(define (complex-obj-encode data)
  (let ((r (real-part data))
        (i (imag-part data)))
    (when (or (< r (- (1- (expt 2 15)))) (> r (1- (expt 2 15))))
      (throw 'laco-error complex-obj-encode
             "Invalid complex object real part `~a', should be signed 16bit!" r))
    (when (or (< i (- (1- (expt 2 15)))) (> i (1- (expt 2 15))))
      (throw 'laco-error complex-obj-encode
             "Invalid complex object imagine part `~a', should be signed 16bit!" i))
    (cond
     ((and (exact? r) (exact? i))
      (let ((bv (make-bytevector 6 0)))
        (bytevector-u8-set! bv 0 #b11100010)
        (bytevector-u8-set! bv 1 16)
        (bytevector-s16-set! bv 2 r 'big)
        (bytevector-s16-set! bv 4 i 'big)
        (label-counter 6)
        bv))
     (else
      (let ((bv (make-bytevector 10 0)))
        (bytevector-u8-set! bv 0 #b11100010)
        (bytevector-u8-set! bv 1 17)
        (bytevector-ieee-single-set! bv 2 r 'big)
        (bytevector-ieee-single-set! bv 6 i 'big)
        (label-counter 10)
        bv)))))

(define (prim-obj-encode pn)
  ;; FIXME: Check pn
  (let ((bv (make-bytevector 6 0)))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 10)
    (bytevector-u32-set! bv 2 pn 'big)
    (label-counter 6)
    bv))

(define* (proc-obj-encode offset arity opt #:optional (count? #t))
  (when (or (< offset (- (1- (expt 2 16)))) (> offset (1- (expt 2 16))))
    (throw 'laco-error proc-obj-encode
           "Invalid proc object `0x~a', should be 16bit!"
           (number->string offset 16)))
  (when (or (>= arity (expt 2 16)) (< arity 0))
    (throw 'laco-error proc-obj-encode
           "Invalid proc arity `~a', should be 16bit!" arity))
  (when (or (>= opt (expt 2 16)) (< opt 0))
    (throw 'laco-error proc-obj-encode
           "Invalid proc arity `~a', should be 16bit!" opt))
  (let ((bv (make-bytevector 6 0)))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 9)
    (bytevector-u16-set! bv 2 offset 'big)
    (bytevector-u8-set! bv 4 arity)
    (bytevector-u8-set! bv 5 opt)
    (when count? (label-counter 6))
    bv))

;; NOTE: String is mapped to C string exactly.
(define (string-obj-encode str)
  (when (not (string? str))
    (throw 'laco-error string-obj-encode
           "Invalid object `~a', should be a string!" str))
  (let ((bv (make-bytevector 2 0))
        ;; NOTE: We don't support UTF-8 for performance
        (sbv (string->bytevector str "iso8859-1")))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 8)
    ;; encoding length = header + type + string + '\0'
    (label-counter (+ 2 (string-length str) 1))
    (list bv sbv #vu8(0))))

(define (collection-obj-encode type size)
  (when (or (< size 0) (>= size (expt 2 16)))
    (throw 'laco-error collection-obj-encode
           "Invalid collection size `~a', should be 0~2^16!" size))
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u8-set! bv 0 #b11100010)
    (bytevector-u8-set! bv 1 type)
    ;; encoding length = header + type + size_in_u16
    (bytevector-u16-set! bv 2 size 'big)
    (label-counter 4)
    bv))

(define (boolean-obj-encode value)
  (when (or (< value 0) (> value 15))
    (throw 'laco-error boolean-obj-encode "Invalid boolean value `~a'!" value))
  (let ((bv (make-bytevector 1 0)))
    (bytevector-u8-set! bv 0 (logior (ash #b1110 4) value))
    (label-counter 1)
    bv))

(define (symbol-encode offset)
  (when (or (< offset 0) (> offset (expt 2 16)))
    (throw 'laco-error symbol-encode
           "Invalid offset in symbol table `~a'!" offset))
  (let ((bv (make-bytevector 3 0)))
    (bytevector-u8-set! bv 0 #b11100110)
    (bytevector-u16-set! bv 1 offset 'big)
    (label-counter 3)
    bv))
