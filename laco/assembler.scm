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

(define-module (laco assembler)
  #:use-module (laco utils)
  #:use-module (laco assembler sasm)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (make-bytevector
                                 bytevector-u32-set!
                                 bytevector-length
                                 put-bytevector
                                 define-record-type))
  #:export (assembler))

(define sasm-module (resolve-module '(laco assembler sasm)))
(define (asm-insr? x) (module-defined? sasm-module x))
(define (insr->proc x) (module-ref sasm-module x))

(define (memory->bytecode me)
  (list (main-entry)))

(define (program->bytecode p)
  (define (->bytecode pe)
    (match pe
      (((exprs ...))
       (map program->bytecode exprs))
      ((('label name) label-body ...)
       `(,(label name) ,@(map program->bytecode label-body)))
      (((? asm-insr? insr) args ...)
       (apply (insr->proc insr) args))
      (else (map program->bytecode pe))))
  (flatten (->bytecode p)))

(define (clean->bytecode pc)
  '())

(define (size-of-section sec)
  (let ((bv (make-bytevector 4 0)))
    (bytevector-u32-set! bv 0
                         (fold (lambda (x p) (+ (bytevector-length x) p)) 0 sec)
                         'big)
    bv))

(define (gen-version-bv)
  ;; TODO: Add version management
  #u8(0 0 1))

(define *head* #u8(76 69 70)) ; LEF
(define (gen-lef port m p c)
  (put-bytevector port *head*)
  (put-bytevector port (gen-version-bv))
  (put-bytevector port (size-of-section m))
  (put-bytevector port (size-of-section p))
  (put-bytevector port (size-of-section c))
  (for-each (lambda (b) (put-bytevector port b)) m)
  (for-each (lambda (b) (put-bytevector port b)) p)
  (for-each (lambda (b) (put-bytevector port b)) c))

(define (assembler out sasm)
  (apply
   gen-lef
   out
   (match sasm
     (('lef ('memory m-expr ...) ('program p-expr ...) ('clean c-expr ...))
      ;; NOTE: The order matters
      (let* ((p (program->bytecode p-expr))
             (m (memory->bytecode m-expr))
             (c (clean->bytecode c-expr)))
        (list m p c)))
     (else (throw 'laco-error assembler "Invalid assembler code!" sasm)))))
