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
  #:use-module (laco assembler sasm)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs)
  #:export (assembler))

;; sasm stands for S-expr ASM
(define (sasm->bytecode sasm)
  (eval sasm (resolve-module '(laco assembler sasm))))

(define (memory->bytecode me)
  '())

(define (program->bytecode pe)
  (map sasm->bytecode pe))

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

(define (assembler sasm output)
  (let ((out (open-file output "w")))
    (apply
     gen-lef
     out
     (match sasm
       (('lef ('memory m-expr ...) ('program p-expr ...) ('clean c-expr ...))
        (list (memory->bytecode m-expr) (program->bytecode p-expr) (clean->bytecode c-expr)))
       (('lef ('memory m-expr ...) ('program p-expr ...))
        (list (memory->bytecode m-expr) (program->bytecode p-expr) '()))
       (('lef ('program p-expr ...) ('clean c-expr ...))
        (list '() (program->bytecode p-expr) (clean->bytecode c-expr)))
       (('lef ('program p-expr ...))
        (list '() (program->bytecode p-expr) '()))
       (else (throw 'laco-error assembler "Invalid assembler code!" sasm))))))
