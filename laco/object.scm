;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2021
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

(define-module (laco object)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco primitives)
  #:use-module (ice-9 match)
  #:use-module (laco records)
  #:use-module ((rnrs) #:select (bytevector?))
  #:export (object?

            integer-object
            integer-object?
            make-integer-object

            list-object
            list-object?
            list-object-size
            list-object-value list-object-value-set!
            make-list-object

            vector-object
            vector-object?
            make-vector-object

            bytevector-object
            bytevector-object?
            make-bytevector-object

            char-object
            char-object?
            make-char-object

            pair-object
            pair-object?
            make-pair-object

            string-object
            string-object?
            make-string-object

            keyword-object
            keyword-object?
            make-keyword-object

            proc-object
            proc-object?
            make-proc-object

            prim-object
            prim-object?
            make-prim-object

            boolean-object
            boolean-object?
            make-boolean-object

            real-object
            real-object?
            make-real-object

            rational-object
            rational-object?
            make-rational-object

            complex-object
            complex-object?
            make-complex-object

            symbol-object
            symbol-object?
            make-symbol-object

            object->value
            create-constant-object
            create-collection-object))

;; NOTE:
;; 1. If the value can be unboxed, then we store them in unboxed style
;; 2. char and boolean are globally unique, so no unboxing available

(define (>=0 x) (>= x 0))

(define-record-type object)

(define-typed-record integer-object (parent object)
  (fields
   (value integer?)))

;; We use list to hold pair elements, and it'll be converted to pair in codegen
(define-typed-record pair-object (parent object)
  (fields
   (size (lambda (x) (= x 2)))
   (value list?)))

(define-typed-record list-object (parent object)
  (fields
   (size >=0)
   (value list?)))

(define-typed-record vector-object (parent object)
  (fields
   ;; we use list to hold the vector, it'll become real vector in codegen
   (size >=0)
   (value list?)))

(define-typed-record char-object (parent object)
  (fields
   (value char?)))

(define-typed-record string-object (parent object)
  (fields
   (value string?)))

(define-typed-record keyword-object (parent object)
  (fields
   (value keyword?)))

(define-typed-record proc-object (parent object)
  (fields
   (name string? not)
   (arity integer?)
   (entry string?)))

(define-typed-record prim-object (parent object)
  (fields
   (prim primitive?)))

(define-typed-record boolean-object (parent object)
  (fields
   (value boolean?)))

(define-typed-record symbol-object (parent object)
  (fields
   (value symbol?)))

(define-typed-record real-object (parent object)
  (fields
   (value real?)))

(define-typed-record complex-object (parent object)
  (fields
   (value complex?)))

(define-typed-record rational-object (parent object)
  (fields
   (value rational?)))

(define-typed-record keyword-object (parent object)
  (fields
   (value keyword?)))

(define-typed-record bytevector-object (parent object)
  (fields
   ;; size is not needed, since it bytevector-length can calculated
   ;; (size >=0)
   (value bytevector?)))

;; constant -> object
(define (create-constant-object c)
  (let ((val (constant-val c)))
    (match (constant-type c)
      ('integer (make-integer-object '() val))
      ('list (make-list-object '() val))
      ('char (make-char-object '() val))
      ('real (make-real-object '() val))
      ('complex (make-complex-object '() val))
      ('rational (make-rational-object '() val))
      ('string (make-string-object '() val))
      ('symbol
       (intern! val)
       (make-symbol-object '() val))
      ('keyword
       ;; NOTE: We don't make keyword unique, since the keyword is usually used in
       ;;       define*, and it'll be eliminated in compile time. We rarely use
       ;;       keyword in other cases.
       (make-keyword-object '() val))
      ('boolean (make-boolean-object '() val))
      ('bytevector (make-bytevector-object '() val))
      (else (throw 'laco-error create-constant-object "Invalid type `~a'!"
                   (constant-type c))))))

(define (object->value o)
  (cond
   ((integer-object? o) (integer-object-value o))
   ((list-object? o) (list-object-value o))
   ((pair-object? o) (pair-object-value o))
   ((char-object? o) (char-object-value o))
   ((real-object? o) (real-object-value o))
   ((complex-object? o) (complex-object-value o))
   ((rational-object? o) (rational-object-value o))
   ((string-object? o) (string-object-value o))
   ((symbol-object? o) (symbol-object-value o))
   ((keyword-object? o) (keyword-object-value o))
   ((boolean-object? o) (boolean-object-value o))
   ((prim-object? o) (prim-object-prim o))
   ((bytevector-object? o) (bytevector-object-value o))
   (else (throw 'laco-error object->value "Invalid type `~a'!" o))))

;; collection -> object
(define (create-collection-object type size val)
  (match type
    ('pair (make-pair-object '() size val))
    ('list (make-list-object '() size val))
    ('vector (make-vector-object '() size val))
    (else (throw 'laco-error create-collection-object "Invalid type `~a'!" type))))
;; TODO: finish others
