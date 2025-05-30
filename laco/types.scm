;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2015,2019,2020-2021
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

(define-module (laco types)
  #:use-module (laco env)
  #:use-module (laco utils)
  #:use-module (laco records)
  #:use-module (srfi srfi-1)
  #:use-module ((rnrs) #:select (bytevector?))
  #:export (constant
            constant?
            make-constant
            constant-val
            constant-type
            detect-minimum-range
            detect-literal-type
            *laco/unspecified*
            gen-constant
            is-integer-node?
            is-boolean-node?
            is-char-node?
            is-unspecified-node?
            integer-check
            pred-constant
            is-immediate?

            id
            make-id
            id?
            id-name
            id-orig
            new-id
            id-eq?
            id-list?
            node-eq?
            id->string
            symbol->id

            lvar lvar?
            make-lvar
            new-lvar
            lvar-offset lvar-offset-set!

            fvar fvar?
            make-fvar
            new-fvar
            lvar-offset

            gvar gvar?
            make-gvar
            new-gvar
            gvar-offset

            local local?
            local-value
            new-local))

(define-record-type constant (fields val type))

(define (detect-literal-type x)
  (cond
   ((and (not (integer? x)) (rational? x))
    (if (inexact? x)
        'real
        (if (= 1 (denominator x))
            'real
            'rational)))
   (else
    (cond
     ((symbol? x) 'symbol)
     ((keyword? x) 'keyword)
     ((char? x) 'char)
     ((exact-integer? x) 'integer)
     ((real? x) 'real)
     ((complex? x) 'complex)
     ((string? x) 'string)
     ((boolean? x) 'boolean)
     ((pair? x) 'pair)
     ((list? x) 'list)
     ((vector? x) 'vector)
     ((char? x) 'char)
     ((bytevector? x) 'bytevector)
     ((unspecified? x) 'unspecified)
     (else (throw 'laco-error 'detect-literal-type "Invalid literal type!" x))))))

(define *laco/unspecified* (make-constant 'unspecified 'unspecified))

(define *laco/true* (make-constant #t 'boolean))
(define *laco/false* (make-constant #f 'boolean))

(define *global-constant-type*
  `((unspecified . ,(lambda (_) *laco/unspecified*))
    (boolean . ,(lambda (b) (if b *laco/true* *laco/false*)))))

(define (global-constant-type? t)
  (assoc-ref *global-constant-type* t))

(define (gen-constant x)
  (let ((type (detect-literal-type x)))
    (cond
     ((global-constant-type? type)
      => (lambda (generator) (generator x)))
     (else (make-constant x type)))))

(define (pred-constant x type)
  (and (constant? x) (eq? (constant-type x) type)))

(define (is-boolean-node? x) (pred-constant x 'boolean))
(define (is-char-node? x) (pred-constant x 'char))
(define (is-integer-node? x) (pred-constant x 'integer))
(define (is-unspecified-node? x) (pred-constant x 'unspecified))

(define *integer-range*
  '((u4 . (0 . 15))
    (s8 . (-128 . 127))
    (u8 . (0 . 255))
    (s16 . (-32768 . 32767))
    (u16 . (0 . 65535))
    (s32 . (-2147483648 . 2147483647))
    (u32 . (0 . 4294967295))))

(define (detect-minimum-range i)
  (or (any (lambda (t) (and (integer-check i t) t)) *integer-range*)
      (throw 'laco-error detect-minimum-range "Out of integer range `~a'" i)))

(define (integer-check x subtype)
  (if (integer? x)
      (let ((range (assoc-ref *integer-range* x)))
        (and (> x (car range)) (< x (cdr range))))
      (throw 'laco-error integer-check "`~a' is not an integer!" x)))

;; immediate refers data type which their raw data are read from LEF binary
;; for example literal string is an immediate
;; if there is a list contains a variable or closure, it is not an immediate
;; A closure contains data generated in runtime thus it is not an immediate
;; literal bytevector is an immediate
(define *immediates-pred*
  (list keyword? integer? string? char? boolean? pair? list? vector? symbol?
        real? number? complex? rational? bytevector?))

(define (is-immediate? x)
  (any (lambda (c) (c x)) *immediates-pred*))

(define-typed-record id
  (fields
   (name symbol?)
   ;; For example, the orig of x-123 is x
   (orig symbol?)))

;; This is only for AST, for macro expanding, we use newsym
;; strying -> <id>
(define* (new-id #:optional (orig "#x-") (rename? #t))
  (let* ((orig-fix (cond
                    ((string? orig) (string->symbol orig))
                    ((symbol? orig) orig)
                    (else (throw 'laco-error new-id "Inavlid orig `~a'" orig))))
         (name (if rename? (newsym orig-fix) orig-fix)))
    (make-id name orig-fix)))

(define (id->string id)
  (symbol->string (id-name id)))

(define (id-eq? x y)
  (cond
   ((or (not (id? x)) (not (id? y)))
    #f)
   (else
    (eq? (id-name x) (id-name y)))))

(define (id-list? lst)
  (make-object-list-pred lst id?))

(define (node-eq? x y)
  (or (id-eq? x y)
      (equal? x y)))

(define (symbol->id sym)
  (new-id (symbol->string sym) #f))

;; local variable
(define-typed-record lvar (parent id)
  (fields
   (offset positive? zero?)))
;; id -> integer -> lvar
(define (new-lvar id offset)
  (make-lvar (list (id-name id) (id-orig id)) offset))

;; free variable
(define-typed-record fvar (parent id)
  (fields
   (label string?)
   (offset positive? zero?)))
;; id -> string -> integer -> fvar
(define (new-fvar id label offset)
  (make-fvar (list (id-name id) (id-orig id)) label offset))

(define-typed-record gvar (parent id))
;; id -> gvar
(define (new-gvar id)
  (make-gvar (id-name id) (id-orig id)))

;; local slot for local binding
;; NOTE: In codegen, it's nothing but a pushing object dropped var name
(define-record-type local (parent id) (fields value))
(define (new-local var value)
  (make-local (id-name var) (id-orig var) value))
