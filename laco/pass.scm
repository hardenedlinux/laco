;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2023
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

(define-module (laco pass)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco cps)
  #:use-module (laco lir)
  #:use-module (laco object)
  #:use-module (laco primitives)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (define-pass
             run-pass))

(define *pass-table* (make-hash-table))
(define (get-pass name)
  (hash-ref *pass-table* name
            (lambda (_) (throw 'laco-error get-pass "Invalid pass `~a'!" name))))

(define-syntax-rule (define-pass name e body ...)
  (begin
    (define (name e) body ...)
    (hash-set! *pass-table* 'name name)))

(define-syntax-rule (run-pass expr lst ...)
  (fold (lambda (item last)
          (let ((e
                 (match item
                   ((name (? integer? cnt))
                    (=> fail!)
                    ;;(format #t "PASS 0: ~a~%" name)
                    (cond
                     ((get-pass name)
                      => (lambda (pass)
                           (fold (lambda (_ p) (pass p)) last (iota cnt))))
                     (else (fail!))))
                   (name
                    (=> fail!)
                    ;;(format #t "PASS 1: ~a~%" item)
                    (cond
                     ((get-pass name)
                      => (lambda (pass)
                           (pass last)))
                     (else (fail!))))
                   (else (throw 'laco-error 'run-pass "Invalid pass: `~a'!" 'item)))))
            ;; (pk "item"(object->string item))
            ;; (pretty-print
            ;;  (match e
            ;;    ((? cps?) (cps->expr e))
            ;;    ((? insr?) (lir->expr e))
            ;;    ((? primitive?) (primitive-name e))
            ;;    ((? object?) (object->value e))
            ;;    ((? id?) (id-name e))
            ;;    (else (error "BUG: invalid type" e))))
            e))
        expr (list 'lst ...)))
