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

(define-module (laco lpass reduce-ret)
  #:use-module (laco types)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; Reduce redundant ret instructions

(define (ret-eq? x y)
  (and (insr-pcall? x)
       (insr-pcall? y)
       (eq? (insr-pcall-op x) (insr-pcall-op y))
       (eq? (insr-pcall-op x) prim:return)))

(define (is-ret? x)
  (ret-eq? x *proc-return*))

(define (rr lexpr)
  (match lexpr
    (($ insr-proc _ _ "#principio" _ _ lexprs)
     (insr-proc-body-set!
      lexpr
      (let ((len (1- (length lexprs))))
        (match (reverse lexprs)
          (((? is-ret?) rest ...) (reverse! rest))
          (else lexprs))))
     lexpr)
    (($ insr-proc _ _ label _ _ lexprs)
     (insr-proc-body-set!
      lexpr
      (reverse! (fold (lambda (e p)
                        (if (or (null? p) (not (ret-eq? e (car p))))
                            (cons e p)
                            p))
                      '() lexprs)))
     lexpr)
    (($ insr-label _ _ label lexprs)
     (insr-label-body-set! lexpr (map rr lexprs))
     lexpr)
    (($ insr-proc _ _ label _ _ body)
     (insr-proc-body-set! lexpr (rr body))
     lexpr)
    (else lexpr)))

(define-pass reduce-ret lexpr (rr lexpr))
