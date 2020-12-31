;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2021
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

(define-module (laco lpass remove-unused-captures)
  #:use-module (laco types)
  #:use-module (laco object)
  #:use-module (laco utils)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (ice-9 match))

;; NOTE:
;; We will only capture the used free-vars, so the offset of capture-frame has
;; to be fixed.

(define *closures* (make-hash-table))

(define (ruc lexpr)
  (match lexpr
    (($ insr-closure _ label _ frees body _)
     (hash-set! *closures* label lexpr)
     (insr-closure-body-set! lexpr (map ruc body))
     lexpr)
    (($ insr-label _ _ _ exprs)
     (insr-label-body-set! lexpr (map ruc exprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ exprs)
     (insr-proc-body-set! lexpr (map ruc exprs))
     lexpr)
    (($ insr-free _ label name mode offset keep?)
     (let* ((captured-closures (is-captured-fv? (cons label name)))
            (upper-closure (hash-ref *closures* label))
            (frees (and upper-closure
                        (insr-closure-frees upper-closure))))
       ;; NOTE:
       ;; Count the fixed-offset to capture free-var, the closure to
       ;; which the free-var belongs should be considered to use to
       ;; fix the offset.
       (when frees
         (hash-set! frees name 'used))
       lexpr))
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map ruc value))
     lexpr)
    (else lexpr)))

(define-pass remove-unused-captures lexpr (ruc lexpr))
