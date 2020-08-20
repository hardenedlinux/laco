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

(define-module (laco lpass closure-capture-fv)
  #:use-module (laco types)
  #:use-module (laco utils)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (ice-9 match))

;; 1. Tag the captured free-vars
;; 2. Convert the captured free-var to local-var

(define need-capture? (make-parameter #f))
(define current-frees (make-parameter #f))

(define (ccfv lexpr)
  (match lexpr
    (($ insr-closure _ _ _ frees body _)
     (parameterize ((need-capture? #t)
                    (current-frees frees))
       (insr-closure-body-set! lexpr (ccfv body)))
     lexpr)
    (($ insr-label _ _ _ exprs)
     (insr-label-body-set! lexpr (map ccfv exprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ exprs)
     (insr-proc-body-set! lexpr (map ccfv exprs))
     lexpr)
    (($ insr-free _ label mode offset keep?)
     (cond
      ((and (need-capture?) (hash-ref (current-frees) label))
       => (lambda (pattern)
            (match pattern
              ((? integer? local-offset)
               (let ((local (make-insr-local '() mode local-offset keep?)))
                 (hash-set! (current-frees) label (cons lexpr local))
                 local))
              (((? insr-free? free) . (? insr-local? local))
               local)
              (else (throw 'laco-error 'closure-capture-fv
                           "Invalid pattern `~a'!" pattern)))))
      (else lexpr)))
    (else lexpr)))

(define-pass closure-capture-free-vars lexpr (ccfv lexpr))
