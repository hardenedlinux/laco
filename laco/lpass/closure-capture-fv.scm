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
  #:use-module (laco object)
  #:use-module (laco utils)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (ice-9 match))

;; 1. Tag the captured free-vars
;; 2. Convert the captured free-var to local-var

(define need-capture? (make-parameter #f))
(define current-frees (make-parameter #f))

;; NOTE;
;; The offset of the closure captured variables start from (local 0),
;; so we need to increase offset of locals starts from the size of closure frame.
;; NOTE:
;; There's no free vars before fix-offset, since they're all converted to locals.
(define (fix-offset lexpr)
  (match lexpr
    (($ insr-closure _ _ _ frees body _)
     (parameterize ((need-capture? #t)
                    (current-frees frees))
       (insr-closure-body-set! lexpr (map fix-offset body)))
     lexpr)
    (($ insr-label _ _ _ exprs)
     (insr-label-body-set! lexpr (map fix-offset exprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ exprs)
     (insr-proc-body-set! lexpr (map fix-offset exprs))
     lexpr)
    (($ insr-local _ name _ offset _)
     (cond
      ((and (need-capture?) (not (hash-ref (current-frees) name)))
       (let ((fixed-offset (+ offset (hash-count (const #t) (current-frees)))))
         ;;(pk "tag" name offset fixed-offset) (read)
         (insr-local-offset-set! lexpr fixed-offset)
         lexpr))
      (else lexpr)))
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map fix-offset value))
     lexpr)
    (else lexpr)))

(define (ccfv lexpr)
  (match lexpr
    (($ insr-closure _ _ _ frees body _)
     (parameterize ((need-capture? #t)
                    (current-frees frees))
       (insr-closure-body-set! lexpr (map ccfv body)))
     lexpr)
    (($ insr-label _ _ _ exprs)
     (insr-label-body-set! lexpr (map ccfv exprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ exprs)
     (insr-proc-body-set! lexpr (map ccfv exprs))
     lexpr)
    (($ insr-free _ label name mode offset keep?)
     (cond
      ((and (need-capture?) (hash-ref (current-frees) name))
       => (lambda (pattern)
            (match pattern
              ((? integer? local-offset)
               (let ((local (make-insr-local '() name mode local-offset keep?)))
                 ;; (pk "tag" label name offset local-offset) (read)
                 (hash-set! (current-frees) name (cons lexpr local))
                 local))
              (((? insr-free? free) . (? insr-local? local))
               local)
              (else (throw 'laco-error 'closure-capture-fv
                           "Invalid pattern `~a' in ~a!" pattern name)))))
      (else lexpr)))
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map ccfv value))
     lexpr)
    (else lexpr)))

(define-pass closure-capture-free-vars lexpr (fix-offset (ccfv lexpr)))
