;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020,2021
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

;; Theory
;; 1. Every captured (free n m) will be converted to (local o), `o' is the index
;;    of free-var in the ordered capture queue.
;; 2. Every captured (free n m) will be recorded in the capture queue. And they'll
;;    be put as pushed constant before closure-capture instructions. So that we can
;;    capture free-vars in the runtime.
;; 3. Every (local m) will be fixed to (local (+ m capture-frame-size))
;; 4. In runtime, there's no free-var in the closure body, when (free n m) appears,
;;    it's used to capture free-vars and store to the environment of the closure.
;;    The offset of local-vars are fixed to fetch from either env or the stack.

(define need-capture? (make-parameter #f))
(define current-frees (make-parameter #f))
(define current-closure (make-parameter #f))

(define *closures* (make-hash-table))

;; NOTE;
;; The offset of the closure captured variables start from (local 0),
;; so we need to increase offset of locals starts from the size of closure frame.
;; NOTE:
;; There's no free vars before fix-offset, since they're all converted to locals.
;; NOTE:
;; The local-var converted from free-var will not be fixed.
(define (fix-local-offset lexpr)
  (match lexpr
    (($ insr-closure _ name _ frees body _)
     (parameterize ((current-closure name)
                    (need-capture? #t)
                    (current-frees frees))
       (insr-closure-body-set! lexpr (map fix-local-offset body)))
     lexpr)
    (($ insr-label _ _ _ exprs)
     (insr-label-body-set! lexpr (map fix-local-offset exprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ exprs)
     (insr-proc-body-set! lexpr (map fix-local-offset exprs))
     lexpr)
    (($ insr-local _ name _ offset _)
     (cond
      ((and (need-capture?) (not (hash-ref (current-frees) name)))
       (let ((fixed-offset (if (list? offset)
                               (car offset)
                               (+ offset (hash-count (const #t) (current-frees))))))
         ;;(pk "frees" (hash-map->list cons (current-frees)))
         ;;(pk "fixed tag" (current-closure) name offset fixed-offset) (read)
         (insr-local-offset-set! lexpr fixed-offset)
         lexpr))
      (else
       (when (list? offset)
         (insr-local-offset-set! lexpr (car offset)))
       lexpr)))
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map fix-local-offset value))
     lexpr)
    (else lexpr)))

(define (ccfv lexpr)
  (match lexpr
    (($ insr-closure _ label _ frees body _)
     (parameterize ((current-closure label)
                    (need-capture? #t)
                    (current-frees frees))
       (hash-set! *closures* label lexpr)
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
               (let* ((captured-closures (is-captured-fv? (cons label name)))
                      (upper-closure (hash-ref *closures* label))
                      (fixed-free-offset
                       (if (and upper-closure (member label captured-closures))
                           (+ offset
                              (hash-count (lambda (_ v) (eq? v 'used))
                                          (insr-closure-frees upper-closure)))
                           offset)))
                 ;; NOTE:
                 ;; Fill the fixed offset, now the used free-vars number is
                 ;; confirmed.
                 (ordered-frees-add! (current-closure)
                                     (list name lexpr fixed-free-offset))

                 ;; Convert free-var to local-var, count the fixed-offset by
                 ;; appearing order of the free-var.
                 ;;
                 ;; FIXME:
                 ;; We use a list to wrap (offset) to distinct the regular local-var
                 ;; and the free-var converted local-var, this is somehow an ugly
                 ;; approach, it's better to find a better way in the future.
                 (make-insr-local '() name mode
                                  (list
                                   (fvar->lvar-fixed-offset
                                    (current-closure)
                                    (lambda (item)
                                      (eq? (car item) name))))
                                  keep?)))
              (else (throw 'laco-error 'closure-capture-fv
                           "Invalid pattern `~a' in ~a!" pattern name)))))
      (else lexpr)))
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map ccfv value))
     lexpr)
    (else lexpr)))

(define-pass closure-capture-free-vars lexpr (fix-local-offset (ccfv lexpr)))
