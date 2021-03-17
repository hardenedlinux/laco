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

(define-module (laco lpass fv-lifting)
  #:use-module (laco types)
  #:use-module (laco object)
  #:use-module (laco utils)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; Lift free-vars in the current closure if they're in tail-call body

(define current-closure-name (make-parameter "global"))
(define current-frees (make-parameter #f))
(define need-lift? (make-parameter #f))
(define closure-escape? (make-parameter #f))
(define need-capture? (make-parameter #f))
(define *closures* (make-hash-table))

(define (fvl lexpr)
  (match lexpr
    (($ insr-label _ _ label (($ insr-prelude _ _ _ m _) rest ...))
     (when (= m *normal-call*)
       ;; (pk "check" label m *normal-call*)
       (normal-call-register! label))
     (if (and (not (need-capture?)) (or (= m *tail-rec*) (= m *tail-call*)))
         (parameterize ((need-lift? #t))
           (insr-label-body-set! lexpr (map fvl (insr-label-body lexpr))))
         (insr-label-body-set! lexpr (map fvl (insr-label-body lexpr))))
     lexpr)
    (($ insr-proc _ _ label _ _ (($ insr-prelude _ _ _ m _) rest ...))
     (when (= m *normal-call*)
       ;; (pk "check" label m *normal-call*)
       (normal-call-register! label))
     (if (and (not (need-capture?)) (or (= m *tail-rec*) (= m *tail-call*)))
         (parameterize ((need-lift? #t))
           (insr-proc-body-set! lexpr (map fvl (insr-proc-body lexpr))))
         (insr-proc-body-set! lexpr (map fvl (insr-proc-body lexpr))))
     lexpr)
    (($ insr-free _ label name mode offset keep?)
     (cond
      ((and (not (need-capture?)) (need-lift?))
       (let ((closure (hash-ref *closures* label)))
         (when closure
           (hash-remove! (insr-closure-frees closure) name))
         (make-insr-local '() name mode offset keep?)))
      (else
       (when (not (string=? label (current-closure-name)))
         ;; Tag the captured free-var
         (register-captured-fv!
          (cons label name)
          (cons label (is-captured-fv? name))))
       lexpr)))
    (($ insr-proc _ _ _ _ _ lexprs)
     (insr-proc-body-set! lexpr (map fvl lexprs))
     lexpr)
    (($ insr-label _ _ _ lexprs)
     (insr-label-body-set! lexpr (map fvl lexprs))
     lexpr)
    (($ insr-closure _ label _ frees (($ insr-prelude _ _ _ m _) rest ...) mode)
     (=> back)
     (when (= m *normal-call*)
       ;; (pk "check" label m *normal-call*)
       (normal-call-register! label))
     (back))
    (($ insr-closure _ name _ frees body mode)
     ;; NOTE: Don't lift closure captured free-vars
     (parameterize ((current-closure-name name)
                    (need-capture? #t))
       (hash-set! *closures* name lexpr)
       (insr-closure-body-set! lexpr (map fvl body))
       lexpr))
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map fvl value))
     lexpr)
    (else lexpr)))

(define-pass free-vars-lifting lexpr (fvl lexpr))
