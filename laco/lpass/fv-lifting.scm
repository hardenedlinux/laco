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

(define-module (laco lpass fv-lifting)
  #:use-module (laco types)
  #:use-module (laco object)
  #:use-module (laco utils)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

;; Lift free-vars if they're in tail-call body

(define need-lift? (make-parameter #f))
(define closure-escape? (make-parameter #f))

(define (fvl lexpr)
  (match lexpr
    (($ insr-label _ _ _ (($ insr-prelude _ _ m) rest ...))
     (if (or (= m *tail-rec*) (= m *tail-call*))
         (parameterize ((need-lift? #t))
           (insr-label-body-set! lexpr (map fvl (insr-label-body lexpr))))
         (insr-label-body-set! lexpr (map fvl (insr-label-body lexpr))))
     lexpr)
    (($ insr-proc _ _ _ _ _ (($ insr-prelude _ _ m) rest ...))
     (if (or (= m *tail-rec*) (= m *tail-call*))
         (parameterize ((need-lift? #t))
           (insr-proc-body-set! lexpr (map fvl (insr-proc-body lexpr))))
         (insr-proc-body-set! lexpr (map fvl (insr-proc-body lexpr))))
     lexpr)
    (($ insr-free _ label mode offset keep?)
     (if (and (not (closure-escape?)) (need-lift?))
         (make-insr-local '() mode offset keep?)
         lexpr))
    (($ insr-proc _ _ _ _ _ lexprs)
     (insr-proc-body-set! lexpr (map fvl lexprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ lexprs)
     (insr-proc-body-set! lexpr (map fvl lexprs))
     lexpr)
    (($ insr-label _ _ _ lexprs)
     (insr-label-body-set! lexpr (map fvl lexprs))
     lexpr)
    (($ insr-proc _ _ _ _ _ body)
     (insr-proc-body-set! lexpr (fvl body))
     lexpr)
    (($ insr-closure _ _ _ _ body mode)
     ;; NOTE: Don't lift closure captured free-vars
     lexpr)
    (($ list-object _ _ value)
     (list-object-value-set! lexpr (map fvl value))
     lexpr)
    (else lexpr)))

(define-pass free-vars-lifting lexpr (fvl lexpr))
