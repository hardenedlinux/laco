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

(define-module (laco lpass reduce-labels)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco lir)
  #:use-module (laco pass)
  #:use-module (laco primitives)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define (rename-label! lexpr old-label new-label)
  (define (hit? l) (string=? l old-label))
  (match lexpr
    (($ insr-proc _ _ _ _ arity body)
     (insr-proc-body-set! lexpr (rename-label! body old-label new-label))
     lexpr)
    (($ insr-label _ label _ exprs)
     (insr-label-body-set!
      lexpr
      (map (lambda (e) (rename-label! e old-label new-label)) exprs))
     lexpr)
    (($ insr-proc-call _ _ (? hit?) _)
     (insr-proc-call-label-set! lexpr new-label)
     lexpr)
    (($ insr-fjump _ (? hit?))
     (insr-fjump-label-set! lexpr new-label)
     lexpr)
    (($ insr-free _ (? hit?) _ _ _ _)
     (insr-free-label-set! lexpr new-label)
     lexpr)
    (($ insr-closure _ label _ _ body _)
     (insr-closure-body-set!
      lexpr
      (map (lambda (e) (rename-label! e old-label new-label)) body))
     lexpr)
    (else lexpr)))

(define (rl lexpr)
  (match lexpr
    (($ insr-label _ _ label (($ insr-label _ _ label2 lexprs2) rest ...))
     (insr-label-body-set!
      lexpr
      (map rl `(,@(map (lambda (e) (rename-label! e label2 label)) lexprs2)
                ,@rest)))
     (rl lexpr))
    (($ insr-proc _ _ label _ _ (($ insr-label _ _ label2 lexprs2) rest ...))
     (cond
      ((or (string=? label "#____principio") (is-scoped-label? label2))
       ;; NOTE: Don't reduce the first label in `principio', since we may need to
       ;;       fetch vars in the env of `principio'.
       (insr-proc-body-set!
        lexpr
        (map rl (insr-proc-body lexpr)))
       lexpr)
      (else
       (insr-proc-body-set!
        lexpr
        (map rl `(,@(map (lambda (e) (rename-label! e label2 label)) lexprs2)
                  ,@rest)))
       (rl lexpr))))
    (($ insr-label _ _ label lexprs)
     (insr-label-body-set! lexpr (map rl lexprs))
     lexpr)
    (($ insr-proc _ _ label _ _ body)
     (insr-proc-body-set! lexpr (rl body))
     lexpr)
    (($ insr-closure _ label _ _ (($ insr-label _ _ label2 lexprs2) rest ...) _)
     (insr-closure-body-set!
      lexpr
      (map rl `(,@(map (lambda (e) (rename-label! e label2 label)) lexprs2)
                ,@rest)))
     lexpr)
    (else lexpr)))

(define-pass reduce-labels lexpr (rl lexpr))
