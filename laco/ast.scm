;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2025
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

(define-module (laco ast)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (ice-9 match)
  #:use-module (laco records)
  #:export (ast
            make-ast ast?
            ast-subx

            def make-def def?
            def-var

            ref make-ref ref?
            ref-var

            assign make-assign assign?
            assign-var

            branch make-branch branch?

            call make-call call?
            call-op call-args

            closure make-closure closure?
            closure-params closure-keys closure-opts
            closure-nargs

            seq make-seq seq?

            binding make-binding binding?
            binding-id

            var make-var var?
            var-uid var-global?
            new-var

            macro make-macro macro?
            macro-name macro-src macro-expander

            syntax make-syntax syntax?
            syntax-reserves syntax-expander

            collection make-collection collection?
            collection-type collection-size

            ->special-form
            ast->src))

;; AST type
(define-record-type ast (fields subx))

(define-record-type ref (parent ast) (fields var)) ; var ref
(define-record-type def (parent ast) (fields var)) ; var define
(define-record-type assign (parent ast) (fields var)) ; var assignment
(define-record-type branch (parent ast)) ; condition
;; calling a function, ast is a list: (func args ...)
;; we don't distinct prim call in AST
(define-record-type call (parent ast) (fields op args))
(define-record-type closure (parent ast)
                    (fields params keys opts nargs def)) ; closure
(define-record-type seq (parent ast))              ; sequence

(define-record-type collection (parent ast) (fields type size))

;; for env, var, and macros
;; NOTE: id and val are single in AST
(define-record-type binding (parent ast) (fields id val))
(define-record-type var (parent binding) (fields uid global?))
(define* (new-var id #:optional (global? #f)) (make-var id (newsym id) global?))

(define-record-type macro (parent ast) (fields name src expander))

;; In Scheme, there'd be these special forms (at least):
;; define lambda let let* letrec quote quasiquote set! if case
;; cond begin do and or let-syntax letrec-syntax delay
;; And macros!
(define-record-type special-form (parent binding) (fields expander))

(define* (ast->src node #:optional (hide-begin? #t))
  (pk 'node node)
  (match node
    (($ constant _ val type) (unless (eq? 'unspecified type) val))
    (($ collection ($ ast _ subx) type size)
     (match type
       ('list `(list ,@(map ast->src subx)))
       ('pair (cons (ast->src (car subx)) (ast->src (cdr subx))))
       ('vector (list->vector (map ast->src subx)))
       (else (throw 'laco-error "Invalid collection type!" type))))
    (($ def ($ ast _ subx) v) `(define ,(ast->src v) ,(ast->src subx)))
    (($ macro ($ ast _ subx) name src _) `(define-syntax ,name ,src))
    (($ ref _ v) (ast->src v))
    (($ assign ($ ast _ subx) v) `(set! ,(ast->src v) ,(ast->src subx)))
    (($ branch ($ ast _ subx))
     (match subx
       ((c b1 b2) `(if ,(ast->src c) ,(ast->src b1 #f) ,(ast->src b2 #f)))
       (else (throw 'laco-error "I don't know what's wrong dude!!!" subx))))
    (($ call _ op args) `(,(ast->src op) ,@(map ast->src args)))
    (($ closure ($ ast _ subx) params keys opts _ _)
     (define (ids-filter p o k)
       (filter (lambda (v) (not (or (assoc-ref o v) (assoc-ref k v)))) p))
     (let ((o (map (lambda (e) (list (car e) (ast->src (cadr e)))) opts))
           (k (map (lambda (e) (list (car e) (ast->src (cadr e)))) keys)))
       `(,(if (or (not (null? o)) (not (null? k))) 'lambda* 'lambda)
         (,@(map ast->src (ids-filter params o k))
          ,@(if (null? o) o '(#:optional)) ,@o
          ,@(if (null? k) k '(#:key)) ,@k)
         ,(ast->src subx))))
    (($ seq ($ ast _ subx))
     (cond
      ((zero? (length subx))
       (throw 'laco-error ast->src "Well, null seq dude huh??"))
      ((= (length subx) 1) (ast->src (car subx)))
      (hide-begin? (map ast->src subx))
      (else `(begin ,@(map ast->src subx)))))
    (($ binding ($ ast _ body) var value)
     `(let ((,(ast->src var) ,(ast->src value))) ,(ast->src body #f)))
    (else node)))
