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

(define-module (laco parser)
  #:use-module (laco ast)
  #:use-module (laco module)
  #:use-module (laco types)
  #:use-module (laco utils)
  #:use-module (laco primitives)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (parser
            parse-module))

;; NOTE: we don't allow primitives-redefine, so this list is for checking.
(define *wrong-op-lst*
  `(quote quasiquote unquote unquote-splicing lambda if set!
          cond and or case let let* letrec begin do define delay
          ,@(@@ (laco primitives) *prim-table*)))

;; (define (_quasiquote obj)
;;   (match obj
;;     ((('unquote unq) rest ...) `(cons ,unq ,(_quasiquote rest)))
;;     (('unquote unq) unq)
;;     ((('unquote-splicing unqsp) rest ...) `(append ,unqsp ,(_quasiquote rest)))
;;     ((head rest ...) `(cons ,(_quasiquote head) ,(_quasiquote rest)))
;;     (else `(quote ,obj))))

(define* (_quasiquote obj #:optional (is-ref? #f))
  (match obj
    (() '())
    ((('unquote unq) rest ...)
     (cons (cons 'unquote (_quasiquote unq #t)) (_quasiquote rest)))
    (('unquote unq) (cons 'unquote unq))
    ((('unquote-splicing unqsp) rest ...)
     (cons (cons 'unquote-splicing (_quasiquote unqsp #t)) (_quasiquote rest)))
    ((head rest ...) (cons (_quasiquote head) (_quasiquote rest)))
    (else
     (if is-ref?
         (list obj)
         `(quote ,obj)))))

(define (list-comprehension->ast lst)
  (let lp ((next lst) (ret '()))
    (if (null? next)
        ret
        (match (car next)
          (('unquote-splicing e)
           (let ((ll (make-collection (reverse ret) 'list (length ret))))
             (lp (cdr next)
                 (make-call #f (make-ref #f 'append) (list ll e)))))
          (else (lp (cdr next) (cons (car next) ret)))))))

;; NOTE: we don't support forward-reference, although I'm willing to...
(define* (parse-it expr #:key (pos 'toplevel) (body-begin? #f) (use 'test) (op? #f))
  (match expr
    (((or 'define 'define*) pattern e ...)
     (let ((head (case (car expr)
                   ((define) 'lambda)
                   ((define*) 'lambda*))))
       ;; TODO: local define -> let binding
       ;;       We need to handle all the local definitions in a row
       (cond
        ((and (eq? pos 'closure-level) (not body-begin?))
         ;; According to R5Rs, there're only two situations to use
         ;; `define':
         ;; 1. In the top-level (toplevel definition).
         ;; 2. In the beginning of body (inner definition).
         (throw 'laco-error parse-it
                "Definition is only allowd in the top of context" expr))
        ((null? e)
         ;; R6Rs supports definition without expression, which implies to define
         ;; a var with the value `unspecifed'.
         ;; With respect to the future Scheme, we support it anyway.
         *laco/unspecified*)
        (else
         (match expr
           (('define (? symbol? var) val)
            (make-def (parse-it val #:body-begin? #t) var))
           (('define ((? symbol? var) args ...) body ...)
            (if (null? body)
                (throw 'laco-error parse-it
                       "No expressions in body in form `~a'" expr)
                (make-def (parse-it `(,head ,args ,@body) #:body-begin? #t) var)))
           ((_ ((? symbol var) (? args-with-keys args)) body ...)
            (when (eq? head 'define)
              (throw 'laco-error parse-it
                     "Source expression failed to match any pattern in form ~a"
                     expr))
            (make-def (parse-it `(define* ,args ,@body) #:body-begin? #t) var))
           (else (throw 'laco-error parse-it
                        "define: no pattern to match! `~a'" expr)))))))
    (('set! id val)
     (cond
      ((symbol? id) (make-assign (parse-it val) (parse-it id)))
      (else
       (throw 'laco-error parse-it
              (format #f "Bad `set!' special form: `~a'" expr)))))
    (('if tst then els ...)
     (let* ((e (parse-it tst #:use 'test))
            (b1 (parse-it then #:body-begin? #t))
            (b2 (match els
                  (()
                   ;; for (if #f e) situation, this expr should return `unspecified',
                   ;; so we generate `unspecified' here for later use.
                   (gen-constant 'unspecified))
                  ((e) (parse-it e #:body-begin? #t))
                  ((e redundant ...)
                   (throw 'laco-error parse-it
                          "if: redundant expr follow the second branch! `~a'" expr))
                  (else (throw 'laco-error parse-it
                               "if: can't match any cases! `~a'" expr)))))
       (make-branch (list e b1 b2))))
    (('cond body ...)
     (let ((tmpvar (gensym "cond.tmp.var-")))
       ;; Because I don't have time at present to write macro system, I have to write `cond'
       ;; as a built-in special form here. It'd be a macro (external special form) in the future.
       (match body
         ((('else rhs ...))
          (parse-it `(begin ,@rhs) #:pos 'closure-level #:body-begin? #t))
         (((tst '=> rhs) rest ...)
          (let ((x (tmpvar)))
            (parse-it `(let ((,x ,tst))
                         (if ,x
                             (,rhs ,x)
                             (cond ,@rest))))))
         (((tst rhs ...) rest ...)
          (parse-it `(if ,tst
                         (begin ,@rhs)
                         (cond ,@rest)))))))
    (('lambda pattern body body* ...)
     (let* ((ids (extract-ids pattern))
            (has-opt? (or (pair? pattern) (symbol? pattern))))
       (make-closure (parse-it `(begin ,body ,@body*)
                               #:pos 'closure-level #:body-begin? #t)
                     ids #f has-opt?)))
    (('lambda* pattern body body* ...)
     (throw 'laco-error parse-it "Sorry but lambda* is not prepared yet!")
     (let* ((ids (extract-ids pattern))
            (keys (extract-keys pattern))
            (has-opt? (or (pair? pattern) (symbol? pattern))))
       (make-closure (parse-it `(begin ,body ,@body*)
                               #:pos 'closure-level #:body-begin? #t)
                     ids keys has-opt?)))
    (('begin body ...)
     (cond
      ((and body-begin? (eq? pos 'closure-level))
       ;; Internal definition:
       ;; definition should be transformed to local bindings.
       (let-values (((rest defs) (get-all-defs body)))
         (parse-it (fold (lambda (x p)
                           (match x
                             ;; FIXME: Should be letrec*
                             (('define var expr) `(let* ((,var ,expr)) ,p))
                             (else (throw 'laco-error parse-it
                                          "Invalid local definition `~a'!" x))))
                         `(begin ,@rest) defs)
                   #:pos 'closure-level #:body-begin? #f)))
      (else
       ;; definition is in begin expr, but not in closure-toplevel, so the
       ;; definition is top level definition.
       (let lp((next body) (p #t) (ret '()))
         (cond
          ((null? next)
           (if (= 1 (length ret))
               (car ret)
               (make-seq (reverse ret))))
          (else
           (match (car next)
             ;; make sure inner definitions are available in a row
             (('define whatever ...)
              (lp (cdr next) p (cons (parse-it (car next) #:body-begin? p) ret)))
             (else (lp (cdr next) #f
                       (cons (parse-it (car next) #:body-begin? #f) ret))))))))))
    (('letrec ((ks vs) ...) body ...)
     (letrec ((dispatch
               (lambda (kk vv)
                 (cond
                  ((and (null? kk) (null? vv)) `(begin ,@body))
                  (else `(let ((,(car kk) #f))
                           ;; NOTE: make sure id is defined before val
                           (set! ,(car kk) ,(car vv))
                           ,(dispatch (cdr kk) (cdr vv))))))))
       (parse-it (dispatch ks vs))))
    (('let ((ks vs) ...) body ...) ; common let
     ;; NOTE: All bindings become single binding here by our CPS design
     (fold (lambda (k v p) (make-binding p (parse-it k) (parse-it v)))
           (parse-it `(begin ,@body)) ks vs))
    (('let id ((ks vs) ...) body ...) ; named let
     (parse-it `(letrec ((,id (lambda ,@ks ,@body))) (,id ,@vs))))
    (('let () body ...)
     (parse-it `(begin ,@body)))
    (('let* () body ...)
     (parse-it `(let () ,@body)))
    (('let* ((ks vs) ...) body ...)
     (letrec ((dispatch
               (lambda (kk vv)
                 (cond
                  ((and (null? kk) (null? vv)) `(begin ,@body))
                  (else
                   ;; NOTE: make sure each ks is defined in order
                   `(let ((,(car kk) ,(car vv)))
                      ,(dispatch (cdr kk) (cdr vv))))))))
       (parse-it (dispatch ks vs))))
    (('or rest ...)
     (let ((tmpvar (gensym "or.tmp.var-")))
       (cond
        ((null? rest) (gen-constant 'false))
        ((null? (cdr rest)) (parse-it (car rest)))
        (else
         (let ((b1 (tmpvar))
               (b2 (tmpvar)))
           (parse-it `((lambda (,b1 ,b2) (if ,b1 ,b1 (,b2)))
                       ,(car rest) (lambda () (or ,@(cdr rest))))))))))
    (('and rest ...)
     (let ((tmpvar (gensym "and.tmp.var-")))
       (cond
        ((null? rest) (gen-constant #t))
        ((null? (cdr rest)) (parse-it (car rest)))
        (else
         (let ((b1 (tmpvar))
               (b2 (tmpvar)))
           (parse-it `((lambda (,b1 ,b2) (if ,b1 (,b2) ,b1))
                       ,(car rest) (lambda () (and ,@(cdr rest))))))))))
    (('quote s)
     (match s
       ((or (? string?) (? number?) (? symbol?))
        (gen-constant s))
       ((? list?) (parse-it `(list ,@s)))
       (else (throw 'laco-error parse-it "quote: haven't support `~a'!" s))))
    (('unquote k) (parse-it k))
    (('unquote-splicing s) `(unquote-splicing ,(parse-it s)))
    (('quasiquote q)
     (let ((lst (map parse-it (_quasiquote q))))
       (list-comprehension->ast lst)))
    (('list e ...) (make-collection (map parse-it e) 'list (length e)))
    (('vector e ...) (make-collection e 'vector (vector-length e)))
    ((op args ...)
     (let ((f (parse-it op #:use 'value #:op? #t)))
       (cond
        ((not f) (throw 'laco-error parse-it "PROC `~a': unbound variable: " op))
        ((macro? f) ((macro-expander f) args))
        (else
         (make-call #f f (map (lambda (e) (parse-it e #:use 'value)) args))))))
    ((? symbol? k) (make-ref #f k))
    ;; NOTE: immediate check has to be the last one!!!
    ((? is-immediate? i) (gen-constant i))
    (else
     (throw 'laco-error parse-it
            "source expression failed to match any pattern in form `~a'"
            expr))))

(define (parser expr)
  (parse-it expr #:body-begin? #t))

(define (parse-module mod)
  (parser (mod-exprs mod)))
