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

(define-module (laco parser)
  #:use-module (laco ast)
  #:use-module (laco module)
  #:use-module (laco types)
  #:use-module (laco utils)
  #:use-module (laco primitives)
  #:use-module (laco macro)
  #:use-module (laco reserved)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module ((rnrs) #:select (bytevector?))
  #:export (parser
            parse-module))

(define *opt-defs* (make-hash-table))
(define (opt-def! func def) (hash-set! *opt-defs* func def))
(define (opt-def-ref func) (hash-ref *opt-defs* func))
(define (opt-def-getter func getter var)
  (let ((def (opt-def-ref func)))
    (and def
         (let ((subx (ast-subx def)))
           (or (closure? subx)
               (throw 'laco-error opt-def-getter
                      "BUG: var `~a', `~a' is not closure!" func subx))
           (and=> (assoc-ref (getter subx) var) car)))))
(define (key-bind func var) (opt-def-getter func closure-keys var))
(define (opt-bind func var) (opt-def-getter func closure-opts var))
(define (def-attr-getter func getter)
  (let ((def (opt-def-ref func)))
    (and def
         (let ((subx (ast-subx def)))
           (or (closure? subx)
               (throw 'laco-error def-attr-getter
                      "BUG: var `~a', `~a' is not closure!" func))
           (getter subx)))))
(define (opts-of-def func) (def-attr-getter func closure-opts))
(define (keys-of-def func) (def-attr-getter func closure-keys))
(define (ids-of-def func) (def-attr-getter func closure-params))
(define (nargs-of-def func) (def-attr-getter func closure-nargs))
(define (cook-opt-args func args)
  (define (verify-kargs func ks)
    (let ((keys (keys-of-def func)))
      (for-each (lambda (x)
                  (when (not (assoc-ref keys (car x)))
                    (throw 'laco-error cook-opt-args
                           "Invalid keywork in function `~a'!" (car x))))
                ks)))
  (define (extract-opt-args func args)
    (let lp ((next args) (ret '()) (keys '()))
      (match next
        (()
         (let* ((r (reverse ret))
                (nargs (nargs-of-def func))
                (ol (map cadr (opts-of-def func)))
                (el (list-head r nargs))
                (opts (cond
                       ((> (length r) nargs)
                        (list-tail ol (- (length r) nargs)))
                       (else ol))))
           (values
            r
            opts
            keys)))
        (((? keyword? k) e rest ...)
         (lp rest ret (cons (list (keyword->symbol k) e) keys)))
        ((e rest ...)
         (lp rest (cons e ret) keys))
        (else (throw 'laco-error extract-opt-args
                     "Invalid pattern `~a`!" next)))))
  (let-values (((el opts ks) (extract-opt-args func args)))
    (let* ((keys (map (lambda (x)
                        (or (and=> (assoc-ref ks (car x)) car)
                            (cadr x)))
                      (keys-of-def func)))
           (ids (ids-of-def func)))
      (verify-kargs func ks)
      (let lp ((next el) (ret '()))
        (match next
          (()
           (values
            ids
            (map list
                 ids
                 `(,@(reverse ret) ,@opts ,@keys))))
          ((e rest ...)
           (lp rest (cons e ret)))
          (else
           (throw 'laco-error cook-opt-args "BUG: Wrong patterns `~a' in `a'!"
                  next func)))))))

(define* (_quasiquote obj #:optional (is-ref? #f))
  (match obj
    (() '())
    ((('unquote unq) rest ...)
     (cons (_quasiquote unq #t) (_quasiquote rest)))
    (('unquote unq) (cons 'unquote unq))
    ((('unquote-splicing unqsp) rest ...)
     (cons (cons 'unquote-splicing (_quasiquote unqsp #t)) (_quasiquote rest)))
    ((head rest ...) (cons (_quasiquote head) (_quasiquote rest)))
    (else
     (if is-ref?
         obj
         (if (symbol? obj)
             `(quote ,obj)
             obj)))))

(define (list-comprehension->ast lst parser)
  (define (quoted? x)
    (memq x '(quote unquote unquote-splicing)))
  (let lp ((next lst) (ret '()))
    (if (null? next)
        (make-collection (reverse ret) 'list (length ret))
        (match (car next)
          (((? quoted?) ,e)
           (lp (cdr next) (cons (parser (car next)) ret)))
          ((? list? ll)
           (lp (cdr next)
               (cons (make-collection (map parser ll) 'list (length ll))
                     ret)))
          ((? pair? p)
           (lp (cdr next)
               (cons (make-collection
                      (cons (parser (car p)) (parser (cdr p))) 'pair 2)
                     ret)))
          (('unquote-splicing e)
           (let ((ll (make-collection (map parser (reverse ret))
                                      'list (length ret))))
             (lp (cdr next)
                 (make-call #f (make-ref #f 'append) (list ll e)))))
          (else (lp (cdr next) (cons (parser (car next)) ret)))))))

(define (extract-all-local-defs-from-let-body body)
  (let lp ((next body) (defs '()) (fixed-body '()))
    (match next
      (()
       (values (reverse defs) fixed-body))
      ((('define v e) rest ...)
       (lp rest (cons (list v e) defs) fixed-body))
      ((('begin e ...) rest ...)
       (lp (append e rest) defs fixed-body))
      (else (lp (cdr next) defs `(,@fixed-body ,(car next)))))))

(define (extract-local-def-from-define-body exprs)
  (let lp ((next exprs) (local-defs '()) (real-body '()))
    (match next
      (() (values (reverse local-defs) (reverse real-body)))
      ((('define* pattern body ...) rest ...)
       (throw 'laco-error extract-local-def-from-define-body
              "Local define* is not supported yet!" (car next)))
      ((('define pattern body ...) rest ...)
       (lp (cdr next) (cons (car next) local-defs) real-body))
      (else (lp (cdr next) local-defs (cons (car next) real-body))))))

(define current-def (make-parameter #f))

;; NOTE: we don't support forward-reference, although I'm willing to...
(define* (parse-it expr #:key (pos 'toplevel) (body-begin? #f) (use 'test) (op? #f))
  (match expr
    (('define-syntax name spec)
     (make-macro name
                 spec
                 (macro-register! name (parse-macro-spec spec parse-it))))
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
                "Definition is only allowed in the top of context" expr))
        ((null? e)
         ;; R6Rs supports definition without expression, which implies to define
         ;; a var with the value `unspecified'.
         ;; With respect to the future Scheme, we support it anyway.
         *laco/unspecified*)
        (else
         (let-values (((local-defs real-body)
                       (extract-local-def-from-define-body e)))
           (cond
            ((null? local-defs)
             ;; No local definitions
             (match expr
               (((or 'define 'define*) ((? symbol? var) args ...) body ...)
                (if (null? body)
                    (throw 'laco-error parse-it
                           "No expressions in body in form `~a'" expr)
                    (let ((def (make-def (parse-it `(,head ,args ,@body)
                                                   #:body-begin? #t) var)))
                      (when (eq? head 'lambda*)
                        (hash-set! *opt-defs* var def))
                      def)))
               (('define (? symbol? var) val)
                (make-def (parse-it val #:body-begin? #t) var))
               ((_ ((? symbol var) (? args-with-keys args)) body ...)
                (when (eq? head 'define)
                  (throw 'laco-error parse-it
                         "Source expression failed to match any pattern in form ~a"
                         expr))
                (make-def (parse-it `(define* ,args ,@body) #:body-begin? #t) var))
               (else (throw 'laco-error parse-it
                            "define: no pattern to match! `~a'" expr))))
            (else
             ;; extract local definitions
             (match expr
               (((or 'define 'define*) ((? symbol? var) args ...) body ...)
                (let ((parsed-body
                       (parse-it
                        `(,head
                          ,args
                          ,@(let extract ((next-def local-defs))
                              (match next-def
                                (() real-body)
                                (((or ('define (? symbol? v) ('lambda (args ...)) body ...)
                                      ('define ((? symbol? v) args ...) body ...))
                                  rest ...)
                                 `((letrec ((,v (lambda (,@args) ,@body)))
                                     ,@(extract (cdr next-def)))))
                                (else
                                 (throw 'laco-error parse-it
                                        "Wrong local define syntax!" next-def)))))
                        #:body-begin? #t)))
                  (make-def parsed-body var)))
               (else
                (throw 'laco-error parse-it
                       "local define: no pattern to match! `~a'" expr))))))))))
    (('set! v val)
     (let ((var (parse-it v)))
       (cond
        ((ref? var) (make-assign (parse-it val) var))
        (else
         (throw 'laco-error parse-it
                (format #f "Invalid variable or reference `~a' in `set!' special form!" expr))))))
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
          (let ((x tmpvar))
            (parse-it `(let ((,x ,tst))
                         (if ,x
                             (,rhs ,x)
                             (cond ,@rest))))))
         (((tst rhs ...) rest ...)
          (parse-it `(if ,tst
                         (begin ,@rhs)
                         (cond ,@rest)))))))
    (('lambda pattern body ...)
     (let-values (((kvs fb) (extract-all-local-defs-from-let-body body)))
       (let* ((ids (extract-ids pattern))
              (has-opt? (or (pair? pattern) (symbol? pattern))))
         (make-closure (parse-it (if (null? kvs)
                                     `(begin ,@fb)
                                     `(letrec* ,kvs (begin ,@fb)))
                                 #:pos 'closure-level #:body-begin? #t)
                       ids '() '() (length ids) (current-def)))))
    (('lambda* pattern body body* ...)
     (let ((ids (extract-ids pattern)))
       (let-values (((keys opts) (extract-keys pattern)))
         (make-closure (parse-it `(begin ,body ,@body*)
                                 #:pos 'closure-level #:body-begin? #t)
                       (append ids (map car opts) (map car keys))
                       keys opts (length ids) (current-def)))))
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
       ;; If the definition is in begin expr, but not in closure-toplevel, this
       ;; definition is the top level definition.
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
    (((or 'letrec 'letrec*) ((ks vs) ...) body ...)
     (letrec ((dispatch
               (lambda (kk vv)
                 (cond
                  ((and (null? kk) (null? vv)) `(begin ,@body))
                  (else
                   `(let ((,(car kk) #f))
                      ;; NOTE: make sure id is defined before val
                      (set! ,(car kk) ,(car vv))
                      ,(dispatch (cdr kk) (cdr vv))))))))
       (parse-it (dispatch ks vs))))
    (('let ((ks vs) ...) body ...) ; common let
     ;; NOTE: All bindings become single binding here by our CPS design
     (let-values (((kvs fb) (extract-all-local-defs-from-let-body body)))
       (fold (lambda (k v p) (make-binding p (parse-it k) (parse-it v)))
             (parse-it (if (null? kvs)
                           `(begin ,@fb)
                           `(letrec* ,kvs (begin ,@fb)))) ks vs)))
    (('let id ((ks vs) ...) body ...) ; named let
     (let-values (((kvs fb) (extract-all-local-defs-from-let-body body)))
       (parameterize ((current-def id))
         (parse-it `(letrec ((,id (lambda ,ks ,(if (null? kvs)
                                                   `(begin ,@fb)
                                                   `(letrec* ,kvs (begin ,@fb))))))
                      (,id ,@vs))))))
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
     (cond
      ((null? rest) (gen-constant #f))
      ((null? (cdr rest)) (parse-it (car rest)))
      (else
       (parse-it `(let ((cnd ,(car rest)))
                    (if cnd cnd (or ,@(cdr rest))))))))
    (('and rest ...)
     (cond
      ((null? rest) (gen-constant #t))
      ((null? (cdr rest)) (parse-it (car rest)))
      (else
       (parse-it `(let ((cnd ,(car rest)))
                    (if cnd (and ,@(cdr rest)) #f))))))
    (('quote s)
     (match s
       ((or (? string?) (? number?) (? symbol?) (? char?))
        (gen-constant s))
       ((? list?) (parse-it `(list ,@(map _quasiquote s))))
       ((? pair?) (parse-it `(cons ,(_quasiquote (car s)) ,(_quasiquote (cdr s)))))
       (else (throw 'laco-error parse-it "quote: haven't support `~a'!" s))))
    (('unquote k) (parse-it k))
    (('unquote-splicing s) `(unquote-splicing ,(parse-it s)))
    (('quasiquote q)
     (list-comprehension->ast (_quasiquote q) parse-it))
    (('cons x y) (make-collection (map parse-it (list x y)) 'pair 2))
    (('list e ...) (make-collection (map parse-it e)
                                    'list (length e)))
    (('vector e ...) (make-collection (map parse-it e) 'vector (vector-length e)))
    ((? bytevector? e) (gen-constant e))
    (((? search-macro-def m) args ...)
     ;; is a macro expanding
     (let ((syntax-transformer (search-macro-def m)))
       (parameterize ((current-macro-context m))
         (parse-it (syntax-transformer args)))))
    ((op args ...)
     (let ((f (parse-it op #:use 'value #:op? #t)))
       (cond
        ((not f) (throw 'laco-error parse-it "PROC `~a': unbound variable: " op))
        ((and (not (object-property expr 'is-extracted?)) (opt-def-ref (ref-var f)))
         (let-values (((ids bindings) (cook-opt-args (ref-var f) args)))
           (let ((new-expr `(,op ,@ids)))
             (set-object-property! new-expr 'is-extracted? #t)
             (parse-it `(let* (,@bindings) ,new-expr)))
           #;
           (make-call #f f              ; ;
           (map (lambda (e) (parse-it e #:use 'value)) (map cadr bindings)))))
        (else
         (make-call #f f
                    (map (lambda (e) (parse-it e #:use 'value)) args))))))
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
