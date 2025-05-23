;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2020-2022
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

(define-module (laco cps)
  #:use-module (laco utils)
  #:use-module (laco env)
  #:use-module (laco ast)
  #:use-module (laco types)
  #:use-module (laco primitives)
  #:use-module (laco records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (cps-list?

            current-env
            current-kont
            current-def

            cps cps?
            cps-kont cps-kont-set!
            cps-name cps-name-set!
            cps-attr cps-attr-set!
            cps-property-set!
            cps-property-ref
            cps-property-remove!

            lambda/k lambda/k?
            lambda/k-args lambda/k-args-set!
            lambda/k-body lambda/k-body-set!
            make-lambda/k new-lambda/k

            closure/k closure/k?
            closure/k-env closure/k-env-set!
            closure/k-body closure/k-body-set!
            make-closure/k new-closure/k

            constant/k constant/k?
            constant/k-value constant/k-value-set!
            make-constant/k new-constant/k

            bind-special-form/k bind-special-form/k?
            bind-special-form/k-var bind-special-form/k-var-set!
            bind-special-form/k-value bind-special-form/k-value-set!
            bind-special-form/k-body bind-special-form/k-body-set!

            letval/k letval/k?
            make-letval/k new-letval/k

            letfun/k letfun/k?
            make-letfun/k new-letfun/k

            letcont/k letcont/k?
            make-letcont/k new-letcont/k

            branch/k branch/k?
            branch/k-cnd branch/k-cnd-set!
            branch/k-tbranch branch/k-tbranch-set!
            branch/k-fbranch branch/k-fbranch-set!
            make-branch/k new-branch/k

            collection/k collection/k?
            collection/k-var collection/k-var-set!
            collection/k-type collection/k-type-set!
            collection/k-size collection/k-size-set!
            collection/k-value collection/k-value-set!
            make-collection/k new-collection/k

            seq/k seq/k?
            seq/k-exprs seq/k-exprs-set!
            make-seq/k new-seq/k

            app/k app/k?
            app/k-func app/k-func-set!
            app/k-args app/k-args-set!
            make-app/k new-app/k

            assign/k assign/k?
            assign/k-var assign/k-var-set!
            assign/k-expr assign/k-expr-set!
            make-assign/k new-assign/k

            cont-apply

            union diff insec
            free-vars names bound-vars all-ref-vars
            make-ref-table

            alpha-renaming

            comp-cps
            ast->cps
            cps->expr
            cps-show
            cps->name
            cps->name-string
            kont-eq?

            tag-proper-tail-recursion!
            is-proper-tail-recursion?
            is-tail-call?
            is-escaped?
            closure-was-named-let?
            clean-tail-call!

            top-level->src
            cps->expr/g

            fix-fv))

;; kontext means kontinuation-context

;; According to <<Compiling with continuations, continued>> - Andrew Kennedy
;; The principle to design CPS IR:
;; 1. All intermediate results are named, that is to say, we use a special binding
;;    to hold the intermediate result.
;; 2. There're 2 kinds of application:
;;    a. Translation-time application, which will not be reduced to normal-form in
;;       the translation.
;;    b.
;; 3. One-pass CPS translation, which introduce no 'administrative-reduction' that
;;    must be removed in a seperated phase.

(define (valid-expr? x)
  (or (cps? x)
      (constant? x)
      (id? x)))

(define (expr-list? lst)
  (make-object-list-pred lst valid-expr?))

(define (cps-list? lst)
  (make-object-list-pred lst cps?))

(define current-env (make-parameter *top-level*))
(define current-kont (make-parameter prim:return))
(define current-def (make-parameter #f))

(define-typed-record cps
  (fields
   ;; the current continuation
   ;; TODO: capture the current continuation
   (kont (lambda (x) (or (id? x) (primitive? x) (cps? x))))
   (name id?) ; the unique name of the continuation
   (attr list?))) ; attributes of the continuation

(define (cps-property-set! cexp k v)
  (let ((attr (cps-attr cexp)))
    (cps-attr-set! cexp (assoc-set! attr k v))))

(define (cps-property-ref cexp k)
  (let ((attr (cps-attr cexp)))
    (assoc-ref attr k)))

(define (cps-property-remove! cexp k)
  (let ((attr (cps-attr cexp)))
    (cps-attr-set! cexp (assoc-remove! attr k))))

(define-typed-record lambda/k (parent cps)
  (fields
   (args id-list?)
   (body valid-expr?)))
(define* (new-lambda/k args body #:key (kont prim:return) (name (new-id "#kont-"))
                       (attr '()))
  (make-lambda/k (list kont name attr) args body))

(define-typed-record closure/k (parent cps)
  (fields
   (env env?)
   (body valid-expr?)))
(define* (new-closure/k env body #:key (kont prim:return) (name (new-id "#kont-"))
                        (attr '()))
  (make-closure/k (list kont name attr) env body))

(define-typed-record constant/k (parent cps)
  (fields
   (value constant?)))
(define* (new-constant/k value #:key (kont prim:return) (name (new-id "#kont-"))
                         (attr '()))
  (make-constant/k (list kont name attr) value))

(define-typed-record bind-special-form/k (parent cps)
  (fields
   (var id?)
   (value cps?)
   (body cps?)))

(define-typed-record letval/k (parent bind-special-form/k))
(define* (new-letval/k var value body #:key (kont prim:return)
                       (name (new-id "#kont-"))
                       (attr '()))
  (make-letval/k (list kont name attr) var value body))

(define-typed-record letfun/k (parent bind-special-form/k))
(define* (new-letfun/k var value body #:key (kont prim:return)
                       (name (new-id "#kont-"))
                       (attr '()))
  (make-letfun/k (list kont name attr) var value body))

(define-typed-record letcont/k (parent bind-special-form/k))
(define* (new-letcont/k var value body #:key (kont prim:return)
                        (name (new-id "#kont-"))
                        (attr '()))
  (make-letcont/k (list kont name attr) var value body))

(define-typed-record branch/k (parent cps)
  (fields
   (cnd valid-expr?)
   (tbranch valid-expr?)
   (fbranch valid-expr?)))
(define* (new-branch/k cnd b1 b2 #:key (kont prim:return)
                       (name (new-id "#kont-"))
                       (attr '()))
  (make-branch/k (list kont name attr) cnd b1 b2))

(define-typed-record collection/k (parent cps)
  (fields
   (var id?)
   (type symbol?)
   (size integer?)
   (value any?)))
(define* (new-collection/k cname type size value
                           #:key (kont prim:return)
                           (name (new-id "#kont-"))
                           (attr '()))
  (make-collection/k (list kont name attr) cname type size value))

(define-typed-record seq/k (parent cps)
  (fields
   (exprs expr-list?)))
(define* (new-seq/k exprs #:key (kont prim:return)
                    (name (new-id "#kont-"))
                    (attr '()))
  (make-seq/k (list kont name attr) exprs))

(define (applicable? x)
  (or (letfun/k? x) (primitive? x) (lambda/k? x) (closure/k? x) (app/k? x)
      ;; FIXME: Not all id, should be the registered proc id
      (id? x)))
(define (valid-arg? x)
  (or (id-list? x)
      (cps-list? x)
      (cps? x)
      (id? x)
      (primitive? x)))
(define-typed-record app/k (parent cps)
  (fields
   (func applicable?)
   (args list?)))
(define* (new-app/k f args #:key (kont prim:return)
                    (name (new-id "#kont-"))
                    (attr '()))
  (make-app/k (list kont name attr)
              f (if (list? args) args (list args))))

(define (cont-apply f e)
  (make-app/k (list prim:return (new-id "#kont-") (new-id "#k-")) f e))

(define-typed-record assign/k (parent cps)
  (fields
   (var id?)
   (expr valid-expr?)))
(define* (new-assign/k v e #:key (kont prim:return)
                       (name (new-id "#kont-4"))
                       (attr '((side-effect? . #t))))
  (make-assign/k (list kont name attr) v e))

(define* (vars-fold rec acc op expr #:key (filter-prim? #f))
  (match expr
    ((? id? id) (list id))
    ((? primitive? p) (if filter-prim? '() (list p)))
    (($ assign/k _ ref expr)
     (op (rec expr) (list ref)))
    (($ lambda/k _ args body)
     (op (rec body) args))
    (($ closure/k _ env body)
     (op (rec body) (queue-slots (env-bindings env))))
    (($ branch/k _ cnd b1 b2)
     (apply acc (map rec (list cnd b1 b2))))
    (($ seq/k _ exprs)
     (apply acc (map rec exprs)))
    (($ app/k _ f args)
     (apply acc (map rec (reverse `(,f ,@args)))))
    (($ collection/k _ _ _ _ value)
     (apply acc `(,@(map rec value))))
    ((? bind-special-form/k?)
     (let ((var (bind-special-form/k-var expr))
           (value (bind-special-form/k-value expr))
           (body (bind-special-form/k-body expr)))
       (apply acc (map rec (list var value body)))))
    (else '())))

(define (union . args) (apply lset-union id-eq? args))
(define (diff . args) (apply lset-difference id-eq? args))
(define (insec . args) (apply lset-intersection id-eq? args))
(define (sym-insec . args) (apply lset-intersection eq? args))

(define* (free-vars expr #:optional (refresh? #f))
  (cond
   ((and (not refresh?) (assoc-ref (cps-attr expr) 'free-vars)) => identity)
   (else
    (let ((fv (vars-fold free-vars union diff expr #:filter-prim? #t))
          (attr (cps-attr expr)))
      (when attr (set! attr (cons (cons 'free-vars fv) attr)))
      fv))))

(define* (names expr #:optional (refresh? #f))
  (cond
   ((and (not refresh?) (assoc-ref (cps-attr expr) 'var-names)) => identity)
   (else
    (let ((nv (vars-fold names union union expr))
          (attr (cps-attr expr)))
      (set! attr (cons (cons 'var-names nv) attr))
      nv))))

;; NOTE: free-vars <= names, so diff is enough
(define (bound-vars expr) (apply diff (names expr) (free-vars expr)))
(define (all-ref-vars expr) (vars-fold all-ref-vars append append expr))

;; The all-ref-vars will count all appear variables, include the local definition,
;; so it has to be performed after these two steps:
;; 1. dead-variable-elimination
;; 2. alpha-renaming.
;; rules:
;; cnt == 1 means it's free-var
;; cnt == 2 means it should be inlined
;; cnt > 2, leave it as it is
(define (make-ref-table expr)
  (let ((vl (all-ref-vars expr))
        (ht (make-hash-table)))
    (for-each (lambda (v)
                (hash-set! ht v (1+ (hash-ref ht v 0))))
              vl)
    ht))

;; cps -> symbol-list -> id-list
(define (alpha-renaming expr old new)
  (define (new->index eid)
    (list-index (lambda (sym) (eq? sym (id-name eid))) old))
  (define (rename eid)
    (cond
     ((new->index eid)
      => (lambda (i) (list-ref new i)))
     (else eid)))
  (for-each (lambda (o n) (renamed-keep! o (id-name n))) old new)
  (match expr
    (($ lambda/k _ fargs body)
     (cond
      ((null? (sym-insec (map id-name fargs) old))
       (lambda/k-body-set! expr (alpha-renaming body old new))
       expr)
      ;; new binding, don't apply rename more deeply
      (else expr)))
    (($ assign/k _ v e)
     (let ((i (new->index v)))
       (when (and i (is-effect-var? (id-name v)))
         (effect-var-register! (id-name (list-ref new i)))))
     (assign/k-var-set! expr (alpha-renaming v old new))
     (assign/k-expr-set! expr (alpha-renaming e old new))
     expr)
    (($ app/k _ f e)
     ;;(format #t "alpha 1 ~a~%" expr)
     (app/k-func-set! expr (alpha-renaming f old new))
     (app/k-args-set! expr (map (lambda (ee) (alpha-renaming ee old new)) e))
     expr)
    (($ seq/k _ e)
     ;;(format #t "alpha 2 ~a~%" expr)
     (seq/k-exprs-set! expr (map (lambda (ee) (alpha-renaming ee old new)) e))
     expr)
    ((? bind-special-form/k?)
     (bind-special-form/k-value-set!
      expr (alpha-renaming (bind-special-form/k-value expr) old new))
     (bind-special-form/k-body-set!
      expr (alpha-renaming (bind-special-form/k-body expr) old new))
     expr)
    (($ branch/k _ cnd b1 b2)
     (branch/k-cnd-set! expr (alpha-renaming cnd old new))
     (branch/k-tbranch-set! expr (alpha-renaming b1 old new))
     (branch/k-fbranch-set! expr (alpha-renaming b2 old new))
     expr)
    ((? id?) (rename expr))
    (else expr)))

(define (clone expr)
  (define new-name (new-id "#kont-"))
  (match expr
    (($ lambda/k ($ cps _ kont _ attr) fargs body)
     (make-lambda/k (list kont new-name attr) fargs (clone body)))
    (($ app/k ($ cps _ kont _ attr) func fargs)
     (make-app/k (list kont new-name attr)
                 (clone func) (map clone fargs)))
    (($ seq/k ($ cps _ kont _ attr) exprs)
     (make-seq/k (list kont new-name attr) (map clone exprs)))
    (($ letcont/k ($ cps _ kont _ attr) var value body)
     (make-letcont/k (list kont new-name attr)
                     var (clone value) (clone body)))
    (($ letfun/k ($ cps _ kont _ attr) var value body)
     (make-letfun/k (list kont new-name attr)
                    var (clone value) (clone body)))
    (($ letval/k ($ cps _ kont _ attr) var value body)
     (make-letval/k (list kont new-name attr)
                    var (clone value) (clone body)))
    (($ branch/k ($ cps _ kont _ attr) cnd b1 b2)
     (make-branch/k (list kont new-name attr)
                    (clone cnd)
                    (clone b1)
                    (clone b2)))
    (($ assign/k ($ cps _ kont _ attr) v e)
     (make-assign/k (list kont new-name attr)
                    v (clone e)))
    (($ collection/k ($ cps _ kont _ attr) var type size value)
     (make-collection/k (list kont new-name attr)
                        var type size
                        (map identity value)))
    (else expr)))

(define* (comp-cps expr #:optional (cont prim:return))
  (match expr
    (($ closure ($ ast _ body) params _ _ _ sym)
     (renamed-register! sym)
     (let* ((fname (new-id "#func-"))
            (fk (new-id "#kont-"))
            (nv (map new-id params))
            (attr (cond
                   (sym => (lambda (nv) `((def . ,sym))))
                   (else '())))
            (fun (new-lambda/k `(,fk ,@nv)
                               (alpha-renaming (ast->cps body fk) params nv)
                               #:name fk #:kont fk #:attr attr)))
       (new-letfun/k fname fun (new-app/k cont
                                          fname #:kont cont)
                     #:kont cont)))
    (($ binding ($ ast _ body) ($ ref _ var) value)
     (let* ((jname (new-id "#jcont-"))
            (ov (new-id var #f))
            (nv (new-id var))
            (fk (new-id "#letcont/k-"))
            (jcont (new-lambda/k
                    (list nv)
                    (alpha-renaming (comp-cps body cont)
                                    (list var) (list nv))
                    #:kont cont)))
       (new-letcont/k jname jcont
                      (alpha-renaming (ast->cps value jname) (list var) (list nv))
                      #:kont cont)))
    (($ branch ($ ast _ (cnd b1 b2)))
     (let* ((jname (new-id "#jcont-branch-"))
            (kname (new-id "#kont-"))
            (k1 (new-id "#letcont/k-branch-1-"))
            (k2 (new-id "#letcont/k-branch-2-"))
            (kont2
             (new-letcont/k k2
                            (new-lambda/k '() (ast->cps b2 cont)
                                          #:kont jname #:attr '((branch . #t)))
                            (new-branch/k kname k1 k2)))
            (kont1
             (new-letcont/k k1
                            (new-lambda/k '() (ast->cps b1 (clone cont))
                                          #:kont jname #:attr '((branch . #t)))
                            kont2))
            (kont3
             ;; According to Kennedy's, we add a local continuation here
             (new-lambda/k (list kname)
                           (new-letcont/k jname
                                          (new-lambda/k '() kname)
                                          kont1))))
       (ast->cps cnd kont3)))
    (else (ast->cps expr cont))))

(define* (ast->cps expr #:optional (cont prim:return))
  (match expr
    ;; FIXME: distinct value and function for the convenient of fun-inline.
    (($ closure ($ ast _ body) params _ _ _ sym)
     (renamed-register! sym)
     (let* ((fname (new-id "#func-"))
            (fk (new-id "#kont-"))
            (nv (map new-id params))
            (attr (cond
                   (sym => (lambda (nv) `((def . ,sym))))
                   (else '())))
            (fun (new-lambda/k `(,fk ,@nv)
                               (alpha-renaming (ast->cps body fk) params nv)
                               #:name fk #:kont fk #:attr attr)))
       (new-letfun/k fname fun (new-app/k cont fname
                                          #:kont cont)
                     #:kont cont)))
    (($ macro ($ ast _ body) name src expander)
     ;; The macro definition will be ignored in CPS, only take care of it
     ;; when it's expanded somewhere.
     (let ((x (new-id (format #f "#macro:~a" name)))
           (cst (new-constant/k *laco/unspecified*)))
       (new-letval/k x cst (new-app/k cont x #:kont cont)
                     #:kont cont)))
    (($ def ($ ast _ body) var)
     ;; NOTE: The local function definition should be converted to let-binding
     ;;       by AST builder. So the definition that appears here are top-level.
     ;; NOTE: And the local function definition will be lifted to top-level later.
     (let ((e (ast->cps body cont)))
       (when (not (null? (insec (free-vars e) (list (new-id var #f)))))
         ;; We tag recursive here to avoid incorrect inlining, please notice that
         ;; recursive doesn't mean recursivly defined, if the `var' was referred
         ;; within the body, then it's still recursive and can't be inlined.
         (register-as-recursive! var))
       (cond
        ((top-level-ref var)
         ;; If it exists, then transform it to an assignment
         (new-assign/k (new-id var #f) e))
        (else
         (top-level-set! var e)
         *definition-in-cps*))))
    (($ binding ($ ast _ body) ($ ref _ var) value)
     (let* ((jname (new-id "#jcont-"))
            (ov (new-id var #f))
            (nv (new-id var))
            (fk (new-id "#letcont/k-"))
            (jcont (new-lambda/k
                    (list nv)
                    (alpha-renaming (ast->cps body cont)
                                    (list var) (list nv))
                    #:kont cont)))
       (new-letcont/k jname jcont
                      (alpha-renaming (ast->cps value jname)
                                      (list (id-name ov))
                                      (list nv))
                      #:kont cont)))
    (($ branch ($ ast _ (cnd b1 b2)))
     (let* ((kname (new-id "#kcont-branch-"))
            (z (new-id "#cnd-"))
            (k1 (new-id "#letcont/k-branch-1-"))
            (k2 (new-id "#letcont/k-branch-2-"))
            (kont2
             (new-letcont/k k2
                            (new-lambda/k '() (comp-cps b2 cont)
                                          #:kont cont
                                          #:attr '((branch . #t)))
                            (new-branch/k z k1 k2)))
            (kont1
             (new-letcont/k k1
                            ;; NOTE: the cont will be reused for branch-2,
                            ;;       however, our CFS is mutable, we have to clone
                            ;;       the cont here to make sure it's safe. Otherwise
                            ;;       the outcome will be wrong definitely.
                            (new-lambda/k '() (comp-cps b1 (clone cont))
                                          #:kont cont
                                          #:attr '((branch . #t)))
                            kont2))
            (kont3
             (new-lambda/k (list z) kont1)))
       (comp-cps cnd kont3)))
    (($ assign ($ ast _ e) ($ ref _ var))
     (let* ((vid (new-id var #f))
            (ev (new-id "#assign-val-"))
            (k (new-lambda/k
                (list ev)
                (new-app/k cont (new-assign/k vid (comp-cps ev)))
                #:name (new-id "#assign-") #:kont cont)))
       (effect-var-register! (id-name vid))
       (comp-cps e k)))
    (($ collection ($ ast _ vals) type size)
     (let ((cname (new-id "#c-"))
           (ex (map (lambda (_) (new-id "#e-")) vals)))
       (fold (lambda (e x p)
               (ast->cps e (new-lambda/k (list x) p #:kont cont)))
             (new-letval/k cname
                           (new-collection/k cname type size ex
                                             #:kont cont)
                           (new-app/k cont cname #:kont cont))
             vals ex)))
    (($ seq ($ ast _ exprs))
     (let* ((r-exprs (reverse exprs))
            (expr-cps (map ast->cps (reverse (cdr r-exprs))))
            (tail (ast->cps (car r-exprs) cont))
            (el (filter-map (lambda (e) (and (not (is-def-in-cps? e)) e))
                            (append expr-cps (list tail))))
            (ev (map (lambda (_) (new-id "#k-")) el)))
       (fold (lambda (e v p)
               (if (is-def-in-cps? e)
                   p
                   (new-letcont/k v e p #:kont cont)))
             (new-seq/k ev #:kont cont)
             el ev)))
    (($ call _ f e)
     (let* ((fn (new-id "#f-"))
            (el (map (lambda (_) (new-id "#x-")) e))
            (is-prim? (and (ref? f) (is-op-a-primitive? (ref-var f))))
            (func (if (ref? f) (new-id (ref-var f) #f) f))
            (k (fold
                (lambda (ee ex p)
                  (comp-cps
                   ee
                   (new-lambda/k
                    (list ex) p
                    #:kont cont #:attr '((binding . #t)))))
                (cond
                 (is-prim?
                  (new-app/k cont
                             (new-app/k fn el #:kont cont
                                        #:attr '((binding-body . #t)))
                             #:kont cont))
                 (else
                  (new-app/k fn (append (list cont) el)
                             #:kont cont
                             #:attr '((binding-body . #t)))))
                e el)))
       (comp-cps (or is-prim? func) (new-lambda/k (list fn) k #:kont fn))))
    (($ ref _ sym)
     (cond
      ((is-op-a-primitive? sym)
       => (lambda (p)
            (new-app/k cont p #:kont cont)))
      ((symbol? sym)
       (new-app/k cont (new-id sym #f) #:kont cont))
      (else (throw 'laco-error 'ast->cps "BUG: ref should be symbol! `~a'" sym))))
    ((? id? id) (new-app/k cont id #:kont cont))
    ((? primitive? p) (new-app/k cont p #:kont cont))
    ((? is-def-in-cps?) expr)
    ((? constant? c)
     (let ((x (new-id "#const-"))
           (cst (new-constant/k c)))
       (new-letval/k x cst (new-app/k cont x #:kont cont)
                     #:kont cont)))
    (else (throw 'laco-error 'ast->cps "Wrong expr: " expr))))

(define* (cps->expr cpse)
  (define (seq-flatten ll)
    (fold-right (lambda (x p)
                  (if (and (list? x) (eq? 'begin (car x)))
                      (append (seq-flatten (cdr x)) p)
                      (cons x p)))
                '() ll))
  (match cpse
    (($ lambda/k _ args body)
     `(lambda (,@(map cps->expr args)) ,(cps->expr body)))
    (($ closure/k _ env body)
     `(closure (lambda (,@(map id-name (env->args env))) ,(cps->expr body))))
    (($ branch/k _ cnd b1 b2)
     `(if ,(cps->expr cnd) ,(cps->expr b1) ,(cps->expr b2)))
    (($ collection/k _ var type size value)
     `(collection ,type ,@(map cps->expr value)))
    (($ seq/k _ exprs)
     (seq-flatten `(begin ,@(map cps->expr exprs))))
    (($ letfun/k ($ bind-special-form/k _ fname fun body))
     `(letfun ((,(cps->expr fname) ,(cps->expr fun))) ,(cps->expr body)))
    (($ letcont/k ($ bind-special-form/k _ jname jcont body))
     `(letcont ((,(cps->expr jname) ,(cps->expr jcont))) ,(cps->expr body)))
    (($ letval/k ($ bind-special-form/k _  var value body))
     `(letval ((,(cps->expr var) ,(cps->expr value))) ,(cps->expr body)))
    (($ app/k _ f e)
     `(,(cps->expr f) ,@(map cps->expr e)))
    (($ assign/k _ v e)
     `(set! ,(cps->expr v) ,(cps->expr e)))
    (($ constant/k _ ($ constant _ val type)) val)
    (($ primitive _ name _ _ _) name)
    (($ lvar ($ id _ name _) offset)
     `(local ,name ,offset))
    (($ gvar ($ id _ name _))
     `(global ,name))
    (($ fvar ($ id _ name _) label offset)
     `(free ,name in ,label ,offset))
    (($ local ($ id _ name _) value)
     `(var ,name ,(cps->expr value)))
    ((? id? id) (id-name id))
    (else (throw 'laco-error 'cps->expr "Wrong cps: " cpse))))

(define (cps-show cexpr)
  `(,(list (cps->expr (cps-kont cexpr)) (cps-name cexpr) (cps-attr cexpr))
    ,(cps->expr cexpr)))

;; cps,id,prim -> symbol
(define (cps->name cexpr)
  (match cexpr
    ('global 'global)
    ((? cps? c) (id-name (cps-name c)))
    ((? id? id) (id-name id))
    ((? primitive? p) (primitive-name p))
    (else (throw 'laco-error cps->name "BUG: Invalid cps `~a'!" cexpr))))

(define (cps->name-string cexpr)
  (match cexpr
    ('global "global")
    ((? cps? c) (id->string (cps-name cexpr)))
    ((? id? id) (symbol->string (id-name id)))
    (($ primitive _ name _ _ _) (symbol->string name))
    (else (throw 'laco-error cps->name-string "BUG: Invalid cps `~a'!" cexpr))))

(define (kont-eq? k1 k2)
  (string=? (cps->name-string k1) (cps->name-string k2)))

(define (tag-proper-tail-recursion! expr)
  (cps-property-set! expr 'ptc #t))

(define (is-proper-tail-recursion? cexpr)
  (assoc-ref (cps-attr cexpr) 'ptc))

(define (is-tail-call? cexpr)
  (assoc-ref (cps-attr cexpr) 'tail-call))

(define (clean-tail-call! cexpr)
  (cps-attr-set! cexpr (assoc-remove! cexpr 'tail-call)))

(define (is-escaped? cexpr)
  (assoc-ref (cps-attr cexpr) 'escape))

(define (closure-was-named-let? cexpr)
  (assoc-ref (cps-attr cexpr) 'def))

(define (top-level->src)
  (top-level->body-list (lambda (v e) `(define ,v ,(cps->expr e)))))

(define (cps->expr/g cpse)
  `(module ,@(top-level->src) ,(cps->expr cpse)))

;; NOTE: Filter global var
(define (fix-fv fl)
  (fold-right (lambda (x p)
                (if (top-level-ref (id-name x))
                    p
                    (cons x p)))
              '() fl))
