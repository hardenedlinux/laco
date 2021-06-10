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

(define-module (laco utils)
  #:use-module (laco records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 q)
  #:use-module (ice-9 iconv)
  #:use-module ((rnrs) #:select (make-bytevector
                                 bytevector-u16-set!))
  #:export (newsym
            new-label
            is-tmp-var?
            extract-ids
            extract-keys
            new-stack
            new-queue
            queue?
            stack?
            stack-slots
            queue-slots
            slot-index
            stack-pop!
            stack-push!
            stack-top
            stack-remove!
            stack-length
            stack->list
            queue-out!
            queue-in!
            queue-head
            queue-tail
            queue-remove!
            queue-empty?
            queue-length
            queue->list
            list->stack
            list->queue
            define-typed-record
            make-object-list-pred
            symbol-list?
            any?
            immediate?
            collection?
            atom?
            args-with-keys
            get-all-defs
            new-counter
            file-basename
            gen-outfile
            flatten
            drop-hash
            *tail-call*
            *tail-rec*
            *normal-call*
            mode->name
            name->mode
            const-useless-position
            tail-position
            normal-call-register!
            is-normal-call?
            label-in!
            label-out!
            label-back-index
            intern!
            intern-offset
            gen-intern-symbol-table
            effect-var-register!
            is-effect-var?
            is-captured-fv?
            register-captured-fv!
            ordered-frees-add!
            get-ordered-frees
            fvar->lvar-fixed-offset
            ordered-frees-fix!
            global-label-register!
            global->label
            global-index
            set-fv-in-globals!
            appears-in-globals
            remove-fvs!
            keyword->string
            pure-label!
            is-pure-label?
            after-rename
            renamed-register!
            renamed-update!
            renamed-keep!
            has-renamed?
            scoped-label!
            is-scoped-label?
            is-ptc?
            ptc-register!
            is-named-let?
            named-let-register!))

(define (newsym sym) (gensym (symbol->string sym)))
(define (new-label str) (symbol->string (gensym str)))

(define (is-tmp-var? sym)
  (or (string-contains (symbol->string sym) "local-tmp-")
      (string-contains (symbol->string sym) "global-tmp-")))

(define (extract-ids pattern)
  (define (symbol-list? x)
    (and (list? x)
         (every symbol? x)))
  (match pattern
    ((? symbol?) (list pattern)) ; (lambda args ...)
    ((? symbol-list?) pattern) ; (lambda (a b c) ...)
    (((? symbol? a) . b) `(,a ,@(extract-ids b))) ; (lambda (a b . c) ...)
    (((? keyword? a) rest ...) '())
    (() '()) ; (lambda () ...)
    (else (throw 'laco-error "lambda: parameter must be an identifier!" pattern))))

(define (extract-keys pattern)
  (let lp ((next pattern) (keys '()) (opts '()) (mode 'normal))
    (cond
     ((null? next) (values keys (reverse opts)))
     ((eq? (car next) #:key)
      (lp (cdr next) keys opts 'key))
     ((eq? (car next) #:optional)
      (lp (cdr next) keys opts 'opt))
     ((eq? mode 'key)
      (lp (cdr next) (cons (car next) keys) opts mode))
     ((eq? mode 'opt)
      (lp (cdr next) keys (cons (car next) opts) mode))
     (else (lp (cdr next) keys opts mode)))))

(define (%q-remove-with-key! q key)
  (assoc-remove! (car q) key)
  (sync-q! q))

(define queue? q?)
(define stack? q?)
(define new-stack make-q)
(define new-queue make-q)
(define stack-slots car)
(define queue-slots car)

(define (slot-index s/q pred)
  (let ((slots (car s/q)))
    (any (lambda (x i) (and (pred x) i)) slots (iota (length slots)))))

(define stack-pop! q-pop!)
(define stack-push! q-push!)
(define stack-top q-front)
(define stack-remove! %q-remove-with-key!)
(define stack-empty? q-empty?)
(define stack-length q-length)
(define (stack->list stk) (list-copy (stack-slots stk)))

(define queue-out! q-pop!)
(define queue-in! enq!)
(define queue-head q-front)
(define queue-tail q-rear)
(define queue-remove! %q-remove-with-key!)
(define queue-empty? q-empty?)
(define queue-length q-length)
(define (queue->list q) (list-copy (queue-slots q)))

(define* (list->stack lst #:optional (stk (new-stack))) ; NOTE: mak
  (for-each (lambda (x) (stack-push! stk x)) lst)
  stk)

(define* (list->queue lst #:optional (queue (new-queue)))
  (for-each (lambda (x) (queue-in! queue x)) lst)
  queue)

(define-syntax-rule (type-check tr o preds ...)
  (or (any (lambda (p) (p o)) (list preds ...))
      (throw 'laco-error
             (format #f "Wrong type in ~a-~a, `~a' expect ~{`~a'~^,~}"
                     'tr 'o o (list 'preds ...)))))

(define-syntax define-typed-record
  (syntax-rules (parent fields)
    ((_ tr (parent p))
     (define-record-type tr (parent p)))
    ((_ tr (parent p) (fields (f t t* ...) ...))
     (define-record-type tr
       (parent p)
       (fields (mutable f) ...)
       (protocol
        (lambda (new)
          (lambda (pf f ...)
            (type-check tr f t t* ...) ...
            ((apply new pf) f ...))))))
    ((_ tr (fields (f t t* ...) ...))
     (define-record-type tr
       (fields (mutable f) ...)
       (protocol
        (lambda (new)
          (lambda (f ...)
            (type-check tr f t t* ...) ...
            (new f ...))))))))

(define (make-object-list-pred lst check)
  (and (list? lst) (every check lst)))

(define (symbol-list? lst)
  (make-object-list-pred lst symbol?))

(define any? (const #t))

(define (laco-list? x) #f)
(define (laco-pair? x) #f)

(define (immediate? x)
  (or (number? x)
      (string? x)
      (symbol? x)
      (vector? x)
      (number? x)))

(define (collection? x)
  (or (laco-list? x)
      (laco-pair? x)))

(define (atom? x)
  (or (immediate? x)
      (collection? x)))

(define (args-with-keys args)
  (any keyword? args))

(define (get-all-defs exprs)
  (let lp ((e exprs) (ret '()))
    (match e
      (() (values '() (reverse ret)))
      ((('define _ ...) rest ...)
       (lp rest (cons (car e) ret)))
      (else (values e (reverse ret))))))

;; NOTE: Set step to 0 will get the current cnt
(define* (new-counter #:optional (init 0))
  (let ((cnt init))
    (lambda* (#:optional (step 1))
      (cond
       ((integer? step) (set! cnt (+ cnt step)) cnt)
       (else (throw 'laco-error new-counter "Invalid step: `~a`!" step))))))

(define (file-basename filename)
  (substring filename 0 (string-index-right filename #\.)))

(define* (gen-outfile filename #:optional (ext ".lef"))
  (string-append (file-basename filename) ext))

;; flatten tree-like list
(define (flatten . args)
  (define (extract e)
    (match e
      (((expr ...)) expr)
      (else e)))
  (let ((lst (extract args)))
    (if (> (length lst) 1)
        (fold-right (lambda (x p) (append (flatten x) p)) '() lst)
        (extract lst))))

(define (drop-hash label)
  (if (char=? #\# (string-ref label 0))
      (substring/shared label 1)
      label))

(define *tail-call* 0)
(define *tail-rec* 1)
(define *normal-call* 2)

(define (mode->name mode)
  (match mode
    (0 'tail-call)
    (1 'tail-rec)
    (2 'normal)
    (else (throw 'laco-error mode->name "Invalid mode `~a'!" mode))))

(define (name->mode name)
  (match name
    ('tail-call 0)
    ('tail-rec 1)
    ('normal 2)
    (else (throw 'laco-error name->mode "Invalid mode-name `~a'!" name))))

(define (const-useless-position exprs)
  (list-head exprs (1- (length exprs))))

(define (tail-position exprs)
  (list-tail exprs (1- (length exprs))))

(define *normal-call-table* (make-hash-table))
(define (normal-call-register! label)
  ;;(pk "normal-call-register" label)
  (hash-set! *normal-call-table* label #t))
;; symbol -> bool
(define (is-normal-call? label)
  ;;(pk "is-normal-call?" label)
  ;;(pk "call-table" (hash-map->list cons *normal-call-table*))
  (hash-ref *normal-call-table* (format #f "#~a" label)))

(define *label-queue* (new-queue))
(define (label-in! label)
  ;;(pk "label-in!" label)
  (stack-push! *label-queue* label))
(define (label-out!)
  ;;(pk "label-out!")
  (stack-pop! *label-queue*))

;; NOTE: after fv-lifting, there's no free-var in tail-call or tail-rec context,
;;       so we can use label to indicate the stack frame.
(define (label-back-index label)
  (let ((ll (queue-slots *label-queue*)))
    (cond
     ((list-index (lambda (x) (eq? x label)) ll) => identity)
     (else (throw 'laco-error label-back-index
                  "Invalid label `~a' for free var!" label)))))

(define *intern-table* (new-queue))
(define intern!
  (let ((count (new-counter)))
    (define (exists? sym)
      (any (lambda (s) (eq? (car s) sym)) (queue-slots *intern-table*)))
    (lambda (sym)
      (cond
       ((exists? sym) #t)
       (else
        (let ((offset (count 0))
              (size (1+ (string-length (symbol->string sym)))))
          (count size)
          (queue-in! *intern-table* (list sym offset size))))))))
(define (intern-offset sym)
  (car (assoc-ref (queue-slots *intern-table*) sym)))
(define (symbol-table-size)
  (fold (lambda (v p) (+ (caddr v) p)) 0 (queue-slots *intern-table*)))
(define (gen-intern-symbol-table)
  (let* ((cnt (queue-length *intern-table*))
         (size (symbol-table-size))
         (cnt-bv (make-bytevector 2))
         (size-bv (make-bytevector 2)))
    (when (>= cnt (expt 2 16))
      (throw 'laco-error gen-intern-symbol-table
             "The current laco only support 16K symbols! `~a'" cnt))
    (when (>= size (expt 2 16))
      (throw 'laco-error gen-intern-symbol-table
             "The current laco only support 16KB symbol-table size! `~a'" size))
    (bytevector-u16-set! cnt-bv 0 cnt 'big)
    (bytevector-u16-set! size-bv 0 size 'big)
    (flatten
     (list
      cnt-bv
      size-bv
      (map
       (lambda (p)
         (list (string->bytevector (symbol->string (car p)) "iso8859-1")
               #vu8(0)))
       (queue-slots *intern-table*))))))

(define *effect-vars* (make-hash-table))
(define (effect-var-register! v)
  (hash-set! *effect-vars* v #t))
(define (is-effect-var? v)
  (hash-ref *effect-vars* v))

(define *upper-level-fv* (make-hash-table))
(define* (is-captured-fv? v #:optional (default '()))
  (hash-ref *upper-level-fv* v default))
(define (register-captured-fv! v ccn)
  (hash-set! *upper-level-fv* v ccn))

(define *ordered-frees-table* (make-hash-table))
(define (ordered-frees-add! label item)
  (cond
   ((is-named-let? (car item)) #f)
   (else
    (let ((frees (hash-ref *ordered-frees-table* label (new-queue))))
      (when (not (member (car item) (map car (queue-slots frees))))
        (hash-set! *ordered-frees-table*
                   label
                   (queue-in! frees item)))))))
(define (get-ordered-frees label)
  (queue-slots (hash-ref *ordered-frees-table* label (new-queue))))
(define (fvar->lvar-fixed-offset name label pred)
  (let ((frees (hash-ref *ordered-frees-table* label (new-queue))))
    (or (list-index pred (queue-slots frees))
        (throw 'laco-error fvar->lvar-fixed-offset
               "Invalid fvar `~a' in label `~a' to get fixed-offset!"
               name label))))
(define (ordered-frees-fix! label pred fixed-offset)
  (let* ((frees (hash-ref *ordered-frees-table* label
                          (format #f
                                  "BUG: it's impossible `~a' has no frees here!"
                                  label)))
         (index (list-index pred (queue-slots frees)))
         (item (list-ref (queue-slots frees) index)))
    (list-set! (queue-slots frees) index
               `(,(car item) ,(cadr item) ,fixed-offset))))

(define *globals* (new-queue))
(define (global-label-register! k v) (queue-in! *globals* (cons k v)))
(define (global->label k) (assoc-ref (queue-slots *globals*) k))
(define (global-index k)
  (list-index (lambda (p) (eq? k (car p))) (queue-slots *globals*)))

(define *fv-in-globals* (make-hash-table))
(define (set-fv-in-globals! k)
  (hash-set! *fv-in-globals* k (1+ (hash-ref *fv-in-globals* k 0))))
(define (appears-in-globals k)
  (hash-ref *fv-in-globals* k))
(define (remove-fvs! fvs)
  (for-each (lambda (fv)
              (let ((cnt (hash-ref *fv-in-globals* fv)))
                (if (= cnt 1)
                    (hash-remove! *fv-in-globals* fv)
                    (hash-set! *fv-in-globals* fv (1- cnt)))))
            fvs))

(define (keyword->string k)
  (symbol->string (keyword->symbol k)))

;; NOTE: scoped label is the label that has environment, for example:
;;       (let () ...)
(define *scoped-labels* (make-hash-table))
(define (scoped-label! label) (hash-set! *scoped-labels* label #t))
(define (is-scoped-label? label) (hash-ref *scoped-labels* label))

;; NOTE: pure label is the label that has no environment, for example:
;;       (begin ...)
(define *pure-labels* (make-hash-table))
(define (pure-label! label)
  ;;(pk "pure-label!" label)
  (hash-set! *pure-labels* (string->symbol (drop-hash label)) #t))
(define (is-pure-label? label) (hash-ref *pure-labels* label))

(define *renamed-vars* (make-hash-table))
(define (after-rename sym) (hash-ref *renamed-vars* sym))
(define (has-renamed? sym)
  (let ((v (hash-ref *renamed-vars* sym)))
    (if (eq? v 'named-let)
        #f
        v)))
(define (renamed-register! sym) (hash-set! *renamed-vars* sym 'named-let))
(define (renamed-update! old new)
  (hash-set! *renamed-vars* old new))
(define (renamed-keep! old new)
  (when (eq? 'named-let (hash-ref *renamed-vars* old))
    ;; (pk "renamed-keep!" old new read)
    (renamed-update! old new)))

(define *ptc-table* (make-hash-table))
(define (is-ptc? name) (hash-ref *ptc-table* name))
(define (ptc-register! name) (hash-set! *ptc-table* name #t))

(define *named-let-table* (make-hash-table))
(define (is-named-let? sym)
  (hash-ref *named-let-table* sym))
(define (named-let-register! sym)
  (hash-set! *named-let-table* sym #t))
