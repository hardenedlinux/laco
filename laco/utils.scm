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

(define-module (laco utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (ice-9 iconv)
  #:use-module ((rnrs) #:select (make-bytevector
                                 define-record-type
                                 bytevector-u16-set!))
  #:export (newsym
            new-label
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
            is-effect-var?))

(define (newsym sym) (gensym (symbol->string sym)))
(define (new-label str) (symbol->string (gensym str)))

(define (extract-ids pattern)
  (define (sym-list? x)
    (and (list? x)
         (every symbol? x)))
  (match pattern
    ((? symbol?) (list pattern)) ; (lambda args ...)
    ((? sym-list?) pattern) ; (lambda (a b c) ...)
    (((? symbol? a) . b) `(,a ,@(extract-ids b))) ; (lambda (a b . c) ...)
    (() '()) ; (lambda () ...)
    (else (throw 'laco-error "lambda: parameter must be an identifier!" pattern))))

(define (extract-keys pattern)
  #f)

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
  (hash-set! *normal-call-table* label #t))
;; symbol -> bool
(define (is-normal-call? label)
  ;;(pk "call-table" (hash-map->list cons *normal-call-table*))
  (hash-ref *normal-call-table* (format #f "#~a" label)))

(define *label-queue* (new-queue))
(define (label-in! label)
  ;;(pk "label-in!" label)
  (queue-in! *label-queue* label))
(define (label-out!)
  ;;(pk "label-out!")
  (queue-out! *label-queue*))
;; NOTE: after fv-lifting, there's no free-var in tail-call or tail-rec context,
;;       so we can use label to indicate the stack frame.
(define (label-back-index label)
  (let ((ll (queue-slots *label-queue*)))
    ;;(pk "ll" ll)
    (cond
     ((member label ll)
      => length)
     (else 0))))

(define *intern-table* (make-hash-table))
(define intern!
  (let ((count (new-counter)))
    (lambda (sym)
      (let ((offset (count 0))
            (size (1+ (string-length (symbol->string sym)))))
        (hash-set! *intern-table* sym (cons offset size))))))
(define (intern-offset sym)
  (hash-ref *intern-table* sym))
(define (symbol-table-size)
  (hash-fold (lambda (_ v p) (+ (cdr v) p)) 0 *intern-table*))
(define (gen-intern-symbol-table)
  (let* ((cnt (hash-count (const #t) *intern-table*))
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
      (hash-map->list
       (lambda (sym _)
         (list (string->bytevector (symbol->string sym) "iso8859-1")
               #vu8(0)))
       *intern-table*)))))

(define *effect-vars* (make-hash-table))
(define (effect-var-register! v)
  (hash-set! *effect-vars* v #t))
(define (is-effect-var? v)
  (hash-ref *effect-vars* v))
