;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2022-2025
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

(define-module (laco macro)
  #:use-module (laco utils)
  #:use-module (laco types)
  #:use-module (laco ast)
  #:use-module (laco env)
  #:use-module (laco records)
  #:use-module (laco primitives)
  #:use-module (laco reserved)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11) ; for let-values
  #:export (macro-register!
            search-macro-def
            parse-macro-spec
            current-macro-context))

;; We implement a r7rs syntax-rules.
;; Here're some notes:
;; 1. No eval-when, we handle macros only in compile time.
;; 2. The hyginic works under alpha-renaming, which is done by the callback of CPS converter.
;; 3. syntax-case is a good thing to have, patches are welcome.
;; 4. Macro expanding works on AST level.

(define *macro-definition* (make-hash-table))
(define (macro-register! name mexpr) (hash-set! *macro-definition* name mexpr))
(define (search-macro-def name) (hash-ref *macro-definition* name))

(define current-ellipsis (make-parameter '...))

(define current-macro-context (make-parameter #f))

;; DFS traverse s-expr and replace old-ids with new-ids
;; NOTE: We expand macros before CPS conversion, so we can do a very simple
;;       alpha-renaming here. Because everything is still s-expr, no struct,
;;       no hashtable and record-type yet.
(define (simple-alpha-renaming expr old-ids new-ids)
  (define (replace-id id)
    (let/ec return
      (for-each (lambda (old new)
                  (if (eq? id old) (return new)))
                old-ids new-ids)
      id))
  (match expr
    ((? symbol? sym)
     (if (memq sym old-ids)
         (replace-id expr)
         ;; otherwise it's a free variable
         sym))
    ((? pair?) (cons (simple-alpha-renaming (car expr) old-ids new-ids)
                     (simple-alpha-renaming (cdr expr) old-ids new-ids)))
    ((? list?) (map (lambda (e) (simple-alpha-renaming e old-ids new-ids))
                    expr))
    (else expr)))

;; NOTE: The macros are normal order, so the redundant evaluation must be
;;       optimized.
;; NOTE: Check (current-macro-context) to decide if it's recursive expand.
(define (simple-beta-reduction template literals lookup)
  (define (substitute sym)
    (hash-ref lookup sym))
  (define (try-to-substitute sym)
    (cond
     ((memq sym literals) sym) ; skip keywords
     ((pk 'sub sym (substitute sym)) ; #f => not a local bound var
      => identity)
     (else
      ;; We will not check unbound variables here.
      ;; It'd defer unbound variable checking to the CPS converter.
      sym)))
  (define (cfs e)
    ;; capture free substitution
    (match e
      (('begin body ...)
       (map cfs body))
      (('quote ,sym) e)                 ; literal symbol
      ((? symbol? sym) (try-to-substitute sym))
      (((? search-macro-def mname) rest ...)
       ((search-macro-def mname) rest))
      (((? list? head) rest ...)
       `(,(map cfs head) ,@(map cfs rest)))
      ((? pair? p)
       (cons (cfs (car p)) (cfs (cdr p))))
      ((? list? lst)
       (map cfs lst))
      (else e)))
  (map cfs template))

(define (hygienize literals pattern lookup template)
  (define (dedup lst)
    (let ((ht (make-hash-table)))
      (for-each (lambda (x)
                  (hash-set! ht x #t))
                lst)
      (hash-map->list (lambda (x _) x) ht)))
  (define (strict-pair? p)
    (and (pair? p) (not (list? p))))
  (define (get-all-bound-vars)
    (let lp ((expr template) (vars '()))
      (pk 'texpr expr)
      (match expr
        (('begin body ...)
         (lp body vars))
        (() (dedup vars))
        (((? symbol? sym) rest ...)
         (if (or (memq sym literals) (is-reserved-symbol? sym))
             (lp rest vars)
             (lp rest (cons sym vars))))
        (((? strict-pair? p) rest ...)
         (let ((a (lp (car p) '()))
               (b (lp (cdr p) '())))
           (lp rest `(,@a ,@b ,@vars))))
        (else
         (let ((ret (lp (car expr) '())))
           (lp (cdr expr) `(,@ret ,@vars)))))))
  (let* ((vars (get-all-bound-vars))
         (valid-ids (filter (lambda (id)
                              (and (not (memq id literals))
                                   (not (is-op-a-primitive? id))
                                   (memq id vars)
                                   id))
                            vars))
         (new-ids (map newsym valid-ids)))
    (for-each (lambda (id new)
                ;; Rename the id to new, however, we don't have to remove
                ;; the old id record, because it won't appear later.
                (cond
                 ((hash-ref lookup id)
                  => (lambda (x)
                       (hash-set! lookup new x)))
                 (else #f)))
              valid-ids new-ids)
    (simple-alpha-renaming template valid-ids new-ids)))

(define (make-syntax-transformer literals rules)
  (define lookup (make-hash-table))
  (define has-vargs? (make-parameter #f))
  (define* (is-matched pattern expr #:optional (last #f))
    ;; match the pattern of r7rs macro
    (cond
     ((null? pattern)
      (if (null? expr)
          #t
          (if (has-vargs?)
              (begin
                (hash-set! lookup last (cdr pattern))
                #t)
              #f)))
     ((and (memq (car pattern) literals) (equal? (car pattern) (car expr)))
      ;; no need to set lookup, because it's a literal
      (is-matched (cdr pattern) (cdr expr)))
     ((and (eq? (car pattern) '.) (eq? (car expr) '.))
      ;; no need to set `.' to lookup
      (is-matched (cdr pattern) (cdr expr)))
     ((eq? (current-ellipsis) (car pattern))
      (is-matched (cdr pattern) (cdr expr) (car expr)))
     ((symbol? (car pattern))
      (hash-set! lookup (car pattern) (car expr))
      (is-matched (cdr pattern) (cdr expr)))
     ((and (list? (car pattern)) (list? (car expr)))
      (pk 'list-set pattern)
      (hash-set! lookup (car pattern) (car expr))
      (and (is-matched (car pattern) (car expr))
           (is-matched (cdr pattern) (cdr expr))))
     ((and (pair? (car pattern)) (pair? (car expr)))
      (hash-set! lookup (caar pattern) (caar expr))
      (hash-set! lookup (cdar pattern) (cdar expr))
      (and (is-matched (caar pattern) (caar expr))
           (is-matched (cdar pattern) (cdar expr))
           (is-matched (cdr pattern) (cdr expr))))
     (else #f)))
  (define (match-a-rule rule expr)
    ;; TODO: how to match a rule?
    (match rule
      (((_ pattern ...) template ...) ; simplest rule
       (=> failed)
       (cond
        ((is-matched pattern expr)
         (let ((safe-template (hygienize literals pattern lookup
                                         `(begin ,@template))))
           (pk 'lookup (hash-map->list cons lookup))
           (simple-beta-reduction safe-template literals lookup)))
        (else (failed))))
      (else #f)))
  (lambda (expr)
    (let lp ((next rules))
      (cond
       ((null? next)
        (throw 'syntax-error
               "source expression failed to match any pattern in form "
               expr))
       ((match-a-rule (car next) expr) => identity)
       (else (lp (cdr next)))))))

(define (parse-macro-spec spec ast-converter)
  (match spec
    (('syntax-rules (literals ...) rules ...)
     (make-syntax-transformer literals rules))
    (('syntax-rules ellipsis (literals ...) rules ...)
     (parameterize ((current-ellipsis ellipsis))
       (make-syntax-transformer literals rules)))
    (else (ast-converter spec))))
