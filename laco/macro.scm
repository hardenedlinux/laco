;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2022-2026
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
  #:use-module (srfi srfi-2) ; for and-let*
  #:use-module (srfi srfi-11) ; for let-values
  #:export (macro-register!
            search-macro-def
            parse-macro-spec
            current-macro-context))

;;
;; R7RS-small syntax-rules implementation.
;;
;; The expander works directly on s-expressions.  Pattern matching is done
;; using a per-rule immutable association list, so attempts on different rules
;; can never leak bindings.  Ellipses are compiled into a small backtracking
;; matcher; vectors, dotted pairs, `_`, custom ellipsis identifiers, and
;; nested ellipses are all supported.  `literals` are matched literally
;; (via `current-literals`), not bound as pattern variables.
;;
;; Hygiene, direction 1 (protecting template-introduced identifiers from
;; being captured by call-site identifiers of the same name, e.g. calling
;; `(swap! tmp x)` where the macro's own template also has an internal
;; variable named `tmp`):
;;   NOT handled by renaming anything here. Pattern-variable substitution
;;   and ellipsis expansion are all this module does (see
;;   `instantiate-template` below); a template-introduced binding such as
;;   `(let ((tmp tmp)) ...)` is emitted as plain, unrenamed source text,
;;   with whatever name the template author wrote. Protection from
;;   call-site capture is left entirely to Laco's CPS-conversion stage
;;   (`(laco cps)`, in `ast->cps'/`comp-cps'`'s handling of `binding'
;;   nodes), which alpha-renames each binding based on actual AST nesting
;;   structure once it can see which occurrences are the binding and which
;;   are references -- it doesn't matter whether a given
;;   `(let ((tmp tmp)) ...)` came from hand-written source or from a macro
;;   expansion, the same correct shadowing resolution applies either way.
;;   An earlier version of this module tried to protect against capture
;;   here instead, by renaming every template-introduced identifier before
;;   substitution. That was removed: working on raw s-expressions, it had
;;   no way to distinguish "a symbol newly BOUND by the template" from "a
;;   symbol the template merely REFERENCES" (e.g. a call to some existing
;;   global helper function), so it renamed both alike -- breaking macros
;;   as simple as
;;     (define-syntax double (syntax-rules () ((_ x) (my-helper x))))
;;   by renaming `my-helper` to a fresh gensym that no longer refers to
;;   anything. The CPS stage doesn't have this problem, since it operates
;;   on the binding structure itself rather than guessing from raw text.
;;
;; Hygiene, direction 2 (referential transparency of the template's free
;; identifiers, e.g. `let`/`if`/helper procedures used in the template
;; continuing to refer to their definition-site bindings even if the
;; call-site environment happens to rebind the same name):
;;   NOT implemented via a full mark+wrap syntax-object engine.  Reserved
;;   core forms (`(laco reserved)`) are left untouched by design (they must
;;   resolve to the core language, not to some renamed local), and ordinary
;;   free identifiers referenced in a template are otherwise passed through
;;   unchanged and rely on the later CPS/alpha-conversion pass for full
;;   lexical correctness.  This is a known, intentional simplification, not
;;   an oversight -- a complete fix requires syntax objects carrying marks,
;;   which is a substantially larger undertaking (see prior design
;;   discussion). If you need it, treat it as a separate follow-up.
;;
;; Nested-ellipsis repetition count is inferred at *expansion time* by
;; checking whether a bound value happens to be a list, rather than by
;; statically tracking each pattern variable's ellipsis depth from the
;; pattern.  This works for the common cases but can misfire if a
;; non-repeated pattern variable happens to be bound to a literal
;; list-shaped datum and is used inside the same repeated template
;; fragment as a genuinely repeated variable.  Flagged here rather than
;; fixed, since fixing it properly means switching to static depth
;; tracking, which is a larger change than the scope of this pass.
;;

;; Global macro table.  Local macros (let-syntax/letrec-syntax) are visible
;; only through current-local-macros, defined further below.
(define *macro-definition* (make-hash-table))
(define (macro-register! name mexpr)
  (hash-set! *macro-definition* name mexpr))
(define (search-macro-def name)
  (or (let ((locals (current-local-macros)))
        (and locals
             (let ((cell (assq name locals)))
               (and cell (cdr cell)))))
      (hash-ref *macro-definition* name)))

(define current-ellipsis (make-parameter '...))

;; The `literals` list of the rule currently being matched.  Consulted by
;; `match-one-pattern` so that literal auxiliary keywords (e.g. `else`,
;; `=>`) are required to match literally instead of being treated as
;; pattern variables that bind to anything.
(define current-literals (make-parameter '()))

;; NOTE: `current-macro-context` is part of this module's public export
;; surface and predates this rewrite -- other parts of the compiler
;; (parser/env) may already parameterize or read it for their own purposes
;; with a shape we don't control (e.g. simply the name of the macro
;; currently being expanded, for diagnostics). We deliberately do NOT read
;; or write it here, to avoid a shape collision. Local-macro visibility for
;; `let-syntax`/`letrec-syntax` uses its own private parameter instead.
(define current-macro-context (make-parameter #f))

;; Private: alist of (name . transformer) for macros introduced by an
;; enclosing let-syntax/letrec-syntax, or #f when none are in scope.
(define current-local-macros (make-parameter #f))

(define (ellipsis? x)
  (eq? x (current-ellipsis)))

(define (strict-pair? x)
  (and (pair? x) (not (list? x))))

(define (alist-cell-ref alist key)
  (let ((cell (assq key alist)))
    (and cell (cdr cell))))

(define (binding-exists? var bindings)
  (assq var bindings))

(define (binding-value var bindings)
  (let ((cell (assq var bindings)))
    (and cell (cdr cell))))

(define (collect-pattern-depth pat)
  (define (record! sym depth table)
    (let ((old (assq sym table)))
      (if old
          (if (eqv? (cdr old) depth)
              table
              (error "syntax-rules: inconsistent ellipsis depth for" sym))
          (acons sym depth table))))
  (define (walk p depth table)
    (cond
     ((symbol? p)
      ;; `_' and literal keywords are not pattern variables, so they must
      ;; not be recorded in the depth table.  Otherwise a pattern containing
      ;; the same wildcard/literal at different ellipsis depths would be
      ;; rejected as “inconsistent ellipsis depth”.
      (if (or (eq? p '_) (memq p (current-literals)))
          table
          (record! p depth table)))
     ((vector? p)
      (fold (lambda (e t) (walk e depth t))
            table (vector->list p)))
     ((list? p)
      (let lp ((ps p) (pending #f) (tbl table))
        (cond
         ((null? ps)
          (if pending (walk pending depth tbl) tbl))
         ((not (pair? ps))
          (if pending
              (walk pending depth (walk ps depth tbl))
              (walk ps depth tbl)))
         (else
          (let ((e (car ps)))
            (cond
             ((ellipsis? e)
              (if pending
                  (lp (cdr ps) #f (walk pending (+ depth 1) tbl))
                  (error "syntax-rules: ellipsis must follow a pattern")))
             (else
              (let ((tbl2 (if pending (walk pending depth tbl) tbl)))
                (lp (cdr ps) e tbl2)))))))))
     ((pair? p)
      (walk (cdr p) depth (walk (car p) depth table)))
     (else table)))
  (walk pat 0 '()))

(define (validate-depth-table! bindings depth-table)
  ;; After pattern matching succeeds, verify that every pattern variable with
  ;; a statically-known ellipsis depth also has a matched value of the same
  ;; nested-list shape.  This turns shape mismatches into a clear macro
  ;; expansion error instead of a confusing error later in `expand-repeat-entry`.
  (for-each
   (lambda (binding)
     (let* ((var (car binding))
            (value (cdr binding))
            (depth-cell (assq var depth-table)))
       (when depth-cell
         (let ((depth (cdr depth-cell)))
           (when (> depth 0)
             (let check ((v value) (d depth))
               (cond
                ((zero? d) #t)
                ((not (list? v))
                 (error "syntax-rules: ellipsis depth mismatch for" var value))
                (else
                 (for-each (lambda (e) (check e (- d 1))) v)))))))))
   bindings))

;; ---------------------------------------------------------------------------
;; Pattern compilation
;;
;; A compiled pattern sequence is a list of entries.  Each entry is either
;;   (normal . pattern)
;; or
;;   (repeat . pattern)
;; The ellipsis identifier is removed and attached to the preceding pattern.
;; ---------------------------------------------------------------------------
(define (compile-pattern-seq pats)
  (let loop ((ps pats)
             (acc '()))
    (cond
     ((null? ps) (reverse acc))
     ((not (pair? ps))
      (error "compile-pattern-seq: improper pattern"))
     (else
      (let ((p (car ps)))
        (cond
         ((ellipsis? p)
          (if (and (pair? acc) (eq? (caar acc) 'normal))
              (let ((prev (cdar acc)))
                (loop (cdr ps)
                      (cons (cons 'repeat prev) (cdr acc))))
              (error "syntax-rules: ellipsis must follow a pattern")))
         (else
          (loop (cdr ps)
                (cons (cons 'normal p) acc)))))))))

;; ---------------------------------------------------------------------------
;; Pattern matching
;;
;; match-one-pattern returns #f or an alist of pattern-variable bindings.
;; A repeated variable is bound to a list of the values matched by that
;; repetition.  For nested repetitions this naturally produces nested lists.
;; ---------------------------------------------------------------------------
(define (match-one-pattern pat expr)
  (cond
   ((eq? pat '_) '())
   ((and (symbol? pat) (memq pat (current-literals)))
    ;; A literal identifier from the rule's `literals` list must appear
    ;; literally at this position (same symbol), and does not bind.
    (and (eq? pat expr) '()))
   ((symbol? pat) `((,pat . ,expr)))
   ((vector? pat)
    (and (vector? expr)
         (match-one-pattern (vector->list pat) (vector->list expr))))
   ((list? pat)
    (let ((elems (compile-pattern-seq pat)))
      (match-seq elems expr '())))
   ((strict-pair? pat)
    ;; Simple dotted pattern without ellipsis: (car . cdr)
    (if (and (strict-pair? expr)
             (not (list? (car pat)))
             (not (list? (cdr pat)))
             (not (member (current-ellipsis)
                          (cons (car pat) (list (cdr pat))))))
        (let ((car-m (match-one-pattern (car pat) (car expr)))
              (cdr-m (match-one-pattern (cdr pat) (cdr expr))))
          (and car-m cdr-m
               (merge-bindings car-m cdr-m)))
        #f))
   (else #f)))

(define (merge-bindings b1 b2)
  ;; Merge two binding alists.  Duplicate variables must have equal? values.
  (let loop ((b b1))
    (if (null? b)
        b2
        (let* ((cell (car b))
               (old (assq (car cell) b2)))
          (cond
           ((not old) (cons cell (loop (cdr b))))
           ((equal? (cdr old) (cdr cell)) (loop (cdr b)))
           (else #f))))))

(define (match-seq elems exprs env)
  (cond
   ((null? elems)
    (and (null? exprs) env))
   ((not (pair? exprs)) #f)
   (else
    (let* ((e (car elems))
           (kind (car e))
           (pat (cdr e)))
      (case kind
        ((normal)
         (if (null? exprs)
             #f
             (let ((m (match-one-pattern pat (car exprs))))
               (and m
                    (match-seq (cdr elems)
                               (cdr exprs)
                               (merge-bindings env m))))))
        ((repeat)
         ;; Backtrack from longest possible match to shortest.  This allows
         ;; fixed tail patterns after an ellipsis.
         (let try ((k (length exprs)))
           (if (< k 0)
               #f
               (let* ((matched (take exprs k))
                      (rest (drop exprs k))
                      (repeated
                       (let lp ((xs matched)
                                (acc '()))
                         (if (null? xs)
                             acc
                             (let ((r (match-one-pattern pat (car xs))))
                               (and r
                                    (lp (cdr xs)
                                        (append acc (list r)))))))))
                 (cond
                  ((and repeated
                        (and=>
                         (match-seq (cdr elems)
                                    rest
                                    (merge-bindings
                                     env
                                     (collect-repeat-bindings repeated)))
                         (lambda (result) result)))
                   => identity)
                  (else (try (- k 1))))))))
        (else #f))))))

(define (collect-repeat-bindings repeat-alists)
  ;; Each repeat-alist corresponds to one successful repetition.
  ;; Return one alist where each variable is bound to a list of the values
  ;; collected across those repetitions, preserving order.
  (define vars
    (delete-duplicates (append-map (lambda (alist) (map car alist))
                                   repeat-alists)
                       eq?))
  (define (val-for var)
    (map (lambda (alist) (alist-cell-ref alist var))
         repeat-alists))
  (map (lambda (v) (cons v (val-for v)))
       vars))

;; ---------------------------------------------------------------------------
;; Template expansion
;;
;; Template expressions are traversed recursively.  Ellipses in the template
;; are handled by compiling the template list exactly like a pattern list into
;; `(normal . expr)` and `(repeat . expr)` entries.
;;
;; Pattern-variable substitution wraps every substituted value in
;; `(%%laco-template-value value)`, so later hygiene can distinguish
;; call-site expressions from template-introduced syntax.
;; ---------------------------------------------------------------------------
(define (template-symbols expr)
  (cond
   ((symbol? expr) (list expr))
   ((pair? expr) (append (template-symbols (car expr))
                         (template-symbols (cdr expr))))
   ((vector? expr) (append-map template-symbols (vector->list expr)))
   (else '())))

(define *hygienic-prefix* (make-parameter "laco-hyg-"))

(define (new-hygienic-symbol orig)
  (gensym (string-append (*hygienic-prefix*)
                         (if (symbol? orig)
                             (symbol->string orig)
                             "s"))))

(define (rename-one orig env)
  (let ((cell (assq orig env)))
    (if cell
        (values (cdr cell) env)
        (let ((fresh (new-hygienic-symbol orig)))
          (values fresh (acons orig fresh env))))))

(define (rename-bindings originals env)
  (let loop ((olds originals) (e env) (news '()))
    (if (null? olds)
        (values (reverse news) e)
        (let-values (((fresh new-e) (rename-one (car olds) e)))
          (loop (cdr olds) new-e (cons fresh news))))))

(define (extract-lambda-binders formals)
  (cond
   ((symbol? formals) (list formals))
   ((pair? formals)
    (let lp ((x formals) (acc '()))
      (cond
       ((null? x) (reverse acc))
       ((symbol? x) (reverse (cons x acc))) ; dotted rest arg
       ((pair? x) (lp (cdr x) (cons (car x) acc)))
       (else (throw 'laco-error 'extract-lambda-binders
                    "malformed lambda formals" formals)))))
   (else (throw 'laco-error 'extract-lambda-binders
                "malformed lambda formals" formals))))

(define (extract-lambda*-binders formals)
  (let lp ((x formals) (acc '()))
    (cond
     ((null? x) (reverse acc))
     ((keyword? x) (lp '() acc))
     ((symbol? x) (append (reverse acc) (list x))) ; dotted rest arg
     ((pair? x)
      (let ((a (car x)))
        (cond
         ((keyword? a)
          (lp (cdr x) acc))
         ((symbol? a) (lp (cdr x) (cons a acc)))
         ((pair? a)
          (if (symbol? (car a))
              (lp (cdr x) (cons (car a) acc))
              (lp (cdr x) acc)))
         (else (lp (cdr x) acc)))))
     (else (throw 'laco-error 'extract-lambda*-binders
                  "malformed lambda* formals" formals)))))

(define (rename-lambda-formals formals ids fresh-ids)
  (define (lookup s)
    (if (symbol? s)
        (let ((idx (list-index (lambda (x) (eq? x s)) ids)))
          (if idx (list-ref fresh-ids idx) s))
        s))
  (cond
   ((symbol? formals) (lookup formals))
   ((pair? formals)
    (let ((a (car formals))
          (d (cdr formals)))
      (cons (if (pair? a)
                (rename-lambda-formals a ids fresh-ids)
                (lookup a))
            (cond
             ((null? d) '())
             ((pair? d) (rename-lambda-formals d ids fresh-ids))
             (else (lookup d))))))
   (else formals)))

(define (rename-lambda*-formals formals ids fresh-ids env)
  (define (fresh s)
    (if (symbol? s)
        (let ((idx (list-index (lambda (x) (eq? x s)) ids)))
          (if idx (list-ref fresh-ids idx) s))
        s))
  (define (transform-entry entry)
    (cond
     ((keyword? entry) entry)
     ((symbol? entry) (fresh entry))
     ((pair? entry)
      (let ((k (car entry)))
        (cond
         ((keyword? k) entry)
         ((symbol? k)
          (cons (fresh k)
                (map (lambda (e) (hygienic-transform e env))
                     (cdr entry))))
         (else entry))))
     (else entry)))
  (cond
   ((symbol? formals) (fresh formals))
   ((pair? formals)
    (let lp ((x formals) (ret '()))
      (cond
       ((null? x) (reverse ret))
       ((pair? x)
        (lp (cdr x) (cons (transform-entry (car x)) ret)))
       (else
        `(,@(reverse ret) . ,(transform-entry x))))))
   (else formals)))

(define (hygienic-wrap-symbol sym env)
  (cond
   ((assq sym env) => cdr)
   ((or (memq sym (current-literals))
        (is-reserved-symbol? sym)
        ;; These are parser-level special forms, not variables that should
        ;; ever be captured as call-site globals.  They may not all be in
        ;; `laco/reserved.scm', but they must remain visible to the parser
        ;; after macro expansion, so we exclude them here too.
        (memq sym '(cons list vector))
        (eq? sym (current-ellipsis))
        (eq? sym '%%toplevel-ref)
        (eq? sym '%%laco-template-value))
    sym)
   (else `(%%toplevel-ref ,sym))))

(define (hygienic-transform expr env)
  (cond
   ;; Values substituted from call-site expressions remain opaque.
   ((and (pair? expr)
         (eq? (car expr) '%%laco-template-value))
    expr)

   ;; Quoted data is not code; leave it unchanged.
   ((and (pair? expr)
         (memq (car expr) '(quote quasiquote unquote unquote-splicing)))
    expr)

   ;; Parser-level special forms whose operator must survive unchanged.
   ((and (pair? expr) (eq? (car expr) 'begin))
    `(begin ,@(map (lambda (e) (hygienic-transform e env)) (cdr expr))))

   ((and (pair? expr) (eq? (car expr) 'list))
    `(list ,@(map (lambda (e) (hygienic-transform e env)) (cdr expr))))

   ((and (pair? expr) (eq? (car expr) 'cons))
    (let ((args (cdr expr)))
      (if (= (length args) 2)
          `(cons ,(hygienic-transform (car args) env)
                 ,(hygienic-transform (cadr args) env))
          (throw 'laco-error 'hygienic-transform
                 "cons form must have exactly 2 arguments" expr))))

   ((and (pair? expr) (eq? (car expr) 'vector))
    `(vector ,@(map (lambda (e) (hygienic-transform e env)) (cdr expr))))

   ((and (pair? expr) (eq? (car expr) 'set!))
    (let* ((args (cdr expr))
           (target (and (pair? args) (car args)))
           (value (and (pair? args) (pair? (cdr args)) (cadr args))))
      (if (not (and target (pair? (cdr args)) (null? (cddr args))))
          (throw 'laco-error 'hygienic-transform
                 "set! form must have exactly 2 arguments" expr)
          ;; NOTE: target and value are resolved through the exact same
          ;; logic below -- previously the target branch unwrapped
          ;; `%%laco-template-value' down to the bare call-site symbol
          ;; (via `cadr'), while the value branch left it wrapped. That
          ;; asymmetry between two structurally symmetric positions of
          ;; the same form was the direct cause of the swap!/tmp-tmp
          ;; regression: it isn't self-evidently more correct to expose
          ;; the bare symbol in one position and keep it opaque in the
          ;; other, and doing so inconsistently corrupted which `tmp'a
          ;; given occurrence was actually referring to. Whatever the
          ;; correct handling of a `%%laco-template-value'-wrapped call-
          ;; site expression is, it must be identical for target and
          ;; value.
          (let* ((resolve
                  (lambda (e)
                    (cond
                     ((and (pair? e) (eq? (car e) '%%laco-template-value))
                      e)
                     ((symbol? e) (hygienic-wrap-symbol e env))
                     (else (hygienic-transform e env)))))
                 (new-target (resolve target))
                 (new-value (resolve value)))
            `(set! ,new-target ,new-value)))))

   ((and (pair? expr) (eq? (car expr) 'lambda))
    (let* ((rest (cdr expr))
           (formals (and (pair? rest) (car rest)))
           (body (and (pair? rest) (cdr rest))))
      (let ((ids (extract-lambda-binders formals)))
        (let-values (((fresh-ids new-env) (rename-bindings ids env)))
          `(lambda ,(rename-lambda-formals formals ids fresh-ids)
             ,@(map (lambda (e) (hygienic-transform e new-env)) body))))))

   ((and (pair? expr) (eq? (car expr) 'lambda*))
    (let* ((rest (cdr expr))
           (formals (and (pair? rest) (car rest)))
           (body (and (pair? rest) (cdr rest))))
      (let ((ids (extract-lambda*-binders formals)))
        (let-values (((fresh-ids new-env) (rename-bindings ids env)))
          `(lambda* ,(rename-lambda*-formals formals ids fresh-ids new-env)
             ,@(map (lambda (e) (hygienic-transform e new-env)) body))))))

   ((and (pair? expr) (eq? (car expr) 'let))
    (let* ((rest (cdr expr))
           (first (and (pair? rest) (car rest))))
      (if (and (symbol? first) (pair? (cdr rest)))
          ;; named let
          (let* ((name first)
                 (bindings (cadr rest))
                 (body (cddr rest))
                 (ks (map car bindings))
                 (vs (map cadr bindings)))
            (let ((new-vs (map (lambda (v) (hygienic-transform v env)) vs)))
              (let-values (((new-name new-env0) (rename-one name env)))
                (let-values (((new-ks new-env) (rename-bindings ks new-env0)))
                  `(let ,new-name ,(map list new-ks new-vs)
                     ,@(map (lambda (e) (hygienic-transform e new-env)) body))))))
          ;; plain let
          (let* ((bindings first)
                 (body (cdr rest))
                 (ks (map car bindings))
                 (vs (map cadr bindings)))
            (let ((new-vs (map (lambda (v) (hygienic-transform v env)) vs)))
              (let-values (((new-ks new-env) (rename-bindings ks env)))
                `(let ,(map list new-ks new-vs)
                   ,@(map (lambda (e) (hygienic-transform e new-env)) body))))))))

   ((and (pair? expr) (eq? (car expr) 'let*))
    (let* ((rest (cdr expr))
           (bindings (and (pair? rest) (car rest)))
           (body (and (pair? rest) (cdr rest)))
           (ks (map car bindings))
           (vs (map cadr bindings)))
      (let loop ((ks-left ks) (vs-left vs) (env-now env) (new-bindings '()))
        (if (null? ks-left)
            `(let* ,(reverse new-bindings)
               ,@(map (lambda (e) (hygienic-transform e env-now)) body))
            (let* ((k (car ks-left))
                   (v (car vs-left))
                   (new-v (hygienic-transform v env-now)))
              (let-values (((new-k new-env) (rename-one k env-now)))
                (loop (cdr ks-left)
                      (cdr vs-left)
                      new-env
                      (cons (list new-k new-v) new-bindings))))))))

   ((and (pair? expr) (eq? (car expr) 'letrec))
    (let* ((rest (cdr expr))
           (bindings (and (pair? rest) (car rest)))
           (body (and (pair? rest) (cdr rest)))
           (ks (map car bindings))
           (vs (map cadr bindings)))
      (let-values (((new-ks new-env) (rename-bindings ks env)))
        (let ((new-vs (map (lambda (v) (hygienic-transform v new-env)) vs)))
          `(letrec ,(map list new-ks new-vs)
             ,@(map (lambda (e) (hygienic-transform e new-env)) body))))))

   ((and (pair? expr) (eq? (car expr) 'letrec*))
    (let* ((rest (cdr expr))
           (bindings (and (pair? rest) (car rest)))
           (body (and (pair? rest) (cdr rest)))
           (ks (map car bindings))
           (vs (map cadr bindings)))
      (let-values (((new-ks new-env) (rename-bindings ks env)))
        (let ((new-vs (map (lambda (v) (hygienic-transform v new-env)) vs)))
          `(letrec* ,(map list new-ks new-vs)
             ,@(map (lambda (e) (hygienic-transform e new-env)) body))))))

   ((symbol? expr)
    (hygienic-wrap-symbol expr env))

   ((vector? expr)
    (list->vector (map (lambda (e) (hygienic-transform e env))
                       (vector->list expr))))

   ((pair? expr)
    (if (list? expr)
        (map (lambda (e) (hygienic-transform e env)) expr)
        (cons (hygienic-transform (car expr) env)
              (hygienic-transform (cdr expr) env))))

   (else expr)))

(define (expand-one expr bindings depth-table)
  (cond
   ((symbol? expr)
    (let ((cell (assq expr bindings)))
      (if cell
          `(%%laco-template-value ,(cdr cell))
          expr)))
   ((vector? expr)
    (list->vector (map (lambda (e) (expand-one e bindings depth-table))
                       (vector->list expr))))
   ((pair? expr)
    (if (list? expr)
        (expand-seq expr bindings depth-table)
        (cons (expand-one (car expr) bindings depth-table)
              (expand-one (cdr expr) bindings depth-table))))
   (else expr)))

(define (expand-seq expr bindings depth-table)
  (let* ((elems (compile-pattern-seq expr))
         (acc '()))
    (for-each
     (lambda (e)
       (case (car e)
         ((normal)
          (set! acc (append acc (list (expand-one (cdr e) bindings depth-table)))))
         ((repeat)
          (set! acc (append acc (expand-repeat-entry (cdr e) bindings depth-table))))
         (else
          (error "expand-seq: bad compiled entry" e))))
     elems)
    acc))

(define (expand-repeat-entry expr bindings depth-table)
  (let* ((vars (delete-duplicates (template-symbols expr) eq?))
         (list-vars
          (filter (lambda (v)
                    (and (binding-exists? v bindings)
                         (let ((depth-cell (assq v depth-table)))
                           (and depth-cell (> (cdr depth-cell) 0)))))
                  vars)))
    (if (null? list-vars)
        (list (expand-one expr bindings depth-table))
        (let* ((cnt (apply max
                           (map (lambda (v)
                                  (let ((lv (binding-value v bindings)))
                                    (if (list? lv)
                                        (length lv)
                                        (error "syntax-rules: ellipsis variable expected a list"
                                               v lv))))
                                list-vars))))
          (map (lambda (i)
                 (let loop ((local bindings) (vs list-vars))
                   (if (null? vs)
                       (expand-one expr local depth-table)
                       (let* ((v (car vs))
                              (lv (binding-value v bindings)))
                         (if (and lv (list? lv) (< i (length lv)))
                             (loop (bindings-extend local v (list-ref lv i))
                                   (cdr vs))
                             (error "syntax-rules: ellipsis repetition length mismatch"
                                    v i))))))
               (iota cnt))))))

(define (bindings-extend bindings key value)
  (acons key value (alist-delete key bindings eq?)))

;; ---------------------------------------------------------------------------
;; Template instantiation
;; ---------------------------------------------------------------------------
;;
;; Pattern-variable substitution and ellipsis expansion happen here; nothing
;; else. In particular this does NOT rename template-introduced identifiers
;; -- see "Hygiene, direction 1" in the file-level comment at the top of
;; this file for why, and why that responsibility belongs to the CPS stage
;; instead.
(define (instantiate-template template literals bindings depth-table)
  (let ((expanded (expand-one template bindings depth-table)))
    (hygienic-transform expanded '())))

;; ---------------------------------------------------------------------------
;; Rule matching and syntax-transformer construction
;; ---------------------------------------------------------------------------
(define (try-rule literals rule expr)
  (match rule
    ((pattern template ...)
     (parameterize ((current-literals literals))
       (let* ((pattern-args (if (pair? pattern) (cdr pattern) '()))
              (compiled (compile-pattern-seq pattern-args))
              (depth-table (collect-pattern-depth pattern-args)))
         (and-let* ((bindings (match-seq compiled expr '())))
           (validate-depth-table! bindings depth-table)
           (macro-expand
            (instantiate-template `(begin ,@template)
                                  literals bindings depth-table))))))
    (_ #f)))

(define (make-syntax-transformer literals rules)
  (lambda (expr)
    (let/ec return
      (for-each
       (lambda (rule)
         (let ((expanded (try-rule literals rule expr)))
           (when expanded (return expanded))))
       rules)
      (throw 'syntax-error
             "source expression failed to match any pattern in form "
             expr))))

;; ---------------------------------------------------------------------------
;; Local syntax forms: let-syntax and letrec-syntax
;; ---------------------------------------------------------------------------
(define (install-local-macros bindings)
  (map (lambda (b)
         (match b
           ((name spec)
            (cons name (parse-macro-spec spec (lambda (x) x))))
           (_ (error "syntax-rules: invalid local macro binding" b))))
       bindings))

(define (macro-expand-list lst)
  ;; Like (map macro-expand lst), but tolerant of improper (dotted) lists.
  (cond
   ((null? lst) '())
   ((pair? lst) (cons (macro-expand (car lst)) (macro-expand-list (cdr lst))))
   (else (macro-expand lst))))

(define (macro-expand expr)
  (cond
   ((symbol? expr) expr)
   ((not (pair? expr)) expr)
   ((null? expr) expr)
   ((eq? (car expr) 'quote) expr) ; quoted data is not code; don't descend
   ((eq? (car expr) '%%toplevel-ref) expr) ; parser/CPS marker
   ((eq? (car expr) '%%laco-template-value)
    (macro-expand (cadr expr)))
   (else
    (let ((head (car expr)))
      (if (symbol? head)
          (let ((m (search-macro-def head)))
            (if m
                (macro-expand (m (cdr expr)))
                (cons (macro-expand head)
                      (macro-expand-list (cdr expr)))))
          (cons (macro-expand head)
                (macro-expand-list (cdr expr))))))))

(define (make-let-syntax-expander recursive?)
  (lambda (rest)
    (match rest
      (((bindings ...) body ...)
       (let* ((local-macros (install-local-macros bindings))
              (expanded-body
               (parameterize ((current-local-macros local-macros))
                 (macro-expand `(begin ,@body)))))
         ;; `macro-expand` puts a begin wrapper around the body; remove it so
         ;; the body expressions are returned in place.
         (match expanded-body
           (('begin inner ...) `(begin ,@inner))
           (other other))))
      (_ (throw 'syntax-error "bad let-syntax/letrec-syntax form" rest)))))

(define let-syntax-expander
  (make-let-syntax-expander #f))
(define letrec-syntax-expander
  (make-let-syntax-expander #t))

(macro-register! 'let-syntax let-syntax-expander)
(macro-register! 'letrec-syntax letrec-syntax-expander)

;; ---------------------------------------------------------------------------
;; Public entry: parse a syntax-rules spec.
;; ---------------------------------------------------------------------------
(define (parse-macro-spec spec ast-converter)
  (match spec
    (('syntax-rules (literals ...) rules ...)
     (make-syntax-transformer literals rules))
    (('syntax-rules ellipsis (literals ...) rules ...)
     (parameterize ((current-ellipsis ellipsis))
       (make-syntax-transformer literals rules)))
    (else (ast-converter spec))))

;; end of macro.scm
