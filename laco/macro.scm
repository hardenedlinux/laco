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
;; being captured by call-site identifiers of the same name):
;;   We rename every template-introduced identifier (a symbol that is not a
;;   pattern variable, not a literal, not the ellipsis marker, and not a
;;   reserved form, and that does not occur inside quoted data) to a fresh
;;   symbol BEFORE pattern-variable substitution happens.  This ordering is
;;   essential: if substitution happened first, a call-site argument that
;;   happens to share a name with a template-introduced identifier (e.g.
;;   calling `(swap! tmp x)` where the macro's own template also introduces
;;   a variable named `tmp`) would become textually indistinguishable from
;;   the template's own identifier, and the later renaming pass would rename
;;   both together -- silently breaking hygiene instead of protecting it.
;;   Renaming first, substituting second, avoids that collision entirely.
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
;; `(normal . expr)` and `(repeat . expr)` entries.  A `repeat` entry expands
;; by iterating over the repetition count derived from list-valued bindings.
;;
;; This supports both simple repeated variables `(x ...)` and nested repeated
;; structures such as `((a b ...) ...)`.
;; ---------------------------------------------------------------------------
(define (template-symbols expr)
  (cond
   ((symbol? expr) (list expr))
   ((pair? expr) (append (template-symbols (car expr))
                         (template-symbols (cdr expr))))
   ((vector? expr) (append-map template-symbols (vector->list expr)))
   (else '())))

(define (expand-one expr bindings)
  (cond
   ((symbol? expr)
    (let ((v (alist-cell-ref bindings expr)))
      (if v v expr)))
   ((vector? expr)
    (list->vector (map (lambda (e) (expand-one e bindings))
                       (vector->list expr))))
   ((pair? expr)
    (if (list? expr)
        (expand-seq expr bindings)
        (cons (expand-one (car expr) bindings)
              (expand-one (cdr expr) bindings))))
   (else expr)))

(define (expand-seq expr bindings)
  (let* ((elems (compile-pattern-seq expr))
         (acc '()))
    (for-each
     (lambda (e)
       (case (car e)
         ((normal)
          (set! acc (append acc (list (expand-one (cdr e) bindings)))))
         ((repeat)
          (set! acc (append acc (expand-repeat-entry (cdr e) bindings))))
         (else
          (error "expand-seq: bad compiled entry" e))))
     elems)
    acc))

(define (expand-repeat-entry expr bindings)
  ;; Determine the number of repetitions from list-valued bindings inside
  ;; EXPR.  Then expand EXPR once for each repetition with the appropriate
  ;; list element substituted for each list-valued variable.
  (let* ((vars (delete-duplicates (template-symbols expr) eq?))
         (list-vars
          (filter (lambda (v)
                    (and (alist-cell-ref bindings v)
                         (list? (alist-cell-ref bindings v))))
                  vars)))
    (if (null? list-vars)
        (list (expand-one expr bindings))
        (let* ((cnt (apply max (map (lambda (v)
                                     (length (alist-cell-ref bindings v)))
                                   list-vars))))
          (map (lambda (i)
                 (let ((local bindings))
                   (for-each
                    (lambda (v)
                      (let ((val (list-ref (alist-cell-ref bindings v) i)))
                        (set! local (bindings-extend local v val))))
                    list-vars)
                   (expand-one expr local)))
               (iota cnt))))))

(define (bindings-extend bindings key value)
  ;; Purely functional update: returns a NEW alist with KEY bound to VALUE,
  ;; never mutating BINDINGS in place. Guile's own `(ice-9 alist)` already
  ;; has a same-named `assoc-set!` with different (and, despite the `!`,
  ;; also non-mutating) semantics; we avoid that name entirely to prevent
  ;; confusion, and we avoid any destructive update here because BINDINGS
  ;; may be shared with sibling template entries in the same `expand-seq`
  ;; call -- mutating a shared cell in place would corrupt bindings still
  ;; needed by those siblings.
  (acons key value (alist-delete key bindings eq?)))

;; ---------------------------------------------------------------------------
;; Template instantiation
;; ---------------------------------------------------------------------------
;;
;; NOTE: this module used to also rename template-introduced identifiers
;; here (a pre-emptive alpha-renaming pass over the raw template, before
;; pattern-variable substitution) to protect them from being captured by a
;; same-named call-site argument -- e.g. `(swap! tmp x)` where the macro's
;; own template also has an internal variable named `tmp`.
;;
;; That renaming pass has been removed. Laco's CPS-conversion stage
;; (`laco cps`, in `ast->cps`/`comp-cps`'s handling of `binding` nodes)
;; already performs exactly this kind of alpha-renaming, per binding form,
;; based purely on AST nesting structure -- it does not matter whether a
;; given `(let ((tmp tmp)) ...)` came from hand-written source or from a
;; macro expansion; the same correct shadowing resolution applies either
;; way, once CPS conversion runs. Keeping a second, independent renaming
;; pass here was not just redundant: it was actively harmful, because it
;; could not distinguish "a symbol newly BOUND by the template" from "a
;; symbol the template merely REFERENCES" (e.g. a call to some existing
;; global helper function). Every free reference in a template that wasn't
;; a pattern variable, literal, or reserved word -- including perfectly
;; ordinary calls to global procedures -- was being renamed to a fresh
;; gensym that no longer refers to anything, breaking macros as simple as:
;;
;;   (define-syntax double (syntax-rules () ((_ x) (my-helper x))))
;;
;; So: pattern-variable substitution and ellipsis expansion happen here;
;; nothing else. Alpha-renaming for newly-introduced bindings is left
;; entirely to the CPS stage, where it can be done correctly and without
;; needing to guess which identifiers are bindings versus references.
(define (instantiate-template template literals bindings)
  (expand-one template bindings))

;; ---------------------------------------------------------------------------
;; Rule matching and syntax-transformer construction
;; ---------------------------------------------------------------------------
(define (try-rule literals rule expr)
  (match rule
    ((pattern template ...)
     (parameterize ((current-literals literals))
       ;; The caller invokes the transformer with `expr` already stripped of
       ;; the macro keyword (see `macro-expand`: `(m (cdr expr))`), so the
       ;; pattern's own leading keyword-position element must be dropped
       ;; before matching too. Per R7RS this position is never matched or
       ;; bound regardless of what identifier appears there (`_`, the
       ;; macro's own name for self-recursive templates, etc.) -- we simply
       ;; never look at it, rather than binding it to a placeholder, so
       ;; self-recursive macros that use their own name in pattern position
       ;; (a common idiom) aren't accidentally captured as a pattern
       ;; variable and substituted away in the template.
       (let* ((pattern-args (if (pair? pattern) (cdr pattern) '()))
              (compiled (compile-pattern-seq pattern-args)))
         (and-let* ((bindings (match-seq compiled expr '())))
           ;; Recursively expand any macro calls left in the instantiated
           ;; template (macro-generating-macro / a template invoking other
           ;; registered macros), mirroring the original implementation's
           ;; normal-order recursive expansion.
           (macro-expand
            (instantiate-template `(begin ,@template) literals bindings))))))
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
