# Known limitations and deliberate scope cuts

This document records design decisions made while bringing `laco`'s
`syntax-rules` macro system up to (most of) R7RS's hygiene requirements,
and a related correctness fix in `closure-conversion.scm` that the work
surfaced. These are not oversights — each was discussed and decided
against for a stated reason. Recorded here so the reasoning doesn't have
to be reconstructed from scratch next time it's relevant.

## Macro hygiene: implemented

- **Free identifiers in a template resolve against the definition site**,
  not wherever the expansion happens to land lexically (`laco/ast.scm`'s
  `toplevel-ref` node, `laco/parser.scm`'s `%%toplevel-ref` recognition,
  `laco/cps.scm`'s handling in `ast->cps`/`alpha-renaming`).
- **Template-introduced bindings are hygienically renamed** to fresh,
  per-expansion-unique symbols, including correctly-independent renaming
  across separate ellipsis repetitions (`laco/macro.scm`,
  `hygienic-transform` and friends).
- **`let-syntax`/`letrec-syntax` local macros** get the same free-identifier
  protection as top-level macros, *for identifiers that ultimately resolve
  to a toplevel/global binding* (see explicit scope cut below).
- **Ellipsis repetition depth is statically derived from the pattern**
  (`collect-pattern-depth`, `validate-depth-table!`), replacing the older
  runtime "does this look like a list" heuristic.

## Macro hygiene: explicitly out of scope, not attempted

- **No `syntax-case` / mark-and-wrap syntax-object system.** The current
  approach (raw s-expression rewriting with an explicit rename
  environment) is deliberately simpler and was judged sufficient. Revisit
  only if a concrete need for non-`syntax-rules` macros (`er-macro-
  transformer`-style, etc.) shows up.
- **`literals` in a `syntax-rules` pattern (e.g. `cond`'s `else`) are
  matched by symbol spelling, not by binding identity.** R7RS technically
  requires the latter. A call site that locally shadows a literal name
  (e.g. `(let ((else #f)) (cond (else 1)))`) will still be treated as
  matching the literal. Fixing this would require scope-tracking during
  macro expansion, before CPS conversion establishes real lexical scope —
  judged not worth the added complexity given how rarely user code
  actually shadows a `syntax-rules` literal.
- **`let-syntax`/`letrec-syntax` templates cannot close over local
  variables of the function they were defined in.** Only free identifiers
  that resolve to a toplevel/global binding are protected. Resolving
  against enclosing-function locals would need a "resolve as of this
  historical lexical environment" capability that the compiler doesn't
  have today. Not attempted.

## Related bug found and fixed during this work

**`closure-conversion.scm`'s top-level `letcont/k` elimination** could
unsoundly substitute a `let`-bound variable directly with a plain
variable reference (`arg`) even when `arg` itself gets mutated later in
the same body — turning a "snapshot the value once, at binding time"
into "read the live variable on every use." This is what broke a
hygienically-expanded `swap!` macro: the macro's own temporary got
substituted down to the caller's variable, and by the time the
substituted reference was read, an intervening `set!` had already
changed it.

Fixed with a local structural scan (`mutates-var?` in
`closure-conversion.scm`) that checks, by object identity, whether the
substitution candidate is the target of any `assign/k` later in the body
being spliced in.

### Why this used a fresh structural scan instead of the existing `*effect-vars*` registry

The existing `any-effect-var?`/`is-effect-var?` mechanism could not be
reused directly for this check. `*effect-vars*` is populated once, when a
source-level `set!` is first converted from AST to CPS, keyed by the
assigned variable's id-name *at that time*. `alpha-renaming` (in
`(laco cps)`) knows to carry that registration forward whenever *it*
renames a variable — but `closure-conversion.scm`'s own `cfs`-based
substitutions (introducing a freshly-created id in place of another, e.g.
a synthesized top-level temp) have no equivalent carry-forward step. A
variable that closure-conversion is about to make the target of a `set!`
can therefore report `any-effect-var?` => `#f` under its new identity,
even though it will, in fact, be mutated.

**This is a general gap in `cfs`, not something specific to this one call
site.** It was patched locally here (`mutates-var?`) because this was the
concrete site with a failing test. `cfs` is also used in `normalize.scm`
(admin-redex beta-reduction) and `elre.scm` (case-5 beta-reduction) —
neither has been audited for the same class of issue. If a similar
"substitution produced a wrong result after a mutation" bug shows up in
either of those passes, this is the first place to look. A more thorough
fix would teach `cfs` itself to carry forward effect-var registrations
during substitution, the way `alpha-renaming` already does — that was not
done here to keep this fix contained to the failing case.

## Operational notes

- **`laco/pass.scm` has a per-pass CPS dump** (`pretty-print` of
  `cps->expr` after every pass in `run-pass`) that was enabled during this
  investigation to trace exactly which pass corrupted a reference. Confirm
  this is disabled (or gated behind a debug flag) before/after merging —
  it should not be default, on-by-default output for ordinary builds.
- **`mutates-var?` walks the entire candidate body** on every top-level
  `letcont/k` elimination where the bound value is a plain variable
  reference. Not expected to matter at current program sizes, but if a
  compile-time performance regression ever shows up on large top-level
  scripts with many sequential `let`s, this is a reasonable first place to
  profile.
