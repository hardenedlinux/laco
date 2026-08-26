# Animula + laco: architecture notes (accumulated understanding)

Working notes on how the two projects fit together, for continuity across
sessions. This is descriptive ("here's what exists and how it behaves"),
not prescriptive — see `animula-gc-memo.md` for the GC-specific design
reasoning, and `lambda-lifting-2-issue.md` for the one open bug.

## The two projects, in one sentence each

- **Animula**: an embedded Scheme bytecode VM (derived from Marc Feeley's
  picobit), targeting Zephyr/Cortex-M and Linux, with an object-pool-based
  memory model aimed at low-latency (eventually real-time) GC.
- **laco**: a commercial Scheme-to-Animula-bytecode compiler, built on a
  CPS intermediate representation with flat closures (Appel's
  methodology), producing the `.lef` binaries Animula runs.

## Animula: object/type model

Every value is an `Object` (`oattr attr` + `void *value`, packed to
minimize footprint). `oattr` packs `type` (6 bits) and `gc` (2 bits) into
one byte. `otype_t` (`types.h`) is the tag enum — pair/list/vector/
closure/bytevector/mut_bytevector/mut_string/string/symbol/primitive/
procedure/imm_int/rational_pos/rational_neg/complex_exact/complex_inexact/
character/boolean/null_obj/none/keyword/continuation/arbi_int, plus
`unbooked = -1` as a sentinel.

Encoding is 32-bit-oriented by default (`object.h` has the full bit-layout
table), with a `ADDRESS_64` variant for 64-bit hosts. Rational numbers
split sign into the type tag itself (`rational_pos`/`rational_neg`)
specifically to avoid spending a bit on sign in the packed representation
— both fields inside the `Rational` union are plain unsigned magnitudes.

See `animula-gc-memo.md` §2 for the outer-Object-vs-inner-value
distinction, which matters a lot in practice.

## Animula: calling conventions

Three call *modes*, tracked in `vm->attr.mode`/`vm->attr.shadow`:

- **`NORMAL_CALL`**: pushes a full 5-field prelude (`pc` placeholder,
  `local`, `fp`, `attr`, `closure`) via `SAVE_ENV()`'s default branch,
  clears `vm->attr.shadow`, sets a new `vm->fp`. Later unwound by
  `RESTORE()`.
- **`TAIL_CALL`**: **does nothing at all** — no prelude pushed, `vm->fp`
  untouched. By design: a genuine tail call needs no new frame, since
  control never needs to return to the current one. Whoever emits a
  `tail-call`-mode `prelude` bytecode is asserting "the caller already
  arranged everything this callee needs to find its own arguments
  correctly, and nothing here needs to survive past this call."
- **`TAIL_REC`** (self-recursive tail call, reusing the same frame):
  sets `vm->attr.shadow = arity`, enabling `IS_SHADOW_FRAME()`/
  `COPY_SHADOW_FRAME()` to shift the new argument values down into the
  *existing* frame position on the next `PRELUDE`, avoiding stack growth
  across recursive iterations. **Not the same mechanism as `TAIL_CALL`**
  — conflating the two was the source of at least one debugging dead-end
  this session (see `lambda-lifting-2-issue.md`).

Important asymmetry confirmed this session: `SAVE_ENV()`'s `TAIL_CALL`
branch **never touches `vm->attr.shadow`** (only `NORMAL_CALL` clears it,
only `TAIL_REC` sets it) — a stale shadow value from an earlier,
unrelated `TAIL_REC` can persist across an intervening `TAIL_CALL` and
misfire `IS_SHADOW_FRAME()` later. This is a real, demonstrated hazard,
not just a theoretical one.

`PROC_CALL` (used by `CALL_PROCEDURE`, the *normal* bytecode-level way to
call a `procedure` object) is the canonical "how do I correctly enter a
callee" reference implementation: check `IS_SHADOW_FRAME`, clear
`vm->closure`, set `vm->local = vm->fp + FPS`, jump. `apply_proc` (the
C-level "synchronously call a procedure object and get its return value"
path, used by `map` and `with-exception-handler`) is a **separate, less
complete implementation of the same idea**, and currently does not fully
match this convention — see the open issue doc.

`vm->local` vs `vm->fp`: `local` exists specifically because "the prelude
would pre-execute before the actual call, so the local frame was hidden by
prelude" (verbatim comment in `types.h`'s `LambdaVM` struct) — i.e. `fp`
marks where the *prelude* lives, `local` marks where the callee's *own
arguments/locals* actually start, and the two are related but not
interchangeable, and not always simply `fp + FPS` (that formula is only
valid when a real `NORMAL_CALL`-style prelude was actually pushed
immediately before).

## Animula: object.h/object.c allocation path

`animula_new_object(type)` / `NEW_OBJ`/`NEW_INNER_OBJ` macros are the
entry points. `CREATE_NEW_OBJ` tries `gc_pool_malloc` (reuse a `FREE_OBJ`
slot from the relevant pool) first, falls back to `GC_MALLOC` (raw
allocation) if the pool has nothing free. Every allocation attempt also
now checks `gc_alloc_budget_exceeded()` (this session's addition) in
addition to the pre-existing `object_list_node_available() == 0` check —
see the GC memo §8.

`CREATE_RET_OBJ()` (`vm.h`) is a different thing — a **stack-resident**,
not-pool-tracked `Object` used as a scratch "return value container" that
primitives fill in and the caller copies out of. This is exactly the
shape that bit us with `mut_string` (GC memo §6): anything built directly
into one of these, whose `.value` needs its own heap cleanup, has no
natural pool-based lifecycle at all.

## Animula: bytecode ISA (`bytecode.h`)

Encodings from 8 to 32 bits depending on operand size needs
(`SINGLE`/`DOUBLE`/`TRIPLE`/`QUADRUPLE`/`SPECIAL`, matching `encode_type`
in `types.h`). Notable ones referenced this session: `PRELUDE`,
`CALL_LOCAL`/`CALL_LOCAL_HIGH`, `CLOSURE_ON_HEAP`/`CLOSURE_ON_STACK`
(only the heap variant is actually implemented at runtime —
`call_closure_on_stack` is a `PANIC` stub, confirmed not needed since
Animula doesn't use stack closures), `PRIMITIVE`/`PRIMITIVE_EXT`,
`HALT`. Primitive numbers of particular note: `0` = `return` (a genuine
no-op continuation), `14` = `restore` (triggers a real `RESTORE()`), `19`
= `map`, `45` = `with-exception-handler`, `47` =
`raise-continuable`/`scm_raise_continuable`.

**Some primitives are registered in `primitives.c` with a `NULL` function
pointer** (`with-exception-handler`, `raise-continuable`, and presumably
`raise`/`return`/`restore` too) — their real implementation lives as a
special-cased `switch` arm directly inside `call_prim` in `vm.c`, not via
the generic function-pointer dispatch. Don't assume `primitives.c` is a
complete list of behavior; always check `vm.c`'s `call_prim` for a
primitive number before assuming it's unimplemented.

## laco: pass pipeline (as reconstructed this session, likely incomplete)

Roughly: source → `normalize` → CPS conversion (`cps.scm` is the core
CPS data structure/accessor module, not itself a "pass" per se) →
a sequence of optimization/lowering passes → `lir` (a lower-level IR with
explicit `insr-prelude`/`insr-closure`-style records) → `codegen` (LIR →
sasm text, purely mechanical, makes no decisions) → `assembler`/`sasm`
(sasm → actual bytecode, `emit-prelude` etc.) → LEF binary.

Known passes (directory listing, not all inspected this session):
`args-extend`, `closure-conversion`, `closure-lifting`,
`const-propagation`, `delta-reduction`, `dce`, `effect-analysis`,
`escape-analysis`, `eta-cont`, `eta-func`, `fold-branch`, `fold-const`,
`func-inline`, `lambda-lifting`, `normalize`, `primitive-conversion`,
`tco`, `useless-constant`, `useless-cont`.

**Passes actually opened and understood this session:**

- **`tco.scm`**: tail-call/tail-recursion detection. Tags CPS nodes with
  `'tail-call` (consumed later, see below) or registers proper
  tail-recursion (`ptc-register!`/`tag-proper-tail-recursion!`, consumed
  via `is-proper-tail-recursion?`). Contains a case matching `(kont-eq?
  kont f)` — i.e. "the function being applied IS the enclosing
  continuation itself" (a return, not a real call) — whose tagging line
  was **found commented out** with a TODO about needing further
  conditions ("no any locals... in tail body"). We tried enabling it; it
  turned out this specific TCO tag isn't even the actual generator of the
  problematic code for the closure-1 bug (see below) — a red herring,
  though possibly still worth understanding/completing for its own sake
  later.
- **`closure-lifting.scm`**: this is where `closure-1`'s actual bug lived.
  Its `cl` function, when compiling an `app/k` node, has three cases:
  `func` is a literal primitive (skip, no frame needed), we're at
  top-level (skip), or **else** — treat it as "calling some procedure,
  materialize any closure-literal argument via a fresh `new-letcont/k`
  binding first" (this generates the `(prelude normal N)` + push +
  `call-local` sequence). The bug: **calling one's own continuation
  directly** (`func` is `kont` itself) fell into the generic `else`
  branch, since `(primitive? func)` is false for a variable reference to
  a continuation. **Fixed this session** by adding a `(kont-eq? kont
  func)` case, treated the same as the primitive case (no lifting
  needed — calling your own continuation is a return, not a call, so no
  new frame/materialization is needed either). This fix was verified
  correct and stable — it's the one clean win from the whole closure-1/
  tco/escape-analysis/closure-lifting investigation arc.
- **`escape-analysis.scm`**: marks whether a lambda "escapes" its birth
  scope (passed as a non-continuation argument, or returned) vs. staying
  purely local (called only within the scope it's defined in). Its
  `app/k` handling unconditionally treats `(car args)` as "the
  syn-kont, never escaping" — which is subtly wrong when `func` itself
  *is* the continuation (there's no separate syn-kont slot in that
  case, so the real argument gets miscategorized as if it were one).
  **Diagnosed but not fixed** — the `closure-lifting.scm` fix above made
  this moot for the specific failing test, but the underlying inaccuracy
  in this pass for "calling one's own continuation" is still there if
  something else ever depends on it being precise.
- **`lambda-lifting.scm`**: hoists closures with zero free variables
  (`no-free-var?`) to plain top-level `procedure`s, avoiding closure
  allocation entirely. Runs after `normalize` and `closure-conversion`.
  This is what turns `(lambda (x) (* x 2))` into a top-level procedure in
  the `lambda-lifting-2` test — see the separate issue doc; the actual bug
  there is runtime-side (`apply_proc`), not in this pass.
- **`cps.scm`**: not a pass, but the shared CPS node accessor library —
  `is-tail-call?`/`clean-tail-call!`/`is-proper-tail-recursion?`/
  `kont-eq?`/etc. all live here, exported for other passes to use.
- **`lir.scm`**: **this is the actual consumer of `'tail-call`** — its
  `cps->lir` function, converting an `app/k` node, checks
  `is-proper-tail-recursion?` then `is-tail-call?` and picks
  `*tail-rec*`/`*tail-call*`/`*normal-call*` accordingly for the emitted
  `insr-prelude`'s mode. Confirmed **already fully wired up and correct**
  — the tco.scm tag, if set, would be correctly honored all the way
  through `codegen.scm` (pure mechanical LIR→sasm translation) and
  `sasm.scm`'s `emit-prelude`. The gap was purely in *setting* the tag in
  the first place (or, as it turned out for closure-1, not needing that
  tag at all — the real fix was one step earlier, in
  `closure-lifting.scm`).

**Debugging technique that worked well for tracing "who actually reads
this property":** grep for the literal property symbol (e.g.
`'tail-call`) across the whole laco source tree — this finds *setters*
and *getters* both, but getters are usually wrapped in a named predicate
(`is-tail-call?`), so a second grep for *that name* finds the real
consumers, which the first grep (matching only the literal symbol) misses
since callers just say `(is-tail-call? expr)`, not `'tail-call` again.

## General observations about the codebase's history

- There is a **recurring pattern of half-finished, commented-out code**
  clearly left over from earlier, abandoned debugging sessions —
  `tco.scm`'s disabled tail-call tag, `closure-lifting.scm`'s disabled
  `assign/k` special case (with a matching `is-return?` predicate already
  defined and unused), the RB-tree active-root code that was fully
  generated but never actually usable due to one missing macro
  invocation, `ANIMULA_GC_HURT` being a compile-time constant instead of
  a runtime signal. None of these were malicious or even obviously wrong
  at a glance — each one required tracing the *consumer* of the disabled
  code to understand why it mattered, which is why this session leaned so
  heavily on "get real sasm/opt/lir output and read it literally" rather
  than reasoning about C/Scheme semantics in the abstract. **Reading
  actual compiler output repeatedly out-performed reasoning from source
  code alone** whenever the two disagreed.
- The obg GC backend was shelved in favor of `tiny_gc` (BDW-based) at some
  point in the project's history, specifically because of exactly this
  class of bug — this session was a resurrection effort. `tiny_gc` is
  believed correct and was explicitly kept out of scope for changes
  throughout.
- Test suite state at the end of this session: started at 17 failing
  tests (after enabling the obg backend for the first time in a long
  time), ended at 1 (`lambda-lifting-2`, see the separate issue doc).
