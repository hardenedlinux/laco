# Animula GC: Design Memo (working notes, not an ADR)

This is a living memo, not a frozen design doc. It records the reasoning we
worked out this session so a future session (human or Claude) doesn't have to
re-derive it from scratch. Treat every "current" statement as "current as of
this session" — parts of this may change as the obg backend keeps getting
debugged.

## 1. Why object-based, not mark-and-sweep

The original design goal is **low latency**, with **real-time GC** as a
long-term aspiration. Two backends exist:

- **tiny gc** (`USE_TINY_GC`): wraps a Boehm-style (BDW) conservative
  mark-and-sweep collector. Simple, currently believed correct, but a
  classic mark-and-sweep pays for its generality with less predictable
  pause behavior — not a great fit for the long-term real-time goal.
- **obg gc** (`USE_OBG_GC`, "object-based generational"): the custom
  backend this whole session was spent debugging. This is the one meant to
  eventually support real-time behavior, because:
  - Objects live in **fixed-size pools per type** (`pair_free_pool`,
    `list_free_pool`, `vector_free_pool`, `closure_free_pool`,
    `bytevector_free_pool`, `mut_bytevector_free_pool`, `obj_free_pool`).
    Allocation is pool-slot reuse, not general-purpose malloc bookkeeping —
    fast and fits the "most objects are the same handful of shapes"
    embedded workload.
  - Liveness is decided by **walking the VM's own call-frame chain and
    globals** (see §3) rather than a full heap scan — the reachable set is
    bounded by "what's on the stack + what's captured by live closures +
    what's in globals," not by total heap size.
  - No reference counting anywhere — refcounting was explicitly rejected
    early in this project's history because of the per-copy runtime
    overhead it would add on an embedded target, and because it doesn't
    handle cycles.

The explicit long-term direction (discussed but **not implemented** this
session) is **compiler-side liveness/escape analysis** doing something
RAII-like: the compiler proves a value's last use and emits a hint the
runtime can act on without any runtime tracing at all. This was raised
specifically in the context of `mut_string` (see §6) but is really the
general direction for anything that currently relies on GC tracing.

## 2. Two-layer object model

Every Scheme value is a small, stack-resident `Object` (`oattr attr` +
`void *value`, `__packed`). For "collection" types, `.value` points at a
separately heap-allocated **inner** struct:

| otype_t            | inner struct    | owns extra heap memory? |
|---------------------|-----------------|--------------------------|
| `pair`              | `Pair`          | no (just two `object_t`) |
| `list`              | `List`          | yes — internal `ListNode` chain |
| `vector`            | `Vector`        | yes — `object_t *vec` array |
| `bytevector`/`mut_bytevector` | `ByteVector`/`MutByteVector` | yes — `u8_t *vec` array |
| `closure_on_heap`   | `Closure`       | no extra alloc — `env[]` is a flexible array member baked into the same allocation |
| `mut_string`        | *(none — raw `char*` in `.value`)* | yes, but **not pool-tracked at all** (see §6) |

**Important, easy-to-get-wrong distinction** we kept re-learning the hard
way: the **outer** `Object` (the stack-resident container) is *not* the
same thing as the **inner** value that gets registered in a pool. Only the
inner value goes through `gc_inner_obj_book`/gets a `.attr.gc` generation
tag that pool-based collection cares about. A "simple" value that's
heap-boxed standalone (e.g. via `animula_new_object` with no inner type)
lives in `obj_free_pool` instead. A value sitting directly on the VM stack
(never separately heap-boxed) isn't pool-tracked at all — its liveness is
whatever the enclosing frame's liveness is.

## 3. Active-root construction (`build_active_root`)

This is the single most bug-prone piece of the whole backend, because it's
the thing responsible for deciding "is this reachable." Current state,
after this session's fixes:

1. Walk the call-frame chain via `fp`/`NEXT_FP()`, scanning each frame's
   locals (`active_root_insert_frame`) **and** — separately — any
   currently-*executing* closure's *live invocation* locals via
   `closure->local` (this is about an in-progress call's temporaries, not
   the closure's own permanent captured state).
2. **Fix this session:** after the frame-walk loop exits (which only
   iterates while `fp > 0`), also scan `[0, sp)` — the **top-level**
   region. `fp == 0` is the *base case* ("no enclosing call"), not "nothing
   to scan." Top-level `define`s live directly on the ordinary stack with
   no call prelude, and the old loop skipped this region unconditionally.
3. **Fix this session:** also scan `vm->globals` (size derived from
   `GLOBAL_REF(VM_GLOBALSEG_SIZE)`, a plain global set by
   `vm_load_lef`/`vm_init_globals`, no special embedded-placement magic —
   see `os.h`'s `GLOBAL_DEF`/`GLOBAL_REF`). Before this fix,
   *anything reachable only through a global binding* (e.g.
   `(define z (some-closure))`) was invisible to a real `gc()` cycle.
4. **Fix this session:** `active_root_inner_insert`'s `closure_on_heap` /
   `closure_on_stack` case now walks the closure's own `env[]` (its
   *permanent* captured-variable storage) regardless of how the closure was
   reached (global, frame, or nested inside a pair/list). It used to assume
   "closure frames are walked separately in `build_active_root`" — true
   only for a closure *currently mid-call*, not for one just sitting as an
   inert value.
5. Marking dispatch was unified: `active_root_insert` (for an outer
   `object_t`) now just delegates to `active_root_inner_insert` (for the
   inner value + `otype_t`) instead of maintaining two independently
   hand-written, easily-desynced copies of the same per-type switch.

**Still open / not attempted:** `vector` has real struct support now (see
§7) in the mark/free paths, but nobody has gone back to double check every
corner of the active-root walk against it end to end beyond what the
vector-support pass covered.

## 4. Generations, `PERMANENT_OBJ`, and the force-teardown escape hatch

- `FREE_OBJ` (0) / `GEN_1_OBJ` (1) / `GEN_2_OBJ` (2) / `PERMANENT_OBJ` (3).
- Aging is **not** primarily about reducing scan work (every `gc()` cycle
  still scans every pool, every time) — it's mostly an **eviction
  priority** for `hurt` (out-of-memory) collections: normally GEN_2 is
  protected even from a full collect; only a `hurt` collect sacrifices it
  too.
- `hurt` used to be a **compile-time-fixed macro** (`ANIMULA_GC_HURT`),
  which defeated the entire "protect GEN_2 normally, sacrifice it only
  when truly desperate" premise — every collect was equally "hurt" or
  never was. **Fixed this session**: `ODB_GC_MALLOC`'s retry loop now
  escalates — first retry after a failed `os_malloc` is a normal collect;
  if *still* out of memory after that, the next retry is a real hurt
  collect.
- `free_object`/`free_inner_object` each had their **own, independent**
  `if (PERMANENT_OBJ == gc) return;` guard, deaf to any caller's `force`
  flag. `collect_inner(force=true)` correctly bypassed *its own*
  permanent-check, called `free_inner_object`, which then immediately
  bailed on *its own* separate check — so a forced sweep would physically
  free the outer struct (via `release_all_free_objects`'s own,
  force-respecting check) without ever tearing down what the object owned
  internally (e.g. a `list_t`'s `ListNode` chain), orphaning it.
  **Fixed** via a single `static bool g_gc_force_teardown` flag, set only
  by the two functions below, that both guards now respect.

## 5. Three different "clean everything" entry points — do not confuse them

This distinction cost several rounds of confusion this session and is worth
writing down precisely:

| Function | When it runs | Respects reachability? | Overrides `PERMANENT_OBJ`? |
|---|---|---|---|
| `gc()` (normal cycle) | reactively (alloc failure / OLN exhaustion) or proactively (`gc_alloc_budget_exceeded`) | yes — real `build_active_root` | no |
| `gc_try_to_recycle()` | every time `vm->sp == 0` in `vm_run`'s loop (in practice: **rarely actually reached** for short scripts — confirmed by direct instrumentation) | **no** — `simple_collect`/`simple_collect_list` blindly mark everything non-permanent as dead, no active-root check at all | no (still respects `PERMANENT_OBJ` in the blunt "skip it" sense) |
| `gc_clean_cache()` | on `HALT`, when *not* `VM_INIT_GLOBALS` — this is the one that actually fires for most short test scripts | no (same blunt approach as above) | **yes, after this session's fix** — now sets `g_gc_force_teardown` around its work, matching `gc_teardown` |
| `gc_teardown()` | added this session; meant to be called exactly once, right before process exit (`animula_clean`, before `vm_clean`) | no — `clean_active_root()` first, active root deliberately empty | yes — sets `g_gc_force_teardown` |

**Known, explicitly-flagged design tension:** making `gc_clean_cache`
override `PERMANENT_OBJ` means every `HALT` effectively treats the ending
script/session as "nothing needs to survive." This is correct for the
current one-shot-script model (`animula_start` loads one LEF, runs it,
exits) and for `shell.c`'s `sload run`/`run_prog` (which either explicitly
resets the whole VM or reloads globals from scratch anyway). It would be
**wrong** if `run_shell`'s interactive mode ever needs "permanent" bindings
to survive across multiple `HALT`s within one live process — that
scenario doesn't currently exist in `shell.c`, but flag it if it's ever
added.

**Also still open:** `gc_try_to_recycle`'s complete lack of reachability
checking is a real, un-fixed correctness gap by itself (independent of the
`PERMANENT_OBJ` question) — it was mostly moot this session because the
function turns out to rarely fire in practice, but if something later makes
`vm->sp == 0` actually happen mid-script more often, this will need the
same kind of real fix `gc_teardown`/`gc_clean_cache` got.

## 6. `mut_string`: the one open, accepted-as-a-leak gap

`mut_string`'s `.value` is a bare `char*`, never wrapped in a struct with
its own `.attr`, and **never registered in any pool** (`gc_inner_obj_book`'s
switch has no `mut_string` case). `free_object`'s own `mut_string` case
already correctly frees it (`os_free((void*)obj->value)`) — the problem
is entirely about *whether anything ever calls `free_object` on the
Object that holds it*.

- `recycle_object` and `active_root_inner_insert` were **missing a
  `mut_string` case entirely** (would `PANIC` if ever reached) — this is
  now fixed, closing a real crash risk for `mut_string` values that are
  bound as locals and go out of scope via `RESTORE()`'s frame-teardown, or
  that get scanned during a real `gc()` cycle.
- The specific case that's **still an accepted, un-fixed leak**: a
  `mut_string` built as a throwaway argument directly consumed by a
  primitive (e.g. `(display (list->string ...))`) — `display`'s
  implementation (`call_prim`'s `object_print` case) pops the argument by
  value and never registers it anywhere pool-based.
- **We tried and explicitly reverted** freeing it right at that
  `display` call site. Reasoning: `mut_string` (like other collection
  types here) is heap-allocated and passed **by reference** — copying the
  `Object` struct only copies the `.value` pointer, so if the *same*
  string were ever bound to a variable and displayed twice, freeing it on
  the first `display` would use-after-free on the second. The fix would
  only be sound with real compiler-side proof that the reference being
  freed is the *last* one — which doesn't exist yet. **A leaked buffer
  is an acceptable interim cost; a correctness regression is not** — this
  was an explicit, deliberate decision, not an oversight.
- **Real fix, not yet started:** either (a) give `mut_string` the same
  two-layer treatment `bytevector` already has (wrap it in a struct with
  its own `.attr`, register it in a proper pool) — this needs auditing
  *every* place in `str.c`/`print.c` that currently assumes `.value` is
  directly a `char*`, or (b) wait for the compiler-side liveness analysis
  mentioned in §1 to make it safe to free at a known-last-use point.

## 7. `vector`: completed this session

`Vector` is `{ oattr attr; u16_t size; object_t *vec; }` (from `types.h`,
obtained late in the session — this was the actual blocker for a long
time). All five places that needed vector handling now mirror the
combination of `pair` (recurse into `object_t` elements) and `bytevector`
(owns a separately-allocated array that needs its own `os_free`):

- `free_object`: recurse `free_object` over each element, mark dead
  (deferred physical free to `vector_free_pool`'s own sweep).
- `free_inner_object`: `os_free(v->vec)`, mark dead.
- `recycle_object`: recurse `recycle_object` over elements, mark via
  `free_object_from_pool(&vector_free_pool, ...)`.
- `active_root_inner_insert`: recurse `active_root_insert` over elements,
  mark the container alive.
- `gc_recycle_current_frame`: already delegated to `recycle_object`, no
  change needed.
- `print.c`'s `vector_printer` was also fixed (it used to infinitely
  recurse, printing the whole vector instead of element `i`).

## 8. Proactive GC triggering

Before this session, `GC()` only ever ran **reactively** — when
`object_list_node_available() == 0` or an allocation had already failed.
A script that never happens to exhaust the OLN pool or hit real OOM would
never run a single collection cycle, no matter how much garbage piled up.
This was confirmed directly: a debug probe in `gc_try_to_recycle` never
fired across an entire short test run.

Added `gc_alloc_budget_exceeded()`: a simple allocation-attempt counter
(`GC_ALLOC_THRESHOLD`, default 256, overridable via `-D`), checked
alongside the existing `object_list_node_available()` check inside
`NEW_OBJ`/`NEW_INNER_OBJ`/`NEW_LIST_NODE` — **deliberately not** placed
inside `gc_obj_book`/`gc_inner_obj_book`, which run mid-construction on a
not-yet-fully-initialized, not-yet-stack-reachable object (see the
`0xDEADBEEF` sentinel pattern already used elsewhere in the codebase for
exactly this hazard). The counter resets at the top of `gc()` itself,
regardless of what triggered that particular collection.

**Everything above is compiled out entirely under `USE_TINY_GC`** — the
whole of `gc.c` is wrapped in one `#ifdef USE_OBG_GC`, and every call site
in `object.h`/`vm.c` goes through `gc.h`'s abstraction layer, which
supplies `do{}while(0)` no-ops (or constant-folded `false`s) for the tiny
backend. This was treated as a hard requirement throughout the session:
**tiny gc must see zero behavioral or binary-size difference from any of
this work.**

## 9. RB-tree active root (performance, not correctness)

The active-root membership check (`exist()`) used to be a hand-rolled O(n)
linear list walk (reusing `rbe_left` as an ad-hoc "next" pointer), despite
`RB_GENERATE_STATIC(ActiveRoot, ActiveRootNode, entry, active_root_compare)`
already being called — the tree was never actually usable because
`struct ActiveRoot` was only ever forward-declared, never given a body.
Fixed by adding `RB_HEAD (ActiveRoot, ActiveRootNode);` in `obg_gc.h` and
switching `insert()`/`exist()`/`clean_active_root()` to the real
`RB_INSERT`/`RB_FIND`/`RB_INIT` macros. `exist()` is now O(log n).

## 10. `vm_t` singleton binding

`obg_gc.h`'s macros (`ODB_GC()`/`GC_MALLOC`) need to reach the current
VM's `fp`/`sp`/`stack` from call sites (e.g. inside `object.c`'s
allocation functions) that have no `vm_t vm` parameter of their own — but
the whole codebase threads `vm_t` explicitly as a parameter, with no
global singleton, and every VM function's own parameter named `vm` would
shadow one anyway. Solution: `g_current_vm` + `gc_bind_vm(vm)`, both owned
entirely by `gc.c`/`obg_gc.h` — **deliberately not wired into `vm.c`**
(reaching a global VM pointer into a shared, backend-agnostic file just to
serve one GC backend's internals was explicitly rejected as the wrong
direction of coupling). The person wiring up a VM calls `gc_bind_vm(vm)`
once, right after `vm_init(vm)` — currently wired into `animula.c`'s
`animula_init`. No-op under tiny gc.

## 11. Non-goals / things explicitly *not* done this session

- No attempt to change the fundamentally reactive-then-proactive trigger
  model into anything more sophisticated (e.g. byte-count-based instead of
  allocation-count-based).
- No attempt at incremental/interruptible collection — every `gc()` cycle
  is still a single, uninterruptible pass. Real-time GC remains a stated
  long-term goal, not started.
- No attempt to give `mut_string` its own pool (see §6) — deliberately
  deferred pending either a struct-layout audit or compiler-side liveness
  analysis.
- `closure_on_stack` is **not implemented at the VM level**
  (`call_closure_on_stack` is a `PANIC` stub) — the person confirmed
  Animula only actually uses heap closures; any `(closure stack ...)`
  IR/bytecode gets lowered/converted to a heap closure somewhere in the
  compiler before it would ever reach the runtime. Worth remembering if a
  future crash trace ever mentions `closure_on_stack`.

## 12. A pattern worth naming: "invisible under tiny gc" is not the same as "correct"

Every single bug found and fixed this session — the RB-tree dead code, the
missing `vector`/`bytevector` cases, the double-free, the `non_shared`
sign confusion, the `vm->globals`/top-level frame scanning gaps, all of
it — was **completely invisible under `USE_TINY_GC`**, because `gc.c`'s
entire body compiles out under that backend (see §8). This is expected
and correct: tiny gc genuinely doesn't need any of this machinery.

What's more interesting, and was only confirmed at the very end of the
session: **the one remaining unresolved bug (`lambda-lifting-2`, see the
separate issue doc) *also* only reproduces under obg**, even though the
root cause identified (`apply_proc` never setting `vm->local`/`vm->closure`
before jumping into a lifted procedure) reads like a pure `vm.c` calling-
convention bug that should misbehave identically regardless of GC backend
— `vm->local` is just a VM register. This was flagged as an open question
in the issue doc rather than resolved: either the calling-convention gap
interacts specifically with obg's pool-based slot reuse to produce a
crash (vs. silently-wrong-but-non-crashing behavior under tiny gc's
different allocation pattern), or there's a second, still-undiscovered
obg-specific bug layered on top of the calling-convention one. Don't
assume "only happens under obg" automatically means "it's a `gc.c` bug"
— this session's evidence is that it can also mean "obg's memory layout
is what turns an unrelated latent bug into a visible crash."
