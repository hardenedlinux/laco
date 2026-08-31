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

## 12. Postmortem: `lambda-lifting-2` was never a GC bug at all

This is a follow-up session's postmortem, kept here because the failure
mode looked exactly like a GC problem for a long time and cost several
rounds of GC-flavored speculation before the real, unrelated cause
surfaced. Filed under the GC memo specifically as a warning against
pattern-matching "obg-only failure" to "GC bug" too quickly.

**Symptom:** `(display (map (lambda (x) (* x 2)) '(1 2 3)))` SEGV'd only
under obg, never under tiny gc, with `AddressSanitizer: SEGV on unknown
address 0xbebebec2` inside `list_printer`'s `SLIST_NEXT` walk, reading a
garbage `node` pointer. `lambda-lifting.scm` was involved only
incidentally — it lifts the argument lambda to a top-level `procedure`,
which routes the call through `map`'s `apply_proc` path, but this fact
turned out to be a total red herring for the actual bug.

**Wrong turn #1 — blamed `apply_proc`'s calling convention.** The initial
(and initially plausible) theory: `apply_proc` never set up `vm->local`/
`vm->closure`/`vm->attr.shadow` before jumping into the lifted procedure's
`tail-call`-mode entry (whose `SAVE_ENV()` is a deliberate no-op, see §
"Animula: calling conventions" in the architecture notes) — so the callee
would read/write through stale registers. Several variations were tried
(see the separate, now-obsolete `lambda-lifting-2-issue.md` for the full
blow-by-blow); each either left the crash unchanged or broke `raise-cont`.
**Fully reverted** — `apply_proc` needed zero changes.

**Wrong turn #2 — blamed GC-rooting in `map`.** The next theory: `map`
builds its input list (`lst`) and result list (`new_list`) purely in C
locals, and `build_active_root` (§3 above) only ever walks VM stack
frames, the top-level region, and globals — never C locals. So a `gc()`
triggered mid-loop by the per-element call would find both lists
unreachable and sweep them, and the `0xbebebe...` pattern was assumed to
be obg's pool-poison-on-free value. The fix parked both lists as real
Objects on the VM stack so `build_active_root` would see them. **This had
zero effect — byte-identical crash address, same call frame, before and
after.** That non-result was itself the important clue: a real GC-timing
bug would not reproduce at the *exact* same address run after run, since
nothing here is randomized; identical addresses across independent runs
point at a deterministic logic bug, not a reachability/timing one.
**Also fully reverted.**

**Actual root cause — `animula_new_list()` never initializes
`list.slh_first`.** `object.c`'s `animula_new_list()` is a bare
`CREATE_NEW_OBJ`-style allocation: it hands back memory from
`gc_pool_malloc`/`GC_MALLOC` and nothing else — no field is ever zeroed or
set. `List.list.slh_first` (the `SLIST_HEAD`'s head pointer) therefore
starts as **whatever bytes were already there**, not `NULL`. `map`'s
construction loop (`SLIST_INSERT_HEAD`/`SLIST_INSERT_AFTER`, standard BSD
queue-macro semantics) always makes a newly-inserted node inherit
whatever "next" value was already sitting at the insertion point, and
never overwrites that inherited value with `NULL` — so the uninitialized
garbage isn't ever discarded, only handed forward, node after node, until
it ends up sitting in the *actual final tail node's* `next` field. The
list looks fully linked and correctly ordered right up until the last
node, whose "end of list" marker is silent garbage instead of `NULL`.
Iterating/printing past that point dereferences the garbage as if it were
a valid `ListNode*`.

**Why this was obg-only:** tiny gc's `GC_MALLOC` is Boehm's `GC_malloc()`,
which **zeroes the memory it returns** — a well-known BDW-GC property.
That made `slh_first` come out as `0`/`NULL` purely by accident, so the
chain always happened to terminate correctly under tiny gc. obg's
`GC_MALLOC` fallback (used whenever `gc_pool_malloc` has nothing free) is
a raw, non-zeroing `os_malloc()`; under AddressSanitizer, that
freshly-returned-but-never-written memory is filled with ASan's default
`malloc_fill_byte` (`0xbe`) — which is exactly the `0xbebebe...` pattern
seen in the crash address. **Not a free-then-read pattern, not GC pool
poisoning on collection — an allocate-then-read-before-write pattern.**
This is a strictly stronger, more specific claim than §"GC backend
differences are amplifiers, not root causes" elsewhere in this memo: here
tiny gc doesn't just fail to *amplify* the bug, it actively (if
accidentally) *fixes* it, via an unrelated allocator property neither
backend's design ever made a contract of.

**The fix:** `animula_new_list()` now explicitly sets
`o->list.slh_first = NULL;` and `o->non_shared = 0;` right after
allocation, before returning — establishing the invariant once, at the
single constructor every caller (`map`, and any future primitive that
builds a fresh list via `NEW_INNER_OBJ(list)`) goes through, rather than
requiring every call site to remember to do it themselves. `vm.c` and
`vm.h` needed **no changes at all** and were reverted to their original
baseline; the entire fix is contained in `object.c`.

**Lessons for next time:**

- **An "only obg fails" symptom is not on its own evidence of a GC bug.**
  It is evidence that *something* differs between the two allocators'
  behavior — and "zeroes memory vs. doesn't" is just as plausible a
  difference as anything in the GC's reachability/collection logic
  proper. Check the allocator's raw-memory contract (zeroing, alignment,
  poisoning) before assuming the bug lives in collection/rooting logic.
- **An identical crash address across independent runs, with no
  intervening randomization, is itself diagnostic.** It argues *against*
  a GC-timing/reachability theory (which would predict some variation as
  allocation history shifts) and *for* a deterministic logic bug that
  fires the same way every time regardless of when or whether a
  collection cycle runs.
- **`0xbebebe...` was mis-attributed to "obg pool poisons freed slots on
  collection"** in earlier analysis of this same bug, without ever
  actually grepping `gc.c` for that byte pattern. It isn't written by
  `gc.c` at all — it's ASan's own `malloc_fill_byte` default for
  freshly-`malloc`'d, never-written memory. Always verify a "the tool
  wrote this poison value" claim by finding where it's actually written,
  rather than treating it as self-evidently a GC-collection artifact.
- **`CREATE_NEW_OBJ`-style constructors (`animula_new_pair`,
  `animula_new_vector`, `animula_new_list`, `animula_new_bytevector`,
  `animula_new_mut_bytevector`) allocate raw memory and initialize
  nothing.** `animula_new_list` needed a bespoke, non-macro constructor
  (mirroring how `make_closure` already has its own hand-written
  constructor) specifically because `List` has an invariant
  (`slh_first == NULL` for "empty") that the generic macro can't know
  about. Worth auditing whether `pair`/`vector`/`bytevector`/
  `mut_bytevector` have any similar "must start as X, not garbage"
  invariants hiding the same way — none are known today, but none were
  specifically checked for either.
