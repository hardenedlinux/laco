# Open issue: `lambda-lifting-2` — `apply_proc` calling convention mismatch

**Status at end of session: unresolved, `vm.c` fully reverted to baseline
(no changes present). Six fix attempts, each either failed to fix this test
or broke `raise-cont`. Do not re-attempt without reading this whole
document first — every "obviously safe" incremental fix tried here had a
non-obvious interaction with `with-exception-handler`.**

**Important, session-closing discovery: every single test in the suite —
including `lambda-lifting-2` — passes under `GC_BACKEND=tiny`.** Only
`GC_BACKEND=obg` exhibits this failure. This means the root cause below
(`apply_proc` never setting `vm->local`/`vm->closure` before jumping) is
either (a) not the whole story — something about *obg's pool-based memory
reuse specifically* is what turns "reads a stale `vm->local`" into an
observable crash, where tiny gc's different allocation strategy (BDW,
never reuses freed slots the same way, likely zero/consistently-patterned
memory) happens not to surface it, or (b) the calling-convention bug is
real and backend-independent, but only obg's specific memory layout turns
the resulting garbage read into something that crashes instead of merely
being silently wrong. Either way: **this needs to be re-investigated with
the tiny-vs-obg contrast in mind, not treated as a pure `vm.c` calling-
convention bug in isolation.** A useful next step for a future session:
reproduce the exact `0xbebebec2`-style garbage under obg and check whether
that address pattern corresponds to a *freed-and-reused pool slot*
specifically (i.e. `map`'s own stale `vm->local` happens to point at a
`pair_free_pool`/`obj_free_pool` slot that obg's pool reuse subsequently
overwrote) — if so, the fix might belong partly in how obg pool reuse
interacts with stale stack-register reads, not purely in `apply_proc`.

## Test

```scheme
(define (func)
  (display (map (lambda (x) (* x 2)) '(1 2 3))))
(func)
```
Expected: `(2 4 6)`. This currently crashes with a SEGV inside
`list_printer` reading `SLIST_NEXT(node, next)` on a garbage `node`
pointer (address pattern `0xbebebec2`, consistent with reading
uninitialized/never-linked memory) — i.e. the *list structure itself*
built by `map` is corrupted, not just individual element values.

## Root cause (confirmed, high confidence)

`(lambda (x) (* x 2))` has no free variables, so `lambda-lifting.scm`
lifts it to a plain top-level `procedure` object instead of leaving it as
a runtime-constructed closure. `map` (primitive 19, in `vm.c`) invokes it
via `apply_proc`, not through the normal bytecode-level call path
(`CALL_PROC`/`global-call`/`call-local`).

`apply_proc`'s original code:
```c
void apply_proc (vm_t vm, object_t proc, object_t ret)
{
  u16_t entry = proc->proc.entry;
  vm->pc = proc->proc.entry;          // <-- jumps directly, no setup at all

  while (VM_RUN == vm->state)
    {
      bytecode8_t bc = FETCH_NEXT_BYTECODE ();
      if (IS_PROC_END (bc))            // sees the trailing `restore`, but...
        break;                         // ...never actually dispatches it
      dispatch (vm, bc);
    }

  if (ret) { *ret = POP_OBJ (); ... }
  else { POP_OBJ (); }
}
```

Compare to the **real** calling convention used everywhere else
(`CALL_PROCEDURE` → `PROC_CALL` in `vm.h`):
```c
#define PROC_CALL(offset)       \
  do {                          \
    if (IS_SHADOW_FRAME ())     \
      COPY_SHADOW_FRAME ();     \
    vm->closure = NULL;         \
    vm->local = vm->fp + FPS;   \
    JUMP (offset);              \
  } while (0)
```

`apply_proc` sets **none** of `vm->closure`, `vm->local`, or the
shadow-frame handling before jumping. The lifted procedure's own compiled
body starts with `(prelude ... tail-call N)`, and `TAIL_CALL` mode's
`SAVE_ENV()` branch is a deliberate no-op (`vm->attr.mode = TAIL_CALL;
break;` — by design: a tail call needs no new frame, per the person's
confirmation this session). This means **nothing ever sets up
`vm->local`** for this call — `(local N)` inside the lifted procedure's
body reads whatever `vm->local` happened to be left at by `map`'s own
(unrelated) caller context. Garbage in, garbage out — but the garbage
compounds silently instead of crashing immediately, corrupting `map`'s own
list-building state across iterations.

## sasm evidence

```
(label kont-516) ; Proc `lifted-closure-517' begin
  (label kont-446)
   (prelude kont-440 tail-call 1)   ; no-op at runtime, per design
    (label kont-445)
     (local 1)                       ; reads garbage: vm->local was never set for this call
     (push-integer-object 2)
     (prim-call 4 #t)                ; *
    (call-local kont-440 0 #t)      ; call own continuation (kont-440 = local[0])
  (prim-call 14 #t)                  ; restore -- apply_proc's IS_PROC_END sees this, never dispatches it
```

## Why this is genuinely two different calling conventions colliding

Two very different callers use `apply_proc`, with incompatible
expectations of what it manages internally:

1. **`map`** (`vm.c`): pushes `(continuation, element)` — the
   continuation is the primitive `return` (a true no-op: `case ret: {
   break; }`). Calling it inside the lifted procedure's body does
   *nothing at all* to the stack, so the computed value just sits on top
   of stack, and `apply_proc`'s simple "see `restore`, break, pop the
   top" logic is sufficient **once `vm->local` is set correctly**.

2. **`with_exception_handler`** (primitive 45, `vm.c` — implementation
   inlined in `call_prim`, *not* in `primitives.c`; `primitives.c` only
   registers it with a `NULL` function pointer): manually does
   `SAVE_ENV_SIMPLE()` **itself**, before calling `apply_proc`, and passes
   `k = GEN_PRIM(restore)` — a **real** `restore`, not a no-op. When the
   thunk's body internally calls its own continuation, that's a genuine
   `RESTORE()` that pops the frame `SAVE_ENV_SIMPLE()` pushed — entirely
   independent of `apply_proc`'s own `IS_PROC_END` handling (this happens
   via ordinary `dispatch()` of a *mid-body* `call-local`, not the
   procedure's *trailing* `restore` bytecode that `apply_proc`'s loop
   specifically watches for). `with_exception_handler` then uses a
   `goto extent:` loop keyed on `vm->state == VM_EXCPT_CONT` to resume the
   thunk's execution (via a *second* `apply_proc` call with
   `thunk.proc.entry` rewritten to a saved `pc`) after the handler runs.

   Full relevant code (`vm.c`, `call_prim`, case `with_exception_handler`):
   ```c
   Object thunk = POP_OBJ ();
   Object handler = POP_OBJ ();
   Object result = CREATE_RET_OBJ ();
   Object k = GEN_PRIM (restore);
   reg_t local = 0;

   SAVE_ENV_SIMPLE ();
   local = vm->local;
   PUSH_OBJ (k);
   apply_proc (vm, &thunk, &result);

 extent:
   if (VM_EXCPT_CONT == vm->state)
     {
       reg_t sp = POP_REG ();
       reg_t pc = POP_REG ();
       SAVE_ENV_SIMPLE ();
       PUSH_OBJ (k);
       PUSH_OBJ (result);
       vm->state = VM_RUN;
       apply_proc (vm, &handler, &result);
       PUSH_OBJ (result);
       RESTORE_SIMPLE ();
       vm->sp = sp;
       vm->local = local;
       PUSH_OBJ (result);
       thunk.proc.entry = pc;
       apply_proc (vm, &thunk, &result);
       goto extent;
     }
   else if (VM_EXCPT == vm->state)
     { /* similar, non-continuable case */ }

   PUSH_OBJ (result);
   RESTORE_SIMPLE ();
   break;
   ```
   And, matching the other end of the "record where we got interrupted"
   protocol, at the tail of `apply_proc` itself:
   ```c
   if (VM_EXCPT_CONT == vm->state)
     {
       PUSH_REG (vm->pc);
       PUSH_REG (vm->sp);
     }
   ```

`with_exception_handler` is already doing careful, correct-as-far-as-we-
know manual state management of exactly the registers `apply_proc` might
be tempted to "helpfully" also save/restore or reset. Any change to
`apply_proc` that touches `vm->local`/`vm->closure`/`vm->attr.shadow`
beyond the one narrow thing `map`'s use case needs risks conflicting with
this — confirmed empirically, repeatedly, below.

## Attempts made this session, in order, and exactly why each failed

1. **Dispatch the trailing `restore` instead of skip-breaking before it**
   (let `IS_PROC_END`'s bytecode run through `dispatch()` normally, then
   break). *Broke `raise-cont`* immediately with `"Oops, no more
   bytecode!"` — because `apply_proc` bypasses `CALL_PROC` entirely, the
   `pc` the procedure's own `PRELUDE` pushed is just the `NORMAL_JUMP`
   placeholder constant (no real call ever paired with it to overwrite
   it). A real `RESTORE()` pops that placeholder into `vm->pc`, and the
   VM tries to execute from a nonsense address.

2. **Manually replicate `RESTORE()`'s pop sequence, but discard the
   popped `pc` instead of assigning it.** Built on a wrong diagnosis:
   assumed a real prelude had been pushed and needed popping. It hadn't —
   `TAIL_CALL` mode's prelude is a genuine no-op, nothing was ever pushed.
   Result: unchanged, identical `"Oops, no more bytecode!"` crash — this
   code path was never even reached, since (as later confirmed from the
   real sasm) the lifted procedure's entry prelude is `tail-call` mode,
   not `normal`.

3. **Set `vm->local = vm->sp - proc->proc.arity * sizeof (Object);`
   alone** (mirroring `call_closure_on_heap`'s non-shadow-frame
   convention). This is the *correct, necessary* piece — but alone it
   only got us back to the *original* SEGV signature (`0xbebebec2` in
   `list_printer`), meaning something else was still wrong.

4. **+ `vm->attr.shadow = 0;`** reasoning: a stale nonzero shadow count
   left over from an unrelated earlier `TAIL_REC` could make
   `IS_SHADOW_FRAME()` misfire for this call. Same crash persisted
   (`list_printer`, same address pattern) — inconclusive on its own, but
   not yet known to conflict with anything.

5. **+ save/restore `vm->local`/`vm->closure`/`vm->attr.shadow` around
   the whole call** (treat them as callee-saved registers, restore before
   returning to the caller). Reasoning: `map`'s own loop does `vm->sp =
   vm->local;` right after each `apply_proc` call, assuming `vm->local`
   is still *map's own* value — if `apply_proc` permanently leaves it
   pointing at the last-called procedure's locals, `map`'s own subsequent
   list-node bookkeeping reads/writes the wrong stack region. Plausible
   and still the leading theory for *why the list structure itself* (not
   just element values) gets corrupted. **But this broke `raise-cont`
   again**, differently — before this, `raise-cont` had been unaffected
   by attempts 3–4.

6. **Minimal fix: only `vm->closure = NULL;` and `vm->local = vm->sp -
   proc->proc.arity * sizeof (Object);`, nothing else** — no shadow
   reset, no save/restore wrapper. This is the narrowest version of the
   "necessary" fix. Result: `lambda-lifting-2` still fails, but
   differently and more mildly — `raise-cont` now reports a **12-byte
   LeakSanitizer leak** and **empty actual output**, not a crash or wrong
   value. This is progress (no corruption, no crash) but still a failure,
   and the underlying interaction with `with_exception_handler`'s nested
   `apply_proc` calls + `goto extent:` loop clearly still isn't fully
   understood. **This is where the session stopped.** `vm.c` was fully
   reverted after this attempt — no version of this fix is currently
   applied.

## What's still missing before trying again

- A byte-by-byte trace (or the author's direct explanation) of exactly
  what `with_exception_handler`'s `goto extent:` loop expects to find in
  `vm->local`/`vm->closure`/`vm->attr.shadow`/`vm->sp` at each of its
  three `apply_proc` call sites (initial thunk call, handler call,
  thunk-resumption call), and whether those expectations are compatible
  with `apply_proc` *also* independently touching those same registers
  for `map`'s sake.
- Confirmation of whether `map`'s calling convention (continuation =
  `return`, a no-op) and `with_exception_handler`'s (continuation =
  `restore`, a real one) can actually share one `apply_proc`
  implementation at all, or whether `apply_proc` needs a parameter (or a
  second entry point) distinguishing "throwaway synchronous call, no
  special register handling needed beyond arg setup" from "structured
  call where the caller manages the surrounding state itself."
- The current best-supported *narrow* fix (attempt 6: just `vm->local` +
  `vm->closure = NULL`) is probably still *part* of the right answer for
  `map`'s case specifically — it stopped the "no more bytecode" and
  "wrong parameter values" failure modes without regressing `raise-cont`
  into a crash. The remaining 12-byte leak + empty output in `raise-cont`
  suggests something *adjacent* still isn't right, not necessarily that
  this piece is wrong.

## Recommendation

**First thing to check next time, before anything else:** why does this
pass under `GC_BACKEND=tiny` but not `GC_BACKEND=obg`? If the calling-
convention gap (`vm->local`/`vm->closure` never set by `apply_proc`) were
the *whole* story, it should misbehave identically under both backends —
`vm->local` is a plain VM register, nothing to do with which GC backend is
compiled in. The fact that it only surfaces under obg strongly suggests
the actual trigger involves *obg's pool-based slot reuse* specifically
(see the note at the top of this document). Confirming or ruling this out
first could redirect the whole investigation somewhere more productive
than another round of `apply_proc` tweaking.

Given six rounds already spent on `apply_proc` itself (each requiring a
full recompile + full test-suite run), and that `raise-cont` (general
exception handling) is almost certainly higher-value than this one
`map`-plus-lifted-lambda edge case: **do not resume work on this without
first getting complete, authoritative answers to the open questions
above.** Trial-and-error on this specific function has a demonstrated
high cost of introducing new regressions in `raise-cont` for each
attempted improvement.
