# Constraints and limitations: the `Aff` foundation

This document is required reading before you commit to `rio-aff`
for anything ambitious. It tells you, in concrete terms, what
`rio-aff` is built on top of and what that choice gives up
relative to `rio-fiber` (which owns its runtime end-to-end).
The short version:

> `rio-aff` is a strict superset of `Effect.Aff`. Every
> primitive in `rio-aff` ultimately compiles to an `Aff` action.
> Anything `Aff` cannot do, `rio-aff` cannot do either.

That is a deliberate trade. It is also a hard upper bound. The
sections below spell out what the bound looks like in practice
and how `rio-fiber` lifts each ceiling by replacing the `Aff`
interpreter with a custom fiber runtime.

If you have used ZIO in Scala or Effect in TypeScript, you
will recognise most of these ceilings: those libraries hit them
too, but they solve them by shipping their own fiber runtime
rather than reusing a host effect type, which is the path
`rio-fiber` takes.

This is the cost analysis you need if you're choosing between
the two runtimes. The headline trade-offs are in the
[main README](../README.md); this document is the precise
version.

## Why ship `rio-aff` at all?

`rio` started as a prototype to answer a specific question: can
PureScript's row types and `Variant` carry the ZIO / Effect
three-channel design (typed requirements, typed errors, typed
success) cleanly enough to be usable in a real codebase? The
answer turned out to be yes, and most of the surface in this
repository (the `RIO` newtype, the `r` / `e` / `a` rows, the
service-injection vocabulary, the layer algebra, the typed-error
catch combinators, the cause tree, the schedule combinators)
exists to make that case.

Building the first iteration on `Effect.Aff` was the pragmatic
choice for that prototype. The reasoning was:

- **Keep the initial scope tractable.** Writing a full custom
  fiber runtime is a multi-month effort on its own, and on its
  own it does not prove anything about PureScript's type
  system. Reusing `Aff` removed an entire axis of work and let
  the project focus on the type-level machinery, which is the
  novel part.
- **Borrow battle-tested mechanics for free.** `Aff` has been
  in production use for years. Its continuation-based stack
  safety, its cooperative cancellation, its `bracket` /
  `finally` finaliser guarantees, and its `Promise` interop are
  already correct. Reimplementing any of those badly would have
  made the package worse than `Aff`, not better, and would have
  obscured whether the row-type design itself was viable.
- **Get to a usable surface quickly.** The goal was to ship a
  working proof of concept: services, typed errors, layers,
  concurrency, streams, STM, observability, all with a test
  story (`itRIO`, `newTestClock`) that exercises the same
  surface real users would.
- **Make the case for the design.** If the row-type approach
  looked compelling on top of `Aff`, that was evidence the
  typed-channel design was worth carrying further (including,
  eventually, on top of a custom runtime). If it did not look
  compelling, no amount of bespoke scheduling would save it.
  Starting with `Aff` made the design question separable from
  the runtime question.

That bet paid off: the row-type design holds up, and the
follow-on package [`rio-fiber`](../rio-fiber/) is the result of
then taking on the runtime question separately. `rio-fiber` is
now the recommended default. `rio-aff` remains in the workspace
as the ecosystem-friendly alternative: same vocabulary, same
three-channel design, but interpreted on top of `Aff` for
codebases that want to stay in the `Aff` ecosystem end-to-end.
The constraints documented below are the price of that choice.

## The core type, restated

```purescript
newtype RIO r e a = RIO (Op r e a)
```

`RIO` wraps an opaque `Op`: an operation-list ADT interpreted
by a hand-rolled step / resume machine in `RIO.Aff.Internal`. The
interpreter drives synchronous binds entirely in a JS while
loop and only crosses into `Aff` for true async work (FFI
that completes on a callback, `Aff.delay`, fiber joins, and
so on). At the edge, `unRIO m env :: Aff (Either (Variant e) a)`
reifies the final outcome back as an `Aff`, where typed
failures appear in the `Left` branch and successes in
`Right`. Three things follow immediately:

1. There *is* a separate `rio-aff` interpreter; the synchronous
   bind chain does not pay a per-bind `Aff` cost. But once any
   step suspends, the rest of the work rides on `Aff`'s
   scheduler. `runRIO` evaluates the program to a final `Aff`
   value at the edge and hands that to whatever runs `Aff`
   (`launchAff_`, `Aff.runAff`, etc.).
2. Typed errors are a userland encoding. The `Either (Variant e)`
   wrapper is not visible to `Aff`. `Aff`'s own error channel is
   a single untyped `Error` and is used by `rio-aff` only for
   defects (the `Die` arm of `Cause`).
3. Cancellation, async waits, and bracketing for the async
   portion of a program are `Aff`'s. `rio-aff` adds vocabulary
   (typed names, structured composition, `Scope`-based
   finalisers) but the underlying async mechanism is whatever
   `Aff` provides.

## What `Aff` gives `rio-aff` for free

These are good defaults and a large part of why building on
`Aff` is sensible at all:

- A continuation-based, stack-safe asynchronous effect type.
  Long monadic chains do not blow the JS stack.
- Cooperative cancellation. `Aff.killFiber` interrupts a running
  fiber at the next async boundary, and `Aff.bracket` /
  `Aff.finally` guarantee finaliser execution on success,
  failure, and cancellation.
- A fiber primitive. `Aff.forkAff` gives you a `Fiber` you can
  `joinFiber` or `killFiber`. `rio-aff`'s `fork`, `join`, and
  `interrupt` are thin typed wrappers over these.
- Synchronisation via `Effect.Aff.AVar` (single-slot blocking
  variable). `RIO.Aff.Deferred` and most of `RIO.Aff.STM`'s blocking
  operators land on `AVar`.
- Promise interop via `Aff.Promise`. Anything that returns a
  JS `Promise` can be lifted with one call.
- Single-threaded JS semantics. There is no need to worry about
  data races on shared mutable references inside a single tick.
  `rio-aff`'s STM implementation exploits this directly.

## What `Aff` cannot give `rio-aff`

Here is the meaningful list. Each item is a real ceiling, not a
nit. Read them as design constraints, not bugs.

### 1. Typed errors are wrapping, not a native channel

`rio-aff`'s typed-error story works by wrapping every result in
`Either (Variant e) a` inside the `Aff`. `Aff`'s own error
channel is reserved for defects (uncaught exceptions, the `Die`
case in `Cause`).

Concretely:

- Every primitive that wants to behave like a typed `RIO`
  primitive must thread the `Either` pattern by hand. `Aff`
  combinators like `Aff.attempt` only see the `Right (Either ...)`
  layer; they do not inspect the `Variant`.
- A native fiber runtime can short-circuit the success
  continuation as soon as a typed failure is raised. `rio-aff` has
  to compute `Left v`, return it from the `Aff`, and let the
  next monadic bind notice and propagate. That is one extra
  allocation per bind in the failure case.
- The PureScript compiler still gives you the full type-level
  guarantee that errors are tracked in `e`. The point is only
  that the *runtime* representation is reconstructed at every
  step, not first-class.

This is the single biggest reason a future custom runtime would
exist: a native channel halves the per-bind work in the failure
case and removes a class of "I forgot to thread the `Either`"
mistakes from new primitives.

### 2. The `Cause` tree is reconstructed, not native

ZIO's `Cause` is observed directly by the runtime: every fiber
death, every interrupt, every parallel join produces a `Cause`
node without any user instrumentation. `Effect.Cause` is the
same in Effect.

`rio-aff`'s `RIO.Aff.Cause` is a userland reification. `attemptCause`,
`parTraverseCause`, `raceCause`, and friends manually walk the
known failure shapes and produce a `Cause` tree. That works
because the failure modes are enumerable (typed failure via
`Variant`, defect via `Aff` exception, parallel branch via
`parTraverseN`, sequential bind via `bind`), but it has two
downsides:

- Anything that goes through raw `Aff` (`liftAff`, third-party
  `Aff` libraries, FFI promises) does not contribute structured
  `Cause` data. Its failures become `Die` leaves with whatever
  the JS exception is.
- The `Interrupt` leaf that ZIO produces on a fiber kill is not
  a separate constructor in `RIO.Aff.Cause`. Kills propagate as
  `Aff` cancellations through `bracket` finalisers; whether
  the caller can distinguish "killed" from "the bracketed
  computation returned" depends on how the finaliser was
  written. `rio-aff` is consistent about this for its own
  primitives, but a custom runtime could surface
  `Interrupted FiberId` as a first-class cause without that
  discipline.

### 3. Interruption is cooperative and untracked

`Aff` cancellation is cooperative: a running fiber checks for a
kill signal at the next async boundary. Synchronous loops do not
yield. There is also no first-class concept of "this region is
uninterruptible".

`rio-aff` exposes `uninterruptible` and pairs `acquireRelease` with
`Aff.bracket` so resources are always released, but:

- `uninterruptible` is implemented by acquiring a guard around
  the body. There is no nested restorable interrupt mask the
  way ZIO has (`ZIO.uninterruptibleMask`). If you need to
  switch interruptibility back on inside an uninterruptible
  region, you cannot.
- "Uninterruptible during acquire, interruptible during use,
  uninterruptible during release" works because
  `Aff.bracket` implements that pattern, but only because the
  three phases are written separately. Arbitrary mid-program
  interrupt masks are not exposed.
- A truly tight CPU-bound loop (no `Aff` step at all) cannot be
  cancelled at all until it yields. Long-running synchronous
  work needs explicit yields (`Aff.delay (Milliseconds 0.0)`
  or equivalent) to remain killable.

A custom runtime can publish interrupt status as an explicit
machine state with proper masking. `Aff` cannot.

### 4. Fibers have no identity or supervision

`Aff.Fiber` is opaque. It does not have a stable id, a printable
name, or an inspectable parent / child relationship. You cannot:

- List currently running fibers.
- Dump the stack of a stuck fiber.
- Attach a supervisor that gets notified when any child dies.
- Build a tree-view of the fiber forest in a debugger.

ZIO's `FiberId`, `Fiber.Runtime#dump`, and `Supervisor` API are
the obvious comparison. None of those have an analogue in `Aff`,
so none has one in `rio-aff` either. The closest thing is `RIO.Aff.Tracer`
spans, but those are an observability concern attached to the
*logical* call tree, not the runtime fiber tree.

### 5. `Local` is shared state, not per-fiber state

`RIO.Aff.Local a` is implemented as an `Effect.Ref` carried in the
environment. That gives you scoped overrides via `locally`, and
the restore is guaranteed by `Aff.finally`.

What it does *not* give you is the ZIO `FiberRef` semantic of
"a forked fiber gets a copy of the parent's value, mutates
locally, and merges back on join". `rio-aff`'s `Local` has the
opposite default by design: a child fiber sees the parent's
current value at read time, and a child's writes are visible
to the parent.

This is documented in [`docs/11-fiber-local.md`](./11-fiber-local.md)
and is intentional for the `Aff` model, but it does mean code
that assumes ZIO-style fork isolation will break in surprising
ways. A custom runtime is the only way to provide both
semantics; with `Aff` you pick one.

### 6. The scheduler is the JS event loop

`Aff` does not own a scheduler. Every async step is dispatched
through the host's event loop (Node's libuv loop, or the browser
microtask + macrotask queues). That means:

- No priorities. A latency-sensitive `RIO` action and a CPU-bound
  one share the same FIFO.
- No work-stealing or thread pool. Everything is single-threaded.
  CPU-bound work blocks every fiber until it yields.
- No "blocking executor" for synchronous I/O. There is no
  equivalent of ZIO's `ZIO.blocking` to move work off the main
  loop. You can `worker_threads` it manually, but that is an
  application choice, not a runtime feature.
- No yield-on-bind throttling. ZIO yields every N binds to give
  other fibers a chance to run; `Aff` does not. A tight
  `flatMap` chain that never reaches an async boundary holds
  the loop.

A custom runtime can ship a real scheduler with all of the
above. `Aff` cannot, because it does not own the dispatch.

### 7. Time depends on `Clock` discipline

`Aff.delay` calls `setTimeout`. There is no hook into it. A
test cannot virtualise `Aff.delay`.

`rio-aff` works around this by routing every sleep through the
`Clock` service. `RIO.Aff.Test.Clock.newTestClock` is a fake clock
that advances on demand, and every `RIO.Aff.Schedule` runner sleeps
through `Clock`, so the test clock can drive retries and
backoffs deterministically.

The catch: this is *discipline*, not enforcement. Any code that
calls `Aff.delay` directly (or any third-party `Aff` library
that does) bypasses the test clock and runs in wall time. Code
review and the comparison docs flag this, but the compiler
cannot.

A custom runtime owns time at the runtime layer (ZIO's
`TestClock` virtualises every internal timer, not just
user-level sleeps). `Aff` cannot give that to `rio-aff`.

### 8. STM atomicity depends on JS single-threadedness

`RIO.Aff.STM` is implemented as "do the whole transaction inside one
JS event-loop tick". That is sound on every host PureScript
targets today (Node, the browser, Deno, Bun) because they are
all single-threaded JavaScript event loops with cooperative
async.

On a hypothetical multi-threaded host (preemptive threads
sharing memory) the same implementation would be unsound. We
would need real CAS loops or a transaction log. That is a
runtime concern, not a library one. If PureScript ever has a
backend with preemption (a native code generator with threads,
say), `rio-aff`'s STM would need a rewrite.

This is a niche concern, but worth knowing if you are evaluating
`rio-aff` for portability to a non-JS backend.

### 9. `Aff.bracket` is the only release primitive

`rio-aff`'s resource safety, layer release order, scope finalisation,
and concurrency cancellation all bottom out in `Aff.bracket` and
`Aff.finally`. The good news: those are battle-tested and
correct. The not-so-good news:

- Finaliser ordering is LIFO within a scope and otherwise driven
  by how `bracket` calls nest. You cannot, for example, request
  "parallel release" of N independent resources; they release
  one at a time as the scope unwinds.
- The "is this release running because we succeeded, failed
  typed-ly, died, or were killed?" distinction is reconstructed
  by `rio-aff` (via `RIO.Aff.Resource`'s `Exit`-aware variants) rather
  than passed by `Aff`. It works, but the bookkeeping is in
  userland.

A custom runtime can pass a structured `Exit` to every finaliser
without reconstruction. `Aff` cannot.

### 10. The `Channel` primitive is bounded by `Aff`

`RIO.Aff.Channel` is a minimal pull-based primitive that
demonstrates the bedrock `Stream` and `Sink` specialise. It
is deliberately small. A full ZIO-style `Channel` with native
broadcasters, halt-when behaviour, and a parallel fan-out
algebra would benefit enormously from a real scheduler and a
native interrupt channel, both of which `Aff` lacks.

So `RIO.Aff.Stream` and `RIO.Aff.Sink` remain the production-grade
specialisations and the recommended user-facing types. `Channel`
is there to show the unified primitive exists; treat it as a
proof of concept, not the load-bearing stream API.

### 11. Promise interop loses cancellation

JavaScript `Promise` has no cancellation. When you lift a
`Promise` into `Aff` (via `Aff.Promise.toAff` or any FFI that
returns one), the resulting `Aff` is *not* cancellable by
`killFiber`: the kill signal is recorded, but the underlying
HTTP request, file read, or whatever else is still running. The
caller's continuation just never resumes.

`rio-aff` inherits this. Any third-party `Aff` library that wraps a
`Promise` will exhibit this. Use `AbortController`-aware wrappers
where they exist, or accept that "killed fiber" does not always
mean "work stopped".

### 12. Stack traces are JS stacks

`rio-aff` does not maintain its own call graph for error reporting.
`prettyCauseWithStack` renders the JS stack attached to each
`Die` leaf when one is available, which is fine for sync defects
and most async ones, but you do not get the curated "this is
where the typed failure was raised, this is where each handler
caught and re-raised it" trace that ZIO offers.

## What a custom fiber runtime provides: `rio-fiber`

Every ceiling listed above is one a custom fiber runtime can
lift. `rio-fiber` is that runtime. The list below was originally
a forward roadmap; everything in it now ships in
[`rio-fiber`](../rio-fiber/) and is testable today:

- **Native typed-error channel.** `Either (Variant e) a` is gone
  from the bind path. The runtime carries failure and success as
  two separate continuations; the per-bind allocation in the
  failure case disappears.
- **Native `Cause` tree.** Every fiber death produces a `Cause`
  node automatically. `parTraverseCause` and `raceCause` no
  longer have to reconstruct the tree from observed outcomes;
  the runtime hands them one.
- **First-class `Interrupted` leaf.** Kills propagate as
  `Cause.Interrupt FiberId` rather than as `Aff` cancellation
  that finalisers reconstruct.
- **`FiberRef` with copy-on-fork semantics.** Per-fiber state
  that is shared until either side mutates and then forks
  copy-on-write. `RIO.Fiber.Ref` is the primitive; `Local`-style
  ambient state is one use of it.
- **Interrupt masking with restoration.** `uninterruptible`
  with a `restore` block, nested correctly, the way ZIO does it.
- **Fiber identity and supervision.** A real `FiberId`, parent /
  child links via `Scope`, a `Supervisor` API
  (`RIO.Fiber.Supervisor`), and observer hooks for every fiber
  start and end.
- **Virtual time at the runtime layer.** `RIO.Fiber.TestClock`
  drives every sleeping fiber: `advance n` finds every fiber
  parked on a deadline `<= now + n` and resumes it. No more
  Clock-discipline workaround.
- **Structured `Exit` (`Cause`) to finalisers.** Releases see
  the failure cause via `addFinalizerExit`, which both packages
  ship with the same signature. rio-aff additionally offers
  `Cause.acquireReleaseCause` as a bracket-scoped variant for
  cases where you want the release callback to receive the cause
  inline rather than registering it against a `Scope`.
- **Native STM retry parking.** Both packages give you the
  same `retry`-on-`TVar`-change semantics at the user level
  (transactions wake when a read `TVar` / `TRef` changes, no
  busy loop); the difference is implementation. `rio-fiber`'s
  STM parks fibers directly on the runtime scheduler;
  `rio-aff`'s `atomically` simulates the same behaviour by
  registering an `AVar` waiter on every read `TRef` and
  blocking on `AVar.read` until a writer signals one of them.
- **Higher fork throughput.** `forkAll x16 + joinAll` runs at
  roughly 5x the speed of `forkAff x16 + joinFiber` (a single
  specialised op vs. a per-element bind chain).

The pieces that remain runtime-level open work even with
`rio-fiber` on the table:

- **Cancellable Promise interop.** Killing a fiber that is
  parked inside a `fetch` does not, on its own, abort the
  underlying request. `AbortController` plumbing is still
  per-call userland today.
- **Curated cause traces.** `prettyCause` renders the cause
  tree well, but the stack frames it shows are JS frames, not a
  logical "this is where the failure was raised, this is where
  each handler caught and re-raised it" trace.
- **Priority scheduling.** The fiber runtime is FIFO; there is
  no notion of priority bands or yield-on-bind throttling.

If `rio-fiber` covers your ceiling list, you can move there
today. If your ceiling is in the second list above, both
runtimes share it.

## What this means for users today

If you are evaluating the workspace for production use:

- For new projects, pick [`rio-fiber`](../rio-fiber/). The
  ceilings in this document do not apply: typed errors are
  native, `Cause` is native, `FiberRef` and structured
  concurrency are real, the `TestClock` actually wakes fibers.
- For projects that need to stay on `Aff` (existing codebase,
  framework that hands you `Aff a` handlers, library you
  depend on that runs the fiber), pick `rio-aff`. The
  workloads where it shines are the typical Node service
  shape: HTTP handlers, Postgres, scheduled jobs, observability.
  The ceilings rarely show because the workloads are I/O bound,
  the fibers are short-lived, and the failure modes are
  coarse-grained.
- For CPU-heavy, latency-sensitive, or fiber-introspection
  workloads on `rio-aff`, those ceilings will show. You can
  mitigate (route CPU work to a `worker_threads` pool,
  instrument with `RIO.Aff.Tracer`, lean on property tests),
  but you cannot remove them without replacing the runtime.
  In that case, switch to `rio-fiber`.
- For test stability on `rio-aff`, route every sleep through
  `Clock`. Treat any direct `Aff.delay` as a smell in test
  setup. If you depend on a third-party `Aff` library that
  sleeps internally, your test clock will not catch it.
  `rio-fiber`'s `TestClock` does not have this problem because
  it wakes parked fibers directly.
- For typed-error ergonomics and service injection, you get the
  full ZIO / Effect feel on either runtime; those are
  language-level features carried by PureScript's row types,
  not runtime features.
- For cancellation correctness on `rio-aff`, prefer
  `acquireRelease` and `Scope` over manual cleanup. They wrap
  `Aff.bracket` and inherit its guarantees. `rio-fiber`'s
  cancellation model is structured: every interrupt is observed
  at the next safe point and every finaliser runs in LIFO
  order, with the failure cause visible to each finaliser via
  `addFinalizerExit`.

In short: `rio-aff` is `Aff` with a typed-services-and-errors
front end and a structured-concurrency vocabulary; the runtime
ceiling above is its upper bound. `rio-fiber` lifts that ceiling
by owning the runtime. Pick `rio-fiber` for new work; pick
`rio-aff` when staying on `Aff` is a hard requirement.

## Pointers

- The fork-inheritance semantics for ambient state are described
  in [`docs/11-fiber-local.md`](./11-fiber-local.md).
- The cause-tree algebra, its constructors, and how
  `parTraverseCause` / `raceCause` reconstruct it are in
  [`docs/14-causes.md`](./14-causes.md).
- The virtual-time testing story (and the discipline of routing
  sleeps through `Clock`) is in
  [`docs/07-testing.md`](./07-testing.md) and
  [`docs/08-scheduling.md`](./08-scheduling.md).
- The current resource-safety primitives and their `Exit`
  variants are in [`docs/05-resources.md`](./05-resources.md).
- The concurrency vocabulary (`fork`, `race`, `parTraverse`,
  cancellation caveats) is in
  [`docs/06-concurrency.md`](./06-concurrency.md). Note the
  cancellation caveats section in particular: it is the most
  direct expression of point 3 above.
