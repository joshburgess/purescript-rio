# Constraints and limitations: the `Aff` foundation

This document is required reading before you commit to `rio` for
anything ambitious. It tells you, in concrete terms, what `rio` is
built on top of and what that choice gives up. The short version:

> `rio` is a strict superset of `Effect.Aff`. Every primitive in
> `rio` ultimately compiles to an `Aff` action. Anything `Aff`
> cannot do, `rio` cannot do either.

That is a deliberate choice. It is also a hard upper bound. The
sections below spell out what the bound looks like in practice and
where a future `rio` (or a fork) would have to replace `Aff` with
a custom fiber runtime to escape it.

If you have used ZIO in Scala or Effect-TS in TypeScript, you
will recognise most of these ceilings: those libraries hit them
too, but they solve them by shipping their own fiber runtime
rather than reusing a host effect type.

## The core type, restated

```purescript
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))
```

`RIO` is a function from an environment record to an `Aff` that
produces either a typed failure (a `Variant` over the error row
`e`) or a success value of type `a`. Three things follow
immediately:

1. The runtime is `Aff`'s runtime. There is no separate `rio`
   interpreter. `runRIO` reduces to `unRIO m env :: Aff (...)`
   and hands the result to whatever runs `Aff` at the edge
   (`launchAff_`, `Aff.runAff`, etc.).
2. Typed errors are a userland encoding. The `Either (Variant e)`
   wrapper is not visible to `Aff`. `Aff`'s own error channel is
   a single untyped `Error` and is used by `rio` only for
   defects (the `Die` arm of `Cause`).
3. Cancellation, scheduling, async waits, and bracketing are
   `Aff`'s. `rio` adds vocabulary (typed names, structured
   composition, `Scope`-based finalisers) but the underlying
   mechanism is whatever `Aff` provides.

## What `Aff` gives `rio` for free

These are good defaults and a large part of why building on
`Aff` is sensible at all:

- A continuation-based, stack-safe asynchronous effect type.
  Long monadic chains do not blow the JS stack.
- Cooperative cancellation. `Aff.killFiber` interrupts a running
  fiber at the next async boundary, and `Aff.bracket` /
  `Aff.finally` guarantee finaliser execution on success,
  failure, and cancellation.
- A fiber primitive. `Aff.forkAff` gives you a `Fiber` you can
  `joinFiber` or `killFiber`. `rio`'s `fork`, `join`, and
  `interrupt` are thin typed wrappers over these.
- Synchronisation via `Effect.Aff.AVar` (single-slot blocking
  variable). `RIO.Deferred` and most of `RIO.STM`'s blocking
  operators land on `AVar`.
- Promise interop via `Aff.Promise`. Anything that returns a
  JS `Promise` can be lifted with one call.
- Single-threaded JS semantics. There is no need to worry about
  data races on shared mutable references inside a single tick.
  `rio`'s STM implementation exploits this directly.

## What `Aff` cannot give `rio`

Here is the meaningful list. Each item is a real ceiling, not a
nit. Read them as design constraints, not bugs.

### 1. Typed errors are wrapping, not a native channel

`rio`'s typed-error story works by wrapping every result in
`Either (Variant e) a` inside the `Aff`. `Aff`'s own error
channel is reserved for defects (uncaught exceptions, the `Die`
case in `Cause`).

Concretely:

- Every primitive that wants to behave like a typed `RIO`
  primitive must thread the `Either` pattern by hand. `Aff`
  combinators like `Aff.attempt` only see the `Right (Either ...)`
  layer; they do not inspect the `Variant`.
- A native fiber runtime can short-circuit the success
  continuation as soon as a typed failure is raised. `rio` has
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
same in Effect-TS.

`rio`'s `RIO.Cause` is a userland reification. `attemptCause`,
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
  a separate constructor in `RIO.Cause`. Kills propagate as
  `Aff` cancellations through `bracket` finalisers; whether
  the caller can distinguish "killed" from "the bracketed
  computation returned" depends on how the finaliser was
  written. `rio` is consistent about this for its own
  primitives, but a custom runtime could surface
  `Interrupted FiberId` as a first-class cause without that
  discipline.

### 3. Interruption is cooperative and untracked

`Aff` cancellation is cooperative: a running fiber checks for a
kill signal at the next async boundary. Synchronous loops do not
yield. There is also no first-class concept of "this region is
uninterruptible".

`rio` exposes `uninterruptible` and pairs `acquireRelease` with
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
so none has one in `rio` either. The closest thing is `RIO.Tracer`
spans, but those are an observability concern attached to the
*logical* call tree, not the runtime fiber tree.

### 5. `Local` is shared state, not per-fiber state

`RIO.Local a` is implemented as an `Effect.Ref` carried in the
environment. That gives you scoped overrides via `locally`, and
the restore is guaranteed by `Aff.finally`.

What it does *not* give you is the ZIO `FiberRef` semantic of
"a forked fiber gets a copy of the parent's value, mutates
locally, and merges back on join". `rio`'s `Local` has the
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

`rio` works around this by routing every sleep through the
`Clock` service. `RIO.Test.Clock.newTestClock` is a fake clock
that advances on demand, and every `RIO.Schedule` runner sleeps
through `Clock`, so the test clock can drive retries and
backoffs deterministically.

The catch: this is *discipline*, not enforcement. Any code that
calls `Aff.delay` directly (or any third-party `Aff` library
that does) bypasses the test clock and runs in wall time. Code
review and the comparison docs flag this, but the compiler
cannot.

A custom runtime owns time at the runtime layer (ZIO's
`TestClock` virtualises every internal timer, not just
user-level sleeps). `Aff` cannot give that to `rio`.

### 8. STM atomicity depends on JS single-threadedness

`RIO.STM` is implemented as "do the whole transaction inside one
JS event-loop tick". That is sound on every host PureScript
targets today (Node, the browser, Deno, Bun) because they are
all single-threaded JavaScript event loops with cooperative
async.

On a hypothetical multi-threaded host (preemptive threads
sharing memory) the same implementation would be unsound. We
would need real CAS loops or a transaction log. That is a
runtime concern, not a library one. If PureScript ever has a
backend with preemption (a native code generator with threads,
say), `rio`'s STM would need a rewrite.

This is a niche concern, but worth knowing if you are evaluating
`rio` for portability to a non-JS backend.

### 9. `Aff.bracket` is the only release primitive

`rio`'s resource safety, layer release order, scope finalisation,
and concurrency cancellation all bottom out in `Aff.bracket` and
`Aff.finally`. The good news: those are battle-tested and
correct. The not-so-good news:

- Finaliser ordering is LIFO within a scope and otherwise driven
  by how `bracket` calls nest. You cannot, for example, request
  "parallel release" of N independent resources; they release
  one at a time as the scope unwinds.
- The "is this release running because we succeeded, failed
  typed-ly, died, or were killed?" distinction is reconstructed
  by `rio` (via `RIO.Resource`'s `Exit`-aware variants) rather
  than passed by `Aff`. It works, but the bookkeeping is in
  userland.

A custom runtime can pass a structured `Exit` to every finaliser
without reconstruction. `Aff` cannot.

### 10. The `Channel` primitive is bounded by `Aff`

`RIO.Channel` (added recently) is a minimal pull-based primitive
that demonstrates the bedrock `Stream` and `Sink` specialise. It
is deliberately small. A full ZIO-style `Channel` with native
broadcasters, halt-when behaviour, and a parallel fan-out
algebra would benefit enormously from a real scheduler and a
native interrupt channel, both of which `Aff` lacks.

So `RIO.Stream` and `RIO.Sink` remain the production-grade
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

`rio` inherits this. Any third-party `Aff` library that wraps a
`Promise` will exhibit this. Use `AbortController`-aware wrappers
where they exist, or accept that "killed fiber" does not always
mean "work stopped".

### 12. Stack traces are JS stacks

`rio` does not maintain its own call graph for error reporting.
`prettyCauseWithStack` renders the JS stack attached to each
`Die` leaf when one is available, which is fine for sync defects
and most async ones, but you do not get the curated "this is
where the typed failure was raised, this is where each handler
caught and re-raised it" trace that ZIO offers.

## What a custom fiber runtime would provide

If `rio` (or a successor) ever shipped its own runtime in place
of `Aff`, here is what would change. Use this as a forward
roadmap, not a current feature list.

- **Native typed-error channel.** `Either (Variant e) a` would
  become two separate continuations inside the runtime, removing
  the per-bind `Either` allocation in the failure path.
- **Native `Cause` tree.** Every fiber death produces a `Cause`
  node automatically; `parTraverseCause` and `raceCause` would
  no longer need to reconstruct the tree from observed outcomes.
- **First-class `Interrupted` leaf.** Kills propagate as
  `Cause.Interrupted FiberId` rather than as `Aff` cancellation
  that finalisers reconstruct.
- **`FiberRef` with copy-on-fork semantics.** Both the "shared"
  default and the "isolated" alternative would be available;
  `Local` would be one implementation, `FiberRef` another.
- **Interrupt masking with restoration.** Nested
  `uninterruptible` regions with `restore` blocks the way ZIO
  does it.
- **Fiber identity, naming, and supervision.** A real
  `FiberId`, parent / child links, a `Supervisor` API, and a
  `dump` that prints the fiber forest for stuck-fiber debugging.
- **A real scheduler.** Priorities, yield-on-bind throttling,
  a blocking-executor pool for synchronous I/O, optional
  cooperative work-stealing if a backend ever supports it.
- **Virtual time at the runtime layer.** `TestClock` style:
  every timer, every sleep, every internal back-off becomes
  virtualised, not just user-level `Clock.sleep`.
- **Structured `Exit` to finalisers.** Releases receive an
  `Exit e a` describing why they ran, without userland
  bookkeeping.
- **Cancellable Promise interop.** First-class
  `AbortController` integration so killing a fiber actually
  aborts in-flight `fetch` requests.
- **Curated traces.** A logical effect-graph trace rather than
  the host JS stack.

That is a substantial undertaking. ZIO has spent years on its
runtime; Effect-TS has rebuilt large parts of theirs more than
once. Reusing `Aff` is the pragmatic choice while `rio` proves
its surface and gathers users. Replacing `Aff` is the eventual
escape hatch if the ceiling starts to bite.

## What this means for users today

If you are evaluating `rio` for production use, here is the
honest summary:

- For the typical Node service (HTTP handlers, Postgres,
  scheduled jobs, observability), `rio` is genuinely usable.
  The ceilings above almost never come up because the workloads
  are I/O bound, the fibers are short-lived, and the failure
  modes are coarse-grained.
- For anything CPU-heavy, latency-sensitive, or that needs to
  introspect the fiber forest in production, those ceilings
  will show. You can mitigate (route CPU work to a
  `worker_threads` pool, instrument with `RIO.Tracer`, lean on
  property tests), but you cannot remove them without replacing
  the runtime.
- For test stability, route every sleep through `Clock`. Treat
  any direct `Aff.delay` as a smell in test setup. If you depend
  on a third-party `Aff` library that sleeps internally, your
  test clock will not catch it.
- For typed-error ergonomics and service injection, you get the
  full ZIO / Effect-TS feel; those are language-level features
  carried by PureScript's row types, not runtime features, and
  they do not depend on `Aff` at all.
- For cancellation correctness, prefer `acquireRelease` and
  `Scope` over manual cleanup. They wrap `Aff.bracket` and
  inherit its guarantees; ad-hoc `try / finally` patterns in
  FFI will not.

In short: `rio` is `Aff` with a typed-services-and-errors front
end and a structured-concurrency vocabulary. Treat the runtime
ceiling above as the upper bound on what it can do, and the
ZIO / Effect-TS feature set as the eventual target. The gap is
the work a future custom runtime would close.

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
