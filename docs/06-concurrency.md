# Concurrency

> **Naming convention.** This guide uses `RIO.Aff.*` module
> names in code samples. The same APIs exist under
> `RIO.Fiber.*` for the premier rio-fiber package, mostly with
> a mechanical prefix swap. One rename worth flagging:
> `RIO.Aff.Deferred` spells its operations `makeDeferred` /
> `succeedDeferred` / `failDeferred` / `awaitDeferred` /
> `pollDeferred`; `RIO.Fiber.Deferred` drops the suffix and
> uses `make` / `succeed` / `fail` / `await` / `poll`.
> The `Aff`-specific guarantees (cooperative cancellation,
> `Effect.Aff.bracket`-backed resource safety) are documented
> here against the rio-aff implementation; rio-fiber provides
> the same observable guarantees through its custom fiber
> interpreter.

`RIO`'s fork-based concurrency surface is a `Fiber` type, the
`fork` / `join` / `interrupt` primitives, parallel combinators
(`parTraverse`, `parSequence`, `zipPar`), and racing (`race`,
`raceAll`). In rio-aff everything is built directly on
`Effect.Aff`; rio-fiber ships its own custom fiber interpreter
with the same observable surface. This document describes:

1. The interruption model: what cancels what, when, and why.
2. Uninterruptible regions, where they live, and how to extend them.
3. How concurrency interacts with `acquireRelease` and `Scope`.

The authoritative source for what `Aff` guarantees is
`spikes/aff-interruption/FINDINGS.md`. The text below cites it
scenario by scenario.

## What "interrupt" means

`interrupt fib` sends a kill exception to the underlying
`Effect.Aff.Fiber`. The Aff runtime's behaviour is:

- If `fib` is currently waiting (on `delay`, on an async callback,
  on `bracket` between phases), the kill takes effect on the next
  event-loop tick. The wait is aborted. This is scenario **S1** in
  the spike: the fiber is killed within ~1ms of the call.
- If `fib` is in a tight synchronous loop with no async boundary,
  the kill is queued but never lands. This is scenario **S2**: the
  loop runs to completion in one JS turn. The mitigation is to
  yield periodically (`liftAff (Aff.delay (Milliseconds 0.0))`),
  which gives the runtime a chance to deliver the kill. Scenario
  **S2b** confirms the kill lands after the first yield. This is
  the same cooperative-cancellation caveat that exists in ZIO and
  Effect.
- If `fib` has already completed, interrupt is a no-op. Subsequent
  `join`s return the cached result. This is scenario **S4**.

Killing a fiber more than once is also a no-op (**S5**).

From the joiner's perspective the kill surfaces as an `Aff`
exception. `join` does not catch it: it propagates as a defect and
is observable only via `RIO.Aff.Error.sandbox`. We chose this over
turning the kill into a typed failure because the kill is not on
the fiber's error row; it has been imposed externally.

## What survives interruption

This is the load-bearing guarantee for `RIO.Aff.Resource`:

> `Effect.Aff.bracket` runs its release action on every termination
> path, including external `killFiber`. The release phase is itself
> uninterruptible: a second kill that lands during release does not
> stop release from completing.

(Scenario **S3** in the spike.) Everything resource-related in `RIO`
sits on top of this:

- `RIO.Aff.Resource.acquireRelease` is a direct `bracket`
  wrapper. If the fiber running it is killed while inside the
  use phase, the release runs uninterruptibly.
- `RIO.Aff.Resource.scoped` allocates a `Scope` and uses
  `bracket` to drain its finalizer stack on exit. The drain is
  in the release phase of that `bracket`, so it runs to
  completion under any termination, including kill.
- Layer-registered finalizers (`RIO.Aff.Layer.provideLayer` and
  friends) hang off the same `Scope` machinery, so the same
  guarantee applies.

In other words: from `RIO`'s perspective there are no "uninterruptible
regions" you need to construct yourself. Every primitive that owns a
resource already wraps it in `bracket`. If you want to push a
finalizer onto an outer scope, use `addFinalizer`; you don't need to
think about interruptibility separately.

## The cooperative-cancellation caveat

`Aff`'s cancellation is cooperative: the kill is delivered at async
boundaries (`delay`, the next callback in a chain, etc.). A tight
synchronous loop will never reach a boundary, so a kill targeted at
it will not land until the loop ends.

For CPU-bound work this means:

```purescript
crunch :: forall r e. Int -> RIO r e Int
crunch n = liftEffect (Ref.modify' ... bigLoop ...)
```

is **not** interruptible. If the loop is long enough that interruption
matters, yield periodically:

```purescript
import Effect.Aff (Milliseconds(..), delay)

crunchYielding :: forall r e. Int -> RIO r e Int
crunchYielding n = do
  ... do some work ...
  liftAff (delay (Milliseconds 0.0))   -- give the runtime a tick
  ... continue ...
```

A zero-duration delay is sufficient: it just registers the
continuation on the macrotask queue, which gives any pending kill a
chance to run first. The spike's S2b shows the kill lands within
~50 yield points when yielding every 100 iterations.

There is no `RIO.yield` primitive because
`liftAff (delay (Milliseconds 0.0))` is short, explicit, and exposes
exactly what's happening at the `Aff` layer. A named helper can be
introduced later if the ergonomic case for one emerges.

## How `race` interacts with resources

`race a b` runs both branches concurrently and returns the winner's
result (success or typed failure). The loser is interrupted by the
Aff runtime. Because the loser's interrupt rides on the same
mechanism as `interrupt` on an explicit fiber, the same guarantees
apply:

- Any `acquireRelease` or `Scope` finalizer held by the loser runs.
- The release runs uninterruptibly, so even if the parent fiber
  itself is killed during the race, the loser's finalizers complete.

The `RIO.Aff.Concurrency` test suite exercises both: a `race` test
where the slow branch acquires a resource and the fast branch
wins, and a `raceAll` test with two losers, each registering a
finalizer. Both report the expected release events after the race
resolves.

If you want a race in which one side is *not* killed when the other
wins, use `fork` + `join` on the long-running side and proceed
without joining the short-running one. There is no "non-cancelling
race" primitive in `RIO.Aff.Concurrency` because the use case is
better served by explicit `fork`.

## `parTraverse` failure semantics

`parTraverse f xs` runs every action concurrently:

- If any branch returns `Left v`, the **first** such failure
  cancels every sibling fiber and is what the combinator returns.
  This matches ZIO `foreachPar` and Effect `forEach` with
  `concurrency: "unbounded"`.
- "First" means observation order, not array index: whichever
  branch's `Left` is captured into the shared first-failure ref
  first wins. In practice, fast failures win over slow ones.
- Defects from any branch propagate as `Aff` defects (observable
  via `RIO.Aff.Error.sandbox`) and also interrupt the siblings.

The same short-circuit applies to `parSequence` (which is
`parTraverse identity`) and to `zipPar` (where the first `Left`
from either side cancels the other).

If you genuinely need run-to-completion semantics, sandbox each
branch first so its failure becomes a `Right (Left _)` on the
parent's success row.

### Bounded concurrency: `parTraverseN`

`parTraverse` is unbounded: one fiber per element. When each
element is heavy (an outbound connection, a large memory
allocation), use `parTraverseN n f xs` to cap the number of fibers
in flight. The implementation splits the input into chunks of size
`n` and `parTraverse`s each chunk in turn, so the short-circuit
semantics apply within each chunk; a failure in chunk `k` aborts
chunks `k+1..` before they start.

```purescript
-- at most 8 fetches in flight at once
bodies = parTraverseN 8 fetch urls
```

### Concurrent fan-out with `Par.ado`

`RIO.Aff.Concurrency.Par` exposes `apply` / `map` / `pure` so a
qualified `ado` block runs each `<-` line concurrently:

```purescript
import RIO.Aff.Concurrency.Par as Par

fetchAll :: forall r e. UserId -> RIO r e Bundle
fetchAll uid = Par.ado
  user  <- fetchUser uid
  prefs <- fetchPrefs uid
  posts <- fetchPosts uid
  in { user, prefs, posts }
```

Each fetch runs as its own `ParAff` branch; the block returns
once every branch completes. Wall-clock cost is roughly the
slowest branch, not the sum.

**Use `ado`, not `do`.** Qualified `do` would still sequence
because monadic `bind` for parallel composition cannot exist
without the second action waiting for the first's value. Every
`<-` in a `Par.ado` block must be independent of the bindings
above it.

**Failure bias differs from `zipPar` / `parTraverse`.** `Par.ado`
runs every branch to completion and returns the leftmost typed
failure. This makes it the right fit for fan-outs where each
branch should always be given a chance (independent reads, side
effects you want to attempt regardless of siblings). For
short-circuiting fan-out, stick with `zipPar` (two branches) or
`parTraverse` (a homogeneous array), which cancel the loser the
moment one branch fails.

### Deadlines: `timeout`

`timeout ms action` is `race action (delay ms *> pure Nothing)`
with the success branch wrapped in `Just`. On success the action's
result is `Just a`; on deadline the action is interrupted and the
result is `Nothing`. Typed failures from the action propagate
unchanged: `timeout` never converts a failure into a `Nothing`.

```purescript
-- a 500ms cache miss falls back to the source
result <- timeout (Milliseconds 500.0) (fromCache key)
case result of
  Just hit -> pure hit
  Nothing -> fromSource key
```

## Critical sections: `uninterruptible`

`uninterruptible action` runs `action` with kills queued, so the
section completes before any pending interrupt lands. It's a thin
wrapper over `Effect.Aff.invincible`.

Reach for it when the inner action's mid-execution state is
observable to other fibers (a multi-step `Ref` update, a handover
that records "we own this resource") and a kill landing partway
through would leave the world inconsistent.

`acquireRelease` already runs its release phase uninterruptibly;
`uninterruptible` is for the cases where the *body* (not the
release) is what must not be torn down.

```purescript
uninterruptible do
  liftEffect (Ref.write True committed)
  liftEffect (Ref.modify_ (_ + 1) commitCounter)
```

## Structured fibers: `forkScoped`

`forkScoped scope action` is like `fork`, but the fiber's lifetime
is bounded by `scope`. When the scope exits (on any termination
path, including kill), the fiber is interrupted as part of its
LIFO finalizer pass. This gives you "the fiber cannot outlive its
enclosing block" without writing the finalizer by hand.

```purescript
scoped do
  scope <- ask (Proxy :: Proxy "scope")
  _ <- forkScoped scope (poll endpoint)
  serveRequests
  -- poll is interrupted automatically when `scoped` returns
```

This is the structured-concurrency counterpart of `fork`. Plain
`fork` is still available for unbounded fibers (background tasks
the program is happy to leak).

## Fiber handoff: `Deferred`

`RIO.Aff.Deferred` is a one-shot write-once cell over
`Effect.Aff.AVar` (rio-fiber ships the equivalent
`RIO.Fiber.Deferred` over its own scheduler). Fiber A blocks on `awaitDeferred`; fiber B
fills the cell with `succeedDeferred` (or `failDeferred`); A wakes
up with the value (or typed failure). Subsequent fills return
`False` instead of overwriting; subsequent awaits see the same
value (reads are non-destructive).

```purescript
ready <- makeDeferred
_ <- fork (initWorker *> succeedDeferred ready unit)
awaitDeferred ready
useWorker
```

Use `pollDeferred` for the non-blocking probe.

## What `RIO` does not give you

For honesty, here is what `RIO` does *not* do:

- **Implicit structured concurrency.** Plain `fork` returns a
  fiber whose lifetime is unbounded; if you want
  "killed-when-parent-dies" semantics, reach for `forkScoped` and
  hand it a `Scope`. There is no automatic supervisor tree.
- **Interrupt-with-cause (rio-aff only).** In rio-aff, kill
  exceptions are `Aff` errors carrying a message; ZIO's richer
  notion (interrupted-by-whom, interrupted-due-to-failure-elsewhere,
  etc.) is not reproduced. rio-fiber does track this: `Cause e`
  carries a first-class `Interrupt FiberId` constructor that
  records the interrupting fiber's identity, and `Then` / `Both`
  preserve cause trees when an interrupt collides with a failure.
  See [`docs/14-causes.md`](./14-causes.md) for the full algebra.

`RIO.Aff.Local` / `RIO.Fiber.Local` (`docs/11-fiber-local.md`)
covers ambient implicit-context state with `locally`-scoped
overrides; `RIO.Aff.STM` / `RIO.Fiber.STM` (`docs/09-stm.md`)
covers transactional refs, queues, maps, semaphores, and
pub/sub hubs. Both are part of the current surface.

## Pointers

- `rio-aff/src/RIO/Aff/Concurrency.purs` and
  `rio-fiber/src/RIO/Fiber/Concurrency.purs`: implementations
  of every primitive described above.
- `rio-aff/src/RIO/Aff/Deferred.purs` and
  `rio-fiber/src/RIO/Fiber/Deferred.purs`: the one-shot
  fiber-handoff primitive.
- `rio-aff/test/Test/RIO/Aff/ConcurrencySpec.purs` and
  `rio-aff/test/Test/RIO/Aff/DeferredSpec.purs`: tests
  covering the scenarios cited here, including the
  short-circuit cancellation behaviour for `parTraverse` /
  `zipPar` and the scope-bounded lifetime of `forkScoped`.
- `spikes/aff-interruption/FINDINGS.md`: the authoritative source
  for `Aff`'s cancellation behaviour and the canonical
  cooperative-cancellation caveat.
- Worked example:
  [`examples/worker-pool/`](../examples/worker-pool/) fans work
  out over a fixed `Semaphore`-bounded pool, drives it with
  `parTraverseCause` for multi-failure (cause-collecting)
  accumulation, and demonstrates
  `forkScoped` plus a `Deferred`-gated shutdown signal.
