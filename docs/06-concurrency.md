# Concurrency

Phase 6 adds fork-based concurrency to `RIO`: a `Fiber` type, the
`fork` / `join` / `interrupt` primitives, parallel combinators
(`parTraverse`, `parSequence`, `zipPar`), and racing (`race`,
`raceAll`). Everything is built directly on `Effect.Aff`. This
document describes:

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
  Effect-TS.
- If `fib` has already completed, interrupt is a no-op. Subsequent
  `join`s return the cached result. This is scenario **S4**.

Killing a fiber more than once is also a no-op (**S5**).

From the joiner's perspective the kill surfaces as an `Aff`
exception. `join` does not catch it: it propagates as a defect and
is observable only via `RIO.Error.sandbox`. We chose this over
turning the kill into a typed failure because the kill is not on
the fiber's error row; it has been imposed externally.

## What survives interruption

This is the load-bearing guarantee for `RIO.Resource`:

> `Effect.Aff.bracket` runs its release action on every termination
> path, including external `killFiber`. The release phase is itself
> uninterruptible: a second kill that lands during release does not
> stop release from completing.

(Scenario **S3** in the spike.) Everything resource-related in `RIO`
sits on top of this:

- `RIO.Resource.acquireRelease` is a direct `bracket` wrapper. If
  the fiber running it is killed while inside the use phase, the
  release runs uninterruptibly.
- `RIO.Resource.scoped` allocates a `Scope` and uses `bracket` to
  drain its finalizer stack on exit. The drain is in the release
  phase of that `bracket`, so it runs to completion under any
  termination, including kill.
- Layer-registered finalizers (`RIO.Layer.provideLayer` and
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

We are not adding a `RIO.yield` primitive in Phase 6 because
`liftAff (delay (Milliseconds 0.0))` is short, explicit, and exposes
exactly what's happening at the `Aff` layer. If a future phase finds
ergonomic value in a named helper we can introduce one then.

## How `race` interacts with resources

`race a b` runs both branches concurrently and returns the winner's
result (success or typed failure). The loser is interrupted by the
Aff runtime. Because the loser's interrupt rides on the same
mechanism as `interrupt` on an explicit fiber, the same guarantees
apply:

- Any `acquireRelease` or `Scope` finalizer held by the loser runs.
- The release runs uninterruptibly, so even if the parent fiber
  itself is killed during the race, the loser's finalizers complete.

The `RIO.Concurrency` test suite exercises both: a `race` test
where the slow branch acquires a resource and the fast branch
wins, and a `raceAll` test with two losers, each registering a
finalizer. Both report the expected release events after the race
resolves.

If you want a race in which one side is *not* killed when the other
wins, use `fork` + `join` on the long-running side and proceed
without joining the short-running one. There is no "non-cancelling
race" primitive in `RIO.Concurrency` because the use case is
better served by explicit `fork`.

## `parTraverse` failure semantics

`parTraverse f xs` runs every action concurrently and waits for all
of them to complete before returning. Crucially:

- If any branch returns `Left v`, the first such failure (in array
  order) is what the combinator returns.
- The other branches are **not** interrupted on failure: they run
  to completion. This is the natural shape of an applicative
  layered on `ParAff`.

If you want first-failure-cancels-the-rest semantics, build it
from `race` plus your own array sweep, or wait for a future phase
that adds it as a named combinator. We did not add it in 6.2
because the simple `ParAff` shape composes with everything else
the way users expect.

## What `RIO` does not give you

For honesty, here is what `RIO` (as of Phase 6) does *not* do:

- **Structured concurrency.** A parent fiber being killed does not
  automatically kill its children. `Aff` doesn't track child-of
  relationships; `RIO` doesn't add them. If you `fork` a child and
  then your parent fiber is killed, the child runs to completion (or
  until something kills it). Structured-concurrency semantics
  (`forkScoped`, supervisor trees, etc.) are out of scope and would
  be a future phase.
- **Interrupt-with-cause.** Kill exceptions are just `Aff` errors
  carrying a message. ZIO has a richer notion (interrupted-by-whom,
  interrupted-due-to-failure-elsewhere, etc.) that `RIO` does not
  reproduce.
- **Fiber-local state.** No equivalent of ZIO's `FiberRef`. Use a
  regular service or a `Ref` if you need per-fiber state.

These are deliberate omissions to keep Phase 6 lean. They are not
hard to add later: ZIO's design and the existing `RIO` row-typed
machinery make each of them a reasonable phase-9-or-later addition.

## Pointers

- `src/RIO/Concurrency.purs`: implementations of every primitive
  described above.
- `test/Test/RIO/ConcurrencySpec.purs`: 22 tests covering the
  scenarios cited here.
- `spikes/aff-interruption/FINDINGS.md`: the authoritative source
  for `Aff`'s cancellation behaviour and the canonical
  cooperative-cancellation caveat.
- `docs/04-resources.md` (forthcoming): the resource-safety doc
  this one assumes you have already read for the `acquireRelease`
  / `Scope` background.
