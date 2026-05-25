# rio-aff

The `Effect.Aff`-backed runtime for `purescript-rio`. A complete
implementation of the same three-channel design as
[`rio-fiber`](../rio-fiber/), interpreted on top of
`purescript-aff` instead of a custom fiber runtime.

`RIO r e a` is the same type you'd see in `rio-fiber`: typed
environment row `r`, typed failure row `e`, success value `a`.
What `rio-aff` does is compile every primitive down to an `Aff`
action and reuse `Aff`'s scheduler, canceler protocol, and
finaliser machinery instead of shipping its own.

Programs are written against `RIO.Aff.*`. The top-level entry
points are `RIO.Aff.Core.runRIO`, `RIO.Aff.Core.runRIO'`, and
`RIO.Aff.Core.unRIO`, all of which produce an `Aff` action you
can `launchAff_` or hand to any host that already speaks `Aff`.

## When to pick `rio-aff` over `rio-fiber`

[`rio-fiber`](../rio-fiber/) is the recommended default for new
projects. `rio-aff` is the right choice when:

- **You're already on `Aff`.** Existing code, libraries you
  depend on, or frameworks you target are written against `Aff`.
  Picking the `Aff`-backed runtime keeps your effect graph
  homogeneous: no boundary conversions, no `runAffThrow`
  bridges, no two-runtime composition story.
- **You want the smallest possible surface to learn first.**
  `rio-aff` shares its mental model with the `purescript-aff`
  vocabulary many PureScript users already know.
- **You're embedding into a host that hands you `Aff`.** Some
  PureScript HTTP servers, GraphQL frameworks, and SDKs accept
  an `Aff a` handler. `rio-aff` lets you write that handler in
  the three-channel style and discharge to `Aff` at the call
  boundary with `runRIO` / `runRIO'`.

## What you give up by staying on `Aff`

These are not "we haven't written them yet" features in
`rio-aff`. Each one is either impossible to add on top of `Aff`
without rewriting its interpreter, or has been tried and found
to be unworkable inside `Aff`'s canceler protocol. The full
analysis is in [`docs/aff-constraints.md`](../docs/aff-constraints.md);
the summary:

- **No fiber identity.** `Aff` has no stable id per fiber, so
  tracing, metrics, and structured logging cannot correlate a
  span to "the fiber it ran on".
- **`FiberRef` is opt-in, not native.** `RIO.Aff.FiberRef`
  provides true per-fiber state (snapshot-on-fork: the child
  inherits a full eager copy at fork time, and subsequent
  writes on either side stay local). It works without
  scheduler-level fork hooks by carrying a per-fiber storage
  map in the environment row under a `fiberRefs` service and
  cloning the map inside `forkFiber` / `forkFiberScoped`
  before delegating to `forkAff`. The cost is that callers
  must use `forkFiber` (not plain `forkAff`) and the env row
  must include `fiberRefs :: FiberRefs`; `RIO.Fiber.FiberRef`
  bakes both into the runtime and needs neither. `RIO.Aff.Local`
  remains the shared-`Effect.Ref` model (no per-fiber
  isolation) for the simpler "ambient context" use case.
- **No first-class `Cause` tree.** `Aff` collapses everything
  into one error channel. `rio-aff` reifies `Cause` at
  boundaries (`attemptCause`, `parTraverseCause`, `raceCause`,
  `prettyCauseWithStack`), but the native failure carrier is
  still a single `Variant e`. You can't ask "was this failure
  also accompanied by an interrupt?" without going through a
  reifier.
- **No `TestClock` that wakes parked fibers.** A virtual clock
  can't observe a fiber parked inside `Aff.delay`. The test
  harness has to fake the parked state itself; `RIO.Aff.Test.Clock`
  works by `Clock`-service discipline, not by waking fibers.
- **No structured concurrency in the proper sense.** `Aff`
  forks are detached by default and there is no protocol to
  attach them to a parent scope's lifetime. `rio-aff` simulates
  this with `forkScoped`, but the underlying fiber is not
  cancellable by closing a parent scope without explicit
  cancellation plumbing.
- **STM `retry` parks on an `AVar`, not the native scheduler.**
  `RIO.Aff.STM`'s `retry` registers an `AVar` waiter against
  every `TRef` the transaction read and blocks on `AVar.read`
  until a writer signals one of them. The observable semantics
  match `rio-fiber`'s scheduler-native park (no busy loop, wake
  on `TRef` change), but the implementation goes through `Aff`'s
  `AVar` primitive rather than parking the fiber directly. On
  large contention or many-reader workloads the extra `AVar`
  hop costs a few hundred nanoseconds per wake.
- **Slower bind hot path.** About 90 ns per `bind` versus
  about 10 ns in `rio-fiber` on the workspace benchmarks.

If any of those items is on your hot path or your correctness
boundary, pick `rio-fiber`.

## A 30-second tour

```purescript
import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Env (ask, provideAll)
import Type.Proxy (Proxy(..))

type Logger = { log :: String -> Effect Unit }

greet
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
greet name = do
  logger <- ask (Proxy :: Proxy "logger")
  liftEffect (logger.log ("hello, " <> name))

main :: Effect Unit
main = launchAff_ do
  let consoleLogger = { log: Console.log }
  runRIO' (provideAll { logger: consoleLogger } (greet "world"))
```

## What's here

The full surface mirrors `rio-fiber`'s module list, with the
prefix `RIO.Aff.*` rather than `RIO.Fiber.*`:

- `RIO.Aff.Core`, `RIO.Aff.Env`, `RIO.Aff.Error`: the type,
  runners, service injection, typed-error combinators.
- `RIO.Aff.Cause`: the failure algebra and reifying
  combinators (`attemptCause`, `parTraverseCause`, `raceCause`,
  `acquireReleaseCause`, `prettyCauseWithStack`).
- `RIO.Aff.Resource`, `RIO.Aff.Layer`: `acquireRelease`,
  `Scope` with LIFO finalisers, and the `Layer rIn e rOut`
  algebra with `>>>` / `<+>` composition.
- `RIO.Aff.Concurrency`: `fork`, `forkScoped`, `join`,
  `interrupt`, `uninterruptible`, `timeout`, `parTraverse`,
  `parTraverseN`, `parSequence`, `zipPar`, `race`, `raceAll`.
- `RIO.Aff.Deferred`, `RIO.Aff.Semaphore`, `RIO.Aff.Queue`,
  `RIO.Aff.Hub`: async coordination primitives.
- `RIO.Aff.STM` (with `TVar` / `TRef`) plus `STM.TArray`,
  `STM.TChan`, `STM.TDeferred`, `STM.THub`, `STM.TMap`,
  `STM.TMVar`, `STM.TQueue`, `STM.TSemaphore`, and `STM.TSet`:
  software transactional memory.
- `RIO.Aff.Stream`, `RIO.Aff.Stream.Par`,
  `RIO.Aff.Stream.Concurrent`, `RIO.Aff.Stream.Resource`,
  `RIO.Aff.Stream.Timed`, `RIO.Aff.Sink`, `RIO.Aff.Channel`:
  pull-based effectful streams, parallel combinators, composable
  sinks, and the unified Channel primitive.
- `RIO.Aff.Clock`, `RIO.Aff.Random`, `RIO.Aff.Config`,
  `RIO.Aff.Config.Rotating`, `RIO.Aff.Schedule`: service types
  plus live and seeded backends.
- `RIO.Aff.Tracer`, `RIO.Aff.Metrics`, `RIO.Aff.Logger`:
  observability services with noop, live, and recording
  backends.
- `RIO.Aff.Local`: ambient state with scoped overrides
  (process-global, not per-fiber).
- `RIO.Aff.Spec`, `RIO.Aff.Test.*`: `itRIO` adapters for
  `purescript-spec`, plus the recording test backends.

For the conceptual walkthrough (services, layers, errors,
resources, concurrency, STM, observability), see the docs in
[`docs/`](../docs/). The walkthroughs use `RIO.Aff.*` examples
and the same concepts carry over to `rio-fiber` with the prefix
change.

## Build

```sh
npx spago build -p rio-aff
npx spago test -p rio-aff
```

## Status

Pre-release. `rio-aff` is feature-complete for the shared
three-channel surface and is maintained as the ecosystem-friendly
alternative to `rio-fiber`. New design work in this repository
targets `rio-fiber` first, then ports back to `rio-aff` when it
can be expressed on top of `Aff`. Bug fixes land in both.
