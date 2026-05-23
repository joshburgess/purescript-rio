# `rio-fiber` vs Effect-TS: parity audit and improvement plan

Snapshot comparison of `rio-fiber` against Effect-TS
(`@effect/effect`) with concrete improvement opportunities,
ranked by value-per-effort. Compiled 2026-05-20 against the
then-current Effect-TS surface and the rio-fiber surface at
that time (the fiber package has since grown to ~98 modules).

This is a working planning document. As items are implemented
they should be checked off in the relevant section, not deleted.

## Top 10 improvement opportunities (ranked by value-per-effort)

| # | Item | Effort | Value | Status |
|---|---|---|---|---|
| 1 | `asyncAbortable` (AbortSignal-aware async) | S | High | done |
| 2 | `onExit` / `ensuringWith` (finalizer receives `Cause`) | S | High | done |
| 3 | `Mailbox` primitive (Queue + `end` / `fail` + `toStream`) | M | High | done |
| 4 | Queue variants (`unbounded`, `dropping`, `sliding`) | S | Med-High | done |
| 5 | `FiberHandle` / `FiberSet` (auto-supervised fiber collections) | M | High | done |
| 6 | Stream JS interop (`fromAsyncIterable`, `from/toReadableStream`) | M | High | partial (`fromAsyncIterable` done; `ReadableStream` interop pending) |
| 7 | Metrics labels + `Frequency` + `timer` shorthand | M | High | open |
| 8 | `Pool.invalidate` + `Pool.makeWithTTL` | M | High | done |
| 9 | Logger annotations + JSON formatter | M | High | open |
| 10 | Stream `peel` / `transduce` / `changes` | S | Med | done |

### #1 `asyncAbortable`

**Status:** Shipped in `RIO.Fiber.AbortSignal` as `asyncAbortable`.
Shipped shape diverges from the proposal: the register receives
`AbortSignal` and the `resume` callback as curried arguments
(`AbortSignal -> (Either (Variant e) a -> Effect Unit) -> Effect
Unit`) rather than a single combined callback, and the cancel
action is constructed automatically from the controller rather
than returned by the register. The original proposal is preserved
below for the historical record.

**Problem.** `RIO.Fiber.Core.async` accepts a register callback
that returns a cancel `Effect Unit`. Every caller that wants
`fetch` cancellation has to allocate an `AbortController`,
plumb the signal into the `fetch` options, and write the cancel
effect to call `controller.abort()`. That is boilerplate at
every HTTP call.

**Shape.**

```purescript
asyncAbortable
  :: forall r e a
   . ((AbortSignal -> Either (Variant e) a -> Effect Unit) -> Effect Unit)
  -> RIO r e a
```

**Implementation.** Pure FFI wrapper over `async`. Allocate an
`AbortController` synchronously, hand the signal to the
register callback, and have the cancel effect call `abort()`.

### #2 `onExit` / `ensuringWith`

**Status:** Shipped in `RIO.Fiber.Core` as `ensuringWith` (with
`onExit` as the success-discarding variant). Shipped shape
diverges from the proposal: `ensuringWith` does not introduce a
fresh `e'` error row for the handler; the callback runs in the
same `RIO r e Unit` as the action. (`onExit`'s callback uses
the empty row `RIO r () Unit` to enforce that the handler
cannot introduce a new failure mode.) The original proposal is
preserved below.

**Problem.** `ensuring` finalizers do not see the cause of
exit. Callers cannot log differently on success vs failure vs
interrupt without writing the `catchAllCause` + `ensuring`
combination by hand each time.

**Shape.**

```purescript
ensuringWith
  :: forall r e e' a
   . RIO r e a
  -> (Either (Cause e) a -> RIO r e' Unit)
  -> RIO r e a

onExit
  :: forall r e a
   . RIO r e a
  -> (Cause e -> RIO r () Unit)
  -> RIO r e a
```

**Implementation.** Library code on top of `causeOf` +
`ensuring`. No interpreter changes.

### #3 `Mailbox` primitive

**Status:** Shipped in `RIO.Fiber.Mailbox`. Shipped shape
diverges from the proposal: the terminal signal is `done` (not
`end`); there is no `fail` export (no typed-error signalling on
the Mailbox); the stream integration is an internal helper rather
than a `toStream` method on the type; and `make :: Int -> Int ->
Effect (Mailbox a)` takes capacity and producer count as `Effect`
(not `RIO`), with no `e` parameter. The original proposal is
preserved below.

**Problem.** No first-class bridge between a fiber-based
producer (event listener, external queue) and a pull-based
`Stream` consumer. Users reinvent the `Queue` + `Deferred` +
terminal-signal pattern.

**Shape.**

```purescript
data Mailbox e a
make    :: forall r e' a e. Int -> RIO r e' (Mailbox e a)
offer   :: forall r e' e a. Mailbox e a -> a -> RIO r e' Unit
end     :: forall r e' e a. Mailbox e a -> RIO r e' Unit
fail    :: forall r e' e a. Mailbox e a -> Variant e -> RIO r e' Unit
toStream :: forall r e a. Mailbox e a -> Stream r e a
```

**Implementation.** Wraps a bounded `Queue` and a `Deferred`
holding the terminal `Either (Variant e) Unit`. `toStream`
drains the queue and then checks the terminal cell on each
pull.

### #4 Queue variants

**Status:** Shipped in `RIO.Fiber.Queue` as `unbounded`,
`dropping`, and `sliding` alongside the original bounded
constructor. Shipped shape diverges from the proposal: all three
constructors are `Effect`-valued with no `r` / `e` parameters,
matching the existing `make` constructor (the proposal showed
them as `RIO`-valued). The original proposal is preserved below.

**Problem.** `Queue.make` is bounded-with-backpressure only.
Real-time pipelines need drop-new / drop-old / unbounded
policies.

**Shape.**

```purescript
Queue.unbounded :: forall r e a. RIO r e (Queue a)
Queue.dropping  :: forall r e a. Int -> RIO r e (Queue a)
Queue.sliding   :: forall r e a. Int -> RIO r e (Queue a)
```

**Implementation.** Variant of the existing offer policy: drop
the offered element / drop the oldest stored element / accept
unconditionally.

### #5 `FiberHandle` / `FiberSet`

**Status:** Shipped in `RIO.Fiber.FiberHandle` and
`RIO.Fiber.FiberSet`. Shipped shape diverges from the proposal:
`run` accepts `RIO r e a` (not `RIO () e a`) so the action is
not restricted to the empty environment. The original proposal
is preserved below.

**Problem.** Tracking N background fibers manually requires a
`Ref (Array Fiber)` + manual interrupt-on-exit logic. Easy to
get wrong.

**Shape.**

```purescript
-- FiberHandle: one fiber slot. Setting replaces the previous,
-- interrupting it. Scope-managed cleanup.
FiberHandle.make :: forall r e e' a. Scope -> RIO r e' (FiberHandle e a)
FiberHandle.run  :: forall r e a. FiberHandle e a -> RIO () e a -> RIO r e (Fiber e a)
FiberHandle.get  :: forall r e e' a. FiberHandle e a -> RIO r e' (Maybe (Fiber e a))

-- FiberSet: bag of fibers. All interrupted at scope close;
-- `awaitEmpty` for graceful drain.
FiberSet.make       :: forall r e e' a. Scope -> RIO r e' (FiberSet e a)
FiberSet.run        :: forall r e a. FiberSet e a -> RIO () e a -> RIO r e (Fiber e a)
FiberSet.awaitEmpty :: forall r e e' a. FiberSet e a -> RIO r e' Unit
FiberSet.size       :: forall r e e' a. FiberSet e a -> RIO r e' Int
```

**Implementation.** Pure library on `Ref`, `Scope`, `fork`, and
`addFinalizer`. Scope finalizer interrupts every tracked fiber.

### #6 Stream JS interop

**Problem.** Modern Node and browser APIs hand back
`AsyncIterable` / `ReadableStream`. No FFI to consume or
produce them.

**Shape.**

```purescript
Stream.fromAsyncIterable
  :: forall r e a. ForeignAsyncIterable a -> Stream r e a

Stream.fromReadableStream
  :: forall r e a. ForeignReadableStream a -> Stream r e a

Stream.toReadableStream
  :: forall r e a. Stream r e a -> RIO r e (ForeignReadableStream a)
```

**Implementation.** FFI bindings that drive the JS async
iteration protocol from `Stream.repeatRIO`-shaped machinery,
and the inverse using a `ReadableStream` controller.

### #7 Metrics labels + `Frequency` + `timer`

**Problem.** `Counter` / `Gauge` / `Histogram` have no label
support. Cannot be exported to Prometheus / DataDog
meaningfully. No `Frequency` (string-event counts). No `timer`
shorthand for "wrap an action, record its duration in a
histogram".

**Shape.**

```purescript
-- Carry a Map String String label set alongside each metric.
newCounter
  :: forall r e
   . { name :: String, labels :: Map String String }
  -> RIO r e Counter

-- Frequency: string event counts.
newFrequency :: forall r e. String -> RIO r e Frequency
observe      :: forall r e. Frequency -> String -> RIO r e Unit

-- Timer: histogram pre-wired for durations.
timer :: forall r e a. Histogram -> RIO r e a -> RIO r e a
```

**Implementation.** Change metric records to carry a label
map. Add `Frequency` as a `Ref (Map String Int)` wrapper.
`timer` is `currentEpoch` before/after + `Histogram.record`.

### #8 `Pool.invalidate` + `Pool.makeWithTTL`

**Status:** Shipped in `RIO.Fiber.Pool` (with the keyed variant
in `RIO.Fiber.KeyedPool`). Shipped shape diverges from the
proposal: `Pool` carries three type parameters (`Pool r e a`,
not `Pool a`); `makeWithTTL` takes a single `capacity` field
rather than separate `min` / `max` (no minimum size); and
`invalidate` is exposed as a per-borrow callback inside
`withResource'` rather than as a standalone `Pool -> a -> RIO`
function. The original proposal is preserved below.

**Problem.** A bad pooled resource (dead DB connection) lives
in the pool forever. No idle eviction; no min/max for dynamic
sizing.

**Shape.**

```purescript
Pool.invalidate :: forall r e a. Pool a -> a -> RIO r e Unit

Pool.makeWithTTL
  :: forall r e a
   . { min :: Int
     , max :: Int
     , timeToLive :: Milliseconds
     , create :: RIO r e a
     , destroy :: a -> RIO r e Unit
     }
  -> RIO r e (Pool a)
```

**Implementation.** `invalidate` removes from free queue,
triggers re-create on next borrow. TTL pool needs a janitor
fiber that walks the free queue every tick and evicts idle
resources past their TTL.

### #9 Logger annotations + JSON formatter

**Problem.** Logger has no contextual annotations
(`request-id`, `user-id`) that propagate to child fibers. No
structured output format.

**Shape.**

```purescript
withLogAnnotation
  :: forall r e a. String -> String -> RIO r e a -> RIO r e a

jsonLogger :: Logger
```

**Implementation.** Annotation store in a `FiberRef (Map
String String)` so it propagates through fork (copy-on-write).
`jsonLogger` is a pure formatter over the log-entry record.

### #10 Stream `peel` / `transduce` / `changes`

**Status:** Shipped in `RIO.Fiber.Stream` as `peel`, `transduce`,
and `changes`. The original proposal is preserved below.

**Problem.** Three commonly-needed Stream operators are absent.

- `peel sink stream`: run `sink` on a prefix of `stream`, return
  the Sink's result plus the remaining Stream.
- `transduce sink stream`: apply a Sink as a stateful
  transducer, emitting each Sink result and flushing on stream
  end.
- `changes`: filter out consecutive duplicate elements.

**Shape.**

```purescript
Stream.peel
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> RIO r e (Tuple b (Stream r e a))

Stream.transduce
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> Stream r e b

Stream.changes
  :: forall r e a
   . Eq a
  => Stream r e a
  -> Stream r e a
```

**Implementation.** All library-only. `peel` runs the sink loop
until it returns Done, then returns the carried-over input
buffer + the rest of the upstream as a fresh Stream. `transduce`
is `peel` in a loop, emitting each result. `changes` is `scan`
+ `filter`.

## Runtime-level items (separate effort)

These need interpreter changes, not just library additions.

- **`uninterruptibleMask` with `restore`.** Medium effort,
  high value. Currently `uninterruptible` is all-or-nothing.
  Without `restore` you can't write the standard "hold lock,
  release safely, *then* allow interrupt" pattern.
  Implementation: interruptibility-mask stack on the fiber +
  per-frame restore op.
- **`forkDaemon`.** Small effort. Needs a module-level root
  scope; then it's `forkScoped rootScope`. Useful for
  background services that must outlive the parent.
- **`yieldNow`.** Small effort, low value. Cooperative yielding
  is already automatic via the tick budget. Occasionally useful
  for long pure loops.

## Things we already do better than Effect-TS

Notes for documentation and positioning. These are
intentional differentiators worth preserving.

- **Typed error rows.** `Variant`-based `e` row gives row-level
  narrowing (`catchTag` removes one tag; the remaining row is
  exact). Effect's single `E` parameter relies on union
  exhaustion.
- **`forkInline` / `forkAllInline`.** No Effect-TS equivalent.
  Sync-bodied children skip two microtask hops per fork-join.
- **`TestClock` as a first-class module.** Effect's `TestClock`
  lives in `@effect/vitest`. Ours is core, framework-agnostic.
- **`CircuitBreaker` + `RateLimiter` in core.** Effect-TS ships
  these only as community packages.
- **`Config.Rotating`.** Built for credential rotation.
  Effect-TS's Config is load-once.
- **`Cause.linearize` / `squash`.** Richer cause-tree
  manipulation than `Cause.pretty` alone.

## Deliberately not worth porting

- **`Channel` (six type parameters).** We have `Pipe` for the
  stream-transducer case. Channel's bidirectionality is
  necessary in TypeScript because of type-system constraints
  PureScript doesn't share.
- **`Effect.gen` generator syntax.** PureScript has native
  do-notation.
- **`ManagedRuntime`.** Our `Layer` + record-row environment
  already does this.
- **`Micro` (minimal Effect subset).** Our runtime is already
  minimal; PureScript's DCE handles bundle size.
- **`FiberRef` with custom fork / join / delete functions.**
  Adds reasoning complexity for ~5% of use cases that can be
  done with explicit state.

## Suggested batching

**Batch 1 (done).** Items #1, #2, #4, #5, #10. All S or
small-M. Share no interpreter changes. Together they close
most of the everyday-ergonomics gap.

**Batch 2 (done; #6 partial).** Items #3 (`Mailbox`) and #6
(streaming surface; `fromAsyncIterable` shipped, `ReadableStream`
interop pending).

**Batch 3 (#8 done; #7 and #9 still open).** Items #7
(metrics labels / `Frequency` / `timer`), #8
(`Pool.invalidate` / `Pool.makeWithTTL`, shipped), #9
(logger annotations / JSON formatter, still open).

**Batch 4 (separate effort).** Runtime-level items
(`uninterruptibleMask`, `forkDaemon`).

## Second pass: additional gaps surfaced by a deeper audit

The first pass focused on the marquee combinators. A re-read of
the actual source surface against Effect-TS turned up these
additional items, not covered above. Same ranking convention
(value-per-effort, high first).

| # | Item | Effort | Value | Status |
|---|---|---|---|---|
| 11 | Bounded-concurrency `forEachParN` / `parTraverseN` | S | High | done |
| 12 | `Stream.aggregate` / `aggregateWithin` | M | High | done |
| 13 | `Tracer` span events / links / status / kind | S | High | done |
| 14 | `Histogram` with configurable bucket boundaries | M | High | done |
| 15 | `Stream.async` / `Stream.fromCallback` | S | High | done |
| 16 | `FiberMap` (keyed) | S | Med | open |
| 17 | `Queue.shutdown` / `isShutdown` / `takeAll` / `takeUpTo` | S | Med | open |
| 18 | `SubscriptionRef` | S | Med | done |
| 19 | `Logger.batched` / `Logger.tagged` / `Logger.json` | S | Med | open |
| 20 | `Stream.toQueue` / `Stream.toHub` / `Stream.groupAdjacent` | S | Med | done |
| 21 | `Schedule.recurUpTo` / `Schedule.tap` | S | Low-Med | open |
| 22 | `partition` (separate successes from typed failures) | S | Low-Med | done |
| 23 | `Config.array` / `Config.json` | S | Med | open |
| 24 | `timed` / `never` / `disconnect` | S | Low | partial |

### #11 Bounded-concurrency `forEachParN` / `parTraverseN`

**Status:** Shipped in `RIO.Fiber.Semaphore` and re-exported via
`RIO.Fiber.Concurrency` as `parTraverseN`. Only the
`parTraverseN` name shipped; `forEachParN` was not added (the
two were proposed as aliases). The original proposal is
preserved below.

**Problem.** `Core.parTraverse` runs every element in parallel
with no cap. Real workloads doing fan-out (HTTP, DB queries,
file IO) need "do these 10000 things, 50 at a time" or memory
and downstream services collapse. Currently the caller has to
wire a `Semaphore` and `acquireN` / `releaseN` around every
task by hand.

**Shape.**

```purescript
forEachParN
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Array a
  -> RIO r e (Array b)

parTraverseN
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Array a
  -> RIO r e (Array b)
```

**Implementation.** Allocate a `Semaphore n` once; wrap each
task in `Semaphore.withPermit`; delegate to `parTraverse`. The
cap clamps to `max 1` so `forEachParN 0` is sequential rather
than a deadlock.

### #12 `Stream.aggregate` / `aggregateWithin`

**Status:** Shipped in `RIO.Fiber.Stream`. Shipped shape diverges
from the proposal: `aggregateWithin` takes a `Milliseconds`
timeout as its first parameter rather than a full `Schedule a c`.
The shipped signature is
`Milliseconds -> Sink r e a b -> Stream r e a -> Stream r e b`
(no `c` type variable; fixed-interval window only). A
schedule-driven variant could be added later if a use case
appears. The original proposal is preserved below.

**Problem.** Micro-batching is the dominant pattern in log
shippers, metric exporters, and event bridges: emit a batch
when N items have arrived OR after T elapsed. We have `Sink`
combinators that can build the batch, but no Stream operator
that drives them on a clock.

**Shape.**

```purescript
Stream.aggregate
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> Stream r e b

Stream.aggregateWithin
  :: forall r e a b c
   . Sink r e a b
  -> Schedule a c
  -> Stream r e a
  -> Stream r e b
```

**Implementation.** Library-only. `aggregate` is `transduce`
with the sink restarted between emissions; `aggregateWithin`
runs the upstream pull and the schedule decision in parallel,
emitting whichever completes first.

### #13 `Tracer` span events / links / status / kind

**Status.** Done. `Span` now carries `addEvent`, `addLink`,
`setStatus`, and the recording / OTel adapters forward each
through. `withSpan` defaults to `Internal`; the smart constructor
that takes an explicit `SpanKind` shipped as `withSpanWith` (the
Implementation paragraph below calls it `withSpanKind`, which was
the proposed name). A stable `spanId :: SpanId` field is exposed
on `Span` itself so external integrations can correlate spans by
id; it is the first field of the shipped record (the Shape block
below preserves the original proposal, which omitted it).

**Problem (resolved).** Previously `RIO.Fiber.Tracer.Span`
exposed only `addAttribute :: String -> String -> Effect Unit`
and `finish`. OTel spans also carry: timestamped events
(`addEvent`), cross-trace links (`addLink`), an explicit
status (`Ok`, `Error`), and a `SpanKind` (Server / Client /
Producer / Consumer / Internal). Without these, the
`rio-fiber-otel` adapter exported fidelity-lossy spans.

**Shape.**

```purescript
data SpanKind = Internal | Server | Client | Producer | Consumer
data SpanStatus = StatusUnset | StatusOk | StatusError String

newtype Span = Span
  { addAttribute :: String -> String -> Effect Unit
  , addEvent     :: String -> Array Attr -> Effect Unit
  , addLink      :: Span -> Effect Unit
  , setStatus    :: SpanStatus -> Effect Unit
  , finish       :: Effect Unit
  }

type StartSpanRequest =
  { name :: String
  , attributes :: Array Attr
  , parent :: Maybe Span
  , kind :: SpanKind
  }
```

**Implementation.** Extend the `Span` record. `defaultTracer`
no-ops the new fields. `rio-fiber-otel/Adapter` forwards each
to the OTel API. `withSpan` learns a `kind` parameter (default
`Internal`); existing call sites are source-compatible because
we provide a `withSpanKind` smart constructor that takes the
old shape.

### #14 `Histogram` with configurable bucket boundaries

**Status:** Shipped in `RIO.Fiber.Metrics`. Shipped names diverge
from the proposal: the type is `BucketHistogram` (not
`Histogram`), the constructor is `newBucketHistogram`
(not `newHistogramWithBoundaries`), the snapshot function is
`bucketSnapshot` (not `histogramSnapshot`), and the snapshot is
`RIO r e BucketSnapshot` (not `Effect _`) where
`type BucketSnapshot = { count :: Int, sum :: Number, buckets ::
Array Bucket }` and `type Bucket = { le :: Number, count :: Int }`.
The original proposal is preserved below.

**Problem.** `Metrics.Histogram` keeps a rolling `Array Number`
of samples and computes p50/p95 in-process. Prometheus and OTel
histogram exporters need bucket counts (`le=0.1, le=0.25, ...`),
not raw samples. Today an exporter can only emit the in-process
summary, which loses information across the wire.

**Shape.**

```purescript
data BucketLayout
  = Linear { start :: Number, width :: Number, count :: Int }
  | Exponential { start :: Number, factor :: Number, count :: Int }
  | Explicit (Array Number)

newHistogramWithBoundaries :: BucketLayout -> Effect Histogram

histogramSnapshot
  :: Histogram
  -> Effect
       { count :: Int
       , sum :: Number
       , buckets :: Array { le :: Number, count :: Int }
       }
```

**Implementation.** Internally store an `Array Int` of bucket
counts plus `count` and `sum`. `record` does a single
`findIndex (x <=)` over the sorted boundaries. The existing
summary-based variant stays as `newHistogram` for in-process
use; the new bucket-based variant is what exporters consume.

### #15 `Stream.async` / `Stream.fromCallback`

**Status:** Shipped in `RIO.Fiber.Stream`. Shipped shape diverges
from the proposal: `async` takes a leading `Scope` parameter
(for finalizer registration) and the user-supplied callback
receives `Emit e a -> Effect Unit` rather than `a -> Effect Unit`,
where `Emit e a` carries typed-error signalling alongside the
value. The shipped signature is
`Scope -> ((Emit e a -> Effect Unit) -> Effect (Effect Unit)) ->
Stream r e a`. The original proposal is preserved below.

**Problem.** Lifting a callback-style API (websocket
`onMessage`, `EventEmitter`, message-port, Node IPC) into a
Stream today means manually writing `Queue.make` + `offer` +
sentinel + cleanup. We do this in `rio-fiber-postgres` for
LISTEN/NOTIFY, in `rio-fiber-node/Http2` for incoming streams,
and at every websocket integration. Standard pattern, ought to
be a library combinator.

**Shape.**

```purescript
Stream.async
  :: forall r e a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Stream r e a
```

**Implementation.** Allocates a Queue, wires `offer` to the
user-supplied callback, registers cleanup as a Stream
`acquireRelease` step.

### #16 `FiberMap` (keyed)

**Problem.** We added `FiberHandle` (one slot) and `FiberSet`
(unkeyed bag) in Batch 1. Effect ships a third member of the
family: `FiberMap`, a `k -> Fiber` mapping where `set k f`
replaces (and interrupts) the previous occupant at that key. The
canonical use is "one background fiber per session" or "one
background fiber per topic"; doing this today is `Ref (Map k
Fiber)` plus the same lifecycle code we wrote for
`FiberHandle`.

**Shape.**

```purescript
FiberMap.make :: forall r e e' k a. Scope -> RIO r e' (FiberMap k e a)
FiberMap.run  :: forall r e k a. Ord k => FiberMap k e a -> k -> RIO r e a -> RIO r e (Fiber e a)
FiberMap.get  :: forall r e e' k a. Ord k => FiberMap k e a -> k -> RIO r e' (Maybe (Fiber e a))
FiberMap.size :: forall r e e' k a. FiberMap k e a -> RIO r e' Int
```

**Implementation.** Reuse the generation-counter trick from
`FiberHandle`, indexed by `Map k`.

### #17 `Queue.shutdown` / `isShutdown` / `takeAll` / `takeUpTo`

**Status:** Partial. `takeAll` and `takeUpTo` shipped in
`RIO.Fiber.Queue`. The shipped `takeUpTo` signature is
`Queue a -> Int -> RIO r e (Array a)` (Queue first, Int second);
the Shape block below has the parameter order reversed.
`shutdown` and `isShutdown` are not yet implemented; the
original proposal is preserved below.

**Problem.** A queue has no lifecycle signal: consumers
`take`-ing from a finished producer block forever. Bulk takes
are also missing, so batching consumers loop on `take` in
tight N+1 patterns. Note `Mailbox` (#3) wraps these concepts in a
higher-level abstraction; this item adds them at the `Queue`
primitive level too.

**Shape.**

```purescript
Queue.shutdown   :: forall r e a. Queue a -> RIO r e Unit
Queue.isShutdown :: forall r e a. Queue a -> RIO r e Boolean
Queue.takeAll    :: forall r e a. Queue a -> RIO r e (Array a)
Queue.takeUpTo   :: forall r e a. Int -> Queue a -> RIO r e (Array a)
```

**Implementation.** Add `shutdown :: Boolean` to the queue
state. `take` on a shutdown queue with no items returns the
fiber via interrupt; offers are rejected. `takeAll`/`takeUpTo`
drain greedily.

### #18 `SubscriptionRef`

**Status:** Shipped in `RIO.Fiber.Ref.Subscription`. Shipped
shape diverges from the proposal: `set` in place of the
originally-proposed `write`; `make :: Int -> a -> RIO r e
(SubscriptionRef a)` takes a leading buffer-capacity argument;
`changes :: Scope -> SubscriptionRef a -> Stream r e a` takes
an explicit `Scope` for the hub subscription. The
implementation pairs a plain `Effect.Ref` under a `Semaphore`
with a `Hub`, rather than wrapping a `Synchronized.Ref` as the
proposal sketched (behaviour is equivalent). The original
proposal is preserved below.

**Problem.** Many state-management flows want "a `Ref` whose
changes you can subscribe to as a Stream": server-side cache
state, UI store, derived computation. Today you build it from
`Ref` + `Hub` by hand.

**Shape.**

```purescript
SubscriptionRef.make    :: forall r e a. a -> RIO r e (SubscriptionRef a)
SubscriptionRef.read    :: forall r e a. SubscriptionRef a -> RIO r e a
SubscriptionRef.write   :: forall r e a. SubscriptionRef a -> a -> RIO r e Unit
SubscriptionRef.update  :: forall r e a. SubscriptionRef a -> (a -> a) -> RIO r e Unit
SubscriptionRef.changes :: forall r e a. SubscriptionRef a -> Stream r e a
```

**Implementation.** A `Synchronized.Ref a` paired with a
`Hub a`; `write` updates the ref and publishes; `changes`
returns a Stream backed by `Hub.subscribe`. First element on a
fresh subscription is the current value (so consumers don't
race the producer).

### #19 `Logger.batched` / `Logger.tagged` / `Logger.json`

**Problem.** `defaultLogger` writes one console line at a time
with no structure. Real deployments want JSON output (for log
ingesters), a fixed tag prefix (for service identification),
and time- or count-window batching (for HTTP-based log
shippers like Datadog or Cloud Logging).

**Shape.**

```purescript
Logger.json      :: Logger
Logger.tagged    :: String -> Logger -> Logger
Logger.batched
  :: forall r e
   . { flushEvery :: Milliseconds
     , maxBatch :: Int
     , write :: Array LogEntry -> RIO r e Unit
     }
  -> RIO r e Logger
```

**Implementation.** `json` is a pure formatter over the
existing log-entry record. `tagged` wraps `emit`. `batched`
allocates a queue and a fiber that flushes on the schedule;
`emit` enqueues. Scope-bound so the flushing fiber is
interrupted on shutdown.

### #20 `Stream.toQueue` / `Stream.toHub` / `Stream.groupAdjacent`

**Status:** Shipped in `RIO.Fiber.Stream`. `toQueue` and `toHub`
shipped with the proposed signatures; `groupAdjacent` shipped
under the name `groupBy` with the same signature. The original
proposal is preserved below.

**Problem.** We have `Stream.fromQueue` and `Stream.fromTQueue`
but no inverse. Common case: a stream-shaped producer needs to
hand off to a fan-out via `Hub`, or to a bounded backlog via
`Queue`. `groupAdjacent` chunks the stream by a key function
("group consecutive entries with the same trace-id").

**Shape.**

```purescript
Stream.toQueue       :: forall r e a. Queue a -> Stream r e a -> RIO r e Unit
Stream.toHub         :: forall r e a. Hub a -> Stream r e a -> RIO r e Unit
Stream.groupAdjacent :: forall r e a k. Eq k => (a -> k) -> Stream r e a -> Stream r e (Array a)
```

**Implementation.** Library-only; `toQueue` is `forEach
(offer q)`, `toHub` is `forEach (publish h)`, `groupAdjacent`
is a stateful fold that emits when the key changes.

### #21 `Schedule.recurUpTo` / `Schedule.tap`

**Problem.** Schedule has `recurs n` (max N tries) and
`spaced d` (every D), but no "for up to T total elapsed". And
no observability hooks for retry/repeat policies, so you
can't log "tries before success" without writing your own
counter.

**Shape.**

```purescript
Schedule.recurUpTo :: forall a. Milliseconds -> Schedule a Int
Schedule.tap
  :: forall a b
   . (a -> b -> Effect Unit)
  -> Schedule a b
  -> Schedule a b
```

**Implementation.** Library-only. `recurUpTo` carries the
deadline in its state; `tap` inserts a side-effect at each
decision.

### #22 `partition`

**Status:** Shipped in `RIO.Fiber.Core` as `partition`. The
original proposal is preserved below.

**Problem.** `parTraverse` short-circuits on the first failure;
`validatePar` accumulates failures but still fails the whole
computation if any element failed. Neither suits "best effort:
run all 100, return the 87 that worked and the 13 that didn't"
semantics, which are useful for backfills, sync jobs, and
partial-failure reporting.

**Shape.**

```purescript
partition
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' { failures :: Array (Variant e), successes :: Array b }
```

**Implementation.** `parTraverse (causeOf <<< f)` + partition
on the result. Loses the `Cause` shape (defects/interrupts are
collapsed to failures); a richer variant could return `Array
(Cause e)` if needed.

### #23 `Config.array` / `Config.json`

**Problem.** `Config` only has primitives (`string`, `int`,
`boolean`) plus `nested` and `secret`. No way to read a list
from `FEATURE_FLAGS=alpha,beta,gamma` or a JSON blob from
`SERVICE_CONFIG={"limit":100}`. Real apps work around this by
parsing strings manually.

**Shape.**

```purescript
Config.array :: forall a. String -> Config a -> Config (Array a)
Config.json  :: forall a. (Json -> Either String a) -> Config a
```

**Implementation.** `array` reads a separator-delimited string
and applies the inner config to each element; `json` reads a
string and applies a user-provided decoder.

### #24 `timed` / `never` / `disconnect`

**Status:** Partial. `timed` and `never` shipped in
`RIO.Fiber.Core` with the proposed signatures; `disconnect` is
not yet implemented. The original proposal is preserved below.

Tiny convenience trio.

```purescript
timed   :: forall r e a. RIO r e a -> RIO r e (Tuple Milliseconds a)
never   :: forall r e a. RIO r e a
disconnect :: forall r e a. RIO r e a -> RIO r e (Fiber e a)
```

`timed` records `currentEpoch` before/after. `never` is `async
\_ -> pure (pure unit)`. `disconnect` forks onto a fresh root
fiber that is NOT a child of the caller (no parent-interrupt
propagation, no scope inheritance).

## Suggested batching, updated

**Batch 1 (done).** Items #1, #2, #4, #5, #10.

**Batch 1.5 (done).** Items #11, #12, #13, #14, #15. Closed
the bounded-concurrency footgun, the micro-batching gap, the
OTel-span fidelity gap, the histogram-export gap, and the
stream-async constructor gap.

**Batch 2 (#3 and #20 done; #6 partial).** Item #3
(`Mailbox`) shipped; item #20 (stream-queue / hub adapters)
shipped (`groupAdjacent` shipped under the name `groupBy`); item
#6 partially shipped (`fromAsyncIterable` done, `ReadableStream`
interop pending). #15 already landed in Batch 1.5.

**Batch 3 (#8 and #18 done; the rest still open).** Item #8
(`Pool.invalidate` / `Pool.makeWithTTL`) and item #18
(`SubscriptionRef`) shipped. Items #7 (metrics labels /
`Frequency` / `timer`), #9 (logger JSON formatter),
#17 (`Queue.shutdown` / `takeUpTo`), and #19 (`Logger.batched`
/ `tagged` / `json`) still open.

**Batch 4 (separate effort).** Runtime-level items
(`uninterruptibleMask`, `forkDaemon`).

**Backlog.** Items #16, #21, #23, #24. (Item #22 `partition`
shipped.)

## Third pass: ergonomics and surface depth

The first two passes targeted marquee combinators and the
biggest documented gaps. A third read of the actual module
exports against Effect-TS's surface turned up ergonomics
combinators, Layer composition primitives, STM atomic primitives,
and stream/sink/random/scope/schedule polish that genuinely
improves day-to-day code. Same ranking convention.

| # | Item | Effort | Value | Status |
|---|---|---|---|---|
| 25 | RIO ergonomics: `tap` / `tapError` / `tapBoth` / `mapError` / `orElse` / `orDie` / `either` | S | High | done |
| 26 | `Layer.scoped` / `Layer.memoize` / `Layer.fresh` | M | High | done |
| 27 | STM `TSemaphore` / `TMap` / `TDeferred` | M | High | done |
| 28 | Stream constructors: `tick` / `range` / `iterate` / `unfold` / `haltWhen` | S | Med-High | done |
| 29 | Stream ergonomics: `tap` / `tapError` / `drop` / `dropWhile` / `mapRIOPar` | S | Med | done |
| 30 | `Random.shuffle` / `choice` / `uuid` / `bytes` | S | Med | done |
| 31 | `Scope.addFinalizerExit` (Cause-aware finalizers) | M | Med | done |
| 32 | `Sink.takeWhile` / `dropWhile` / `mkString` / `findRIO` | S | Med | done |
| 33 | `Schedule.compose` / `mapInput` / `passthrough` / `elapsed` / `delays` | S | Low-Med | done |
| 34 | `whenRIO` / `unlessRIO` / `iterate` / `loop` / `Stream.partitioned` | S | Low | done |
| 35 | `Cause.find` / `contains` (annotate deferred) | S | Low | done (partial) |
| 36 | `Fiber.poll` (non-blocking outcome check) | S | Low | done |

### #25 RIO ergonomic combinators

**Status (revised after audit).** The error-handling family
(`tap`, `tapError`, `tapBoth`, `tapErrorCause`, `mapError`,
`orElse`, `orDie`, `either`) is already provided by
`RIO.Fiber.Error`, along with extras effect-ts doesn't have
(`absolve`, `attemptCause`, `catchAllCause`, `catchSome`,
`catchSomeCause`, `catchTag`, `foldCauseRIO`, `foldRIO`,
`fromEither`, `fromMaybe`, `mapBoth`, `option`, `orElseFail`,
`orElseSucceed`, `refineOrDie`, `refineOrDieWith`, `rethrow`,
`tapDefectCause`). The original entry was a false positive.

What was genuinely missing and has now landed in
`RIO.Fiber.Core`:

```purescript
poll      :: forall r e a. Fiber e a -> RIO r e (Maybe (Outcome e a))
whenRIO   :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
unlessRIO :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
iterate   :: forall r e a. a -> (a -> Boolean) -> (a -> RIO r e a) -> RIO r e a
loop      :: forall r e s a. s -> (s -> Boolean) -> (s -> s) -> (s -> RIO r e a) -> RIO r e (Array a)
```

`poll` reads `Internal.fiberIsDone` + `Internal.fiberOutcome`
without blocking. `whenRIO` / `unlessRIO` gate side-effects on
a `RIO r e Boolean` condition. `iterate` is a tail-recursive
while-style loop returning the final state; `loop` collects
the per-step body results into an array. All library-only, no
interpreter changes.

### #26 `Layer.scoped` / `Layer.memoize` / `Layer.fresh`

**Status:** Shipped in `RIO.Fiber.Layer`. One shape note:
`fresh` is literally `identity` (source: `rio-fiber/src/RIO/Fiber/
Layer.purs:232`). Layers in the current model are already
rebuilt on every `provide`, so there is no implicit sharing
for `fresh` to defeat; the combinator is kept for compositional
symmetry with `memoize` and as a forward-compatible marker if
implicit sharing is ever added. The original proposal is
preserved below.

**Problem.** Our `Layer` surface (`fromValue`, `fromRIO`,
`chainLayer`, `mergeLayers`, `provide`, `provideScoped`)
handles flat composition but not resource lifecycle or
sharing. A layer that needs a finalizer (DB pool, HTTP
client, file handle) can only be wired via `provideScoped`,
which breaks `chainLayer` composition. A layer used twice in
a `chainLayer` chain is allocated twice instead of shared.

**Shape.**

```purescript
Layer.scoped
  :: forall e rIn rOut
   . (Scope -> RIO rIn e (Record rOut))
  -> Layer e rIn rOut

Layer.memoize
  :: forall e rIn rOut
   . Layer e rIn rOut
  -> Effect (Layer e rIn rOut)

Layer.fresh
  :: forall e rIn rOut
   . Layer e rIn rOut
  -> Layer e rIn rOut
```

**Implementation.** `scoped` wraps `provideScoped` semantics
into a Layer-shaped value by carrying the scope inside its
build action. `memoize` allocates a once-cell (Ref + Deferred)
in Effect; the inner build uses the cell. `fresh` is a marker
that prevents `memoize`'s sharing.

### #27 STM atomic primitives

**Status.** Done. `RIO.Fiber.STM.TSemaphore`,
`RIO.Fiber.STM.TMap`, and `RIO.Fiber.STM.TDeferred` ship as
the compositionally-atomic counterparts to the existing
`TVar` / `TArray` / `TChan` / `TMVar` / `TQueue`. `TSet` and
`TPubSub` landed alongside. Two shape divergences from the
proposal below: `TMap.get` / `TMap.put` shipped as `TMap.lookup`
/ `TMap.insert` (matching `Data.Map`), and `TDeferred.make`
shipped as `Effect (TDeferred a)` rather than `STM (TDeferred a)`
(it allocates a fresh `TVar`, which is `Effect` outside a
transaction; a separate `STM`-valued allocator was not deemed
necessary).

**Problem (resolved).** Previously the STM surface had `TVar`,
`TArray`, `TChan`, `TMVar`, and `TQueue` only. Missing was
the trio that makes STM *compositionally atomic*: a
semaphore-typed acquire/release that participates in
transactions, a keyed atomic map, and an STM one-shot
deferred. The regular `Semaphore` lives outside STM, so
acquire-then-check patterns had race windows that didn't roll
back.

**Shape.**

```purescript
-- TSemaphore
TSemaphore.make      :: Int -> Effect TSemaphore
TSemaphore.acquire   :: TSemaphore -> STM Unit
TSemaphore.release   :: TSemaphore -> STM Unit
TSemaphore.acquireN  :: Int -> TSemaphore -> STM Unit
TSemaphore.releaseN  :: Int -> TSemaphore -> STM Unit
TSemaphore.available :: TSemaphore -> STM Int

-- TMap
TMap.empty   :: forall k v. Effect (TMap k v)
TMap.get     :: forall k v. Ord k => k -> TMap k v -> STM (Maybe v)
TMap.put     :: forall k v. Ord k => k -> v -> TMap k v -> STM Unit
TMap.delete  :: forall k v. Ord k => k -> TMap k v -> STM Unit
TMap.size    :: forall k v. TMap k v -> STM Int
TMap.toArray :: forall k v. TMap k v -> STM (Array (Tuple k v))

-- TDeferred
TDeferred.make     :: forall a. STM (TDeferred a)
TDeferred.complete :: forall a. TDeferred a -> a -> STM Boolean
TDeferred.await    :: forall a. TDeferred a -> STM a
TDeferred.poll     :: forall a. TDeferred a -> STM (Maybe a)
```

**Implementation.** All library on top of `TVar` + `retry` +
`orElse`. `TSemaphore` is a `TVar Int`; acquire retries when
the count is below the request. `TMap` is `TVar (Map k v)`
plus a per-key `TVar` ladder; for our scale a flat
`TVar (Map k v)` is enough. `TDeferred` is `TVar (Maybe a)`
where `await` retries while empty.

### #28 Stream constructors

**Status.** Shipped in `RIO.Fiber.Stream`. Shipped shape diverges
from the proposal: `haltWhen` accepts any `RIO r e x` sentinel
(not specifically a `Deferred () Unit`), which lets it compose
with arbitrary async signals (a `Deferred` await, a queue take,
a fiber join, etc.). The original proposal is preserved below.

**Problem.** Common stream generators are missing. `Stream.tick`
is the canonical "every D milliseconds, emit unit" used by
metric exporters and health-check loops. `range`, `iterate`,
and `unfold` are the natural finite/infinite generators.
`haltWhen` lets a stream terminate on an external signal
(graceful shutdown).

**Shape.**

```purescript
Stream.tick     :: forall r e. Milliseconds -> Stream r e Unit
Stream.range    :: forall r e. Int -> Int -> Stream r e Int
Stream.iterate  :: forall r e a. a -> (a -> a) -> Stream r e a
Stream.unfold   :: forall r e s a. s -> (s -> Maybe (Tuple a s)) -> Stream r e a
Stream.haltWhen :: forall r e a. Deferred () Unit -> Stream r e a -> Stream r e a
```

**Implementation.** Library-only on top of existing Stream
primitives. `tick` is `repeatRIO (sleep d)`; `haltWhen` polls
the Deferred at each pull and emits `Done` once filled.

### #29 Stream ergonomics

**Problem.** Stream-side counterparts to the RIO combinators
in #25, plus `drop`/`dropWhile` (we have `take` but no drop)
and `mapRIOPar` (concurrent map without preserving order,
faster than `mapPar` when downstream is order-insensitive).

**Shape.**

```purescript
Stream.tap       :: forall r e a. (a -> RIO r e Unit) -> Stream r e a -> Stream r e a
Stream.tapError  :: forall r e a. (Variant e -> RIO r e Unit) -> Stream r e a -> Stream r e a
Stream.drop      :: forall r e a. Int -> Stream r e a -> Stream r e a
Stream.dropWhile :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
Stream.mapRIOPar :: forall r e a b. Int -> (a -> RIO r e b) -> Stream r e a -> Stream r e b
```

**Implementation.** `tap`/`tapError` ride on `mapRIO`/`catchAll`.
`drop`/`dropWhile` are simple stateful folds. `mapRIOPar`
spins a queue of permits via `Semaphore.parTraverseN`-style
worker dispatch; differs from existing `mapPar` (round-robin,
order-preserving) by not buffering for reorder.

### #30 `Random` depth

**Problem.** `Random` exposes `nextNumber`/`nextInt`/
`nextBoolean` and stops. Real callers want `shuffle` (random
permutation), `choice` (pick an array element), `uuid` (v4 for
correlation IDs), and `bytes` (random `Uint8Array` for nonces,
salts).

**Shape.**

```purescript
Random.shuffle :: forall r e a. Array a -> RIO r e (Array a)
Random.choice  :: forall r e a. Array a -> RIO r e (Maybe a)
Random.uuid    :: forall r e. RIO r e String
Random.bytes   :: forall r e. Int -> RIO r e (Array Int)
```

**Implementation.** `shuffle` is Fisher–Yates on top of
`nextInt`. `choice` is `nextInt 0 (length - 1)` + `index`.
`uuid` is FFI to `crypto.randomUUID()` (Node 14.17+, browsers).
`bytes` is FFI to `crypto.getRandomValues` / `randomBytes`.

### #31 `Scope.addFinalizerExit`

**Problem.** `addFinalizer :: Scope -> Effect Unit -> RIO r e
Unit` runs the finalizer on scope close, but the finalizer
can't see *why* the scope closed (success / typed fail /
defect / interrupt). The standard pattern "close the DB
connection cleanly on success, force-abort on interrupt"
requires reading the closing Cause.

**Shape.**

```purescript
Scope.addFinalizerExit
  :: forall r e
   . Scope
  -> (Maybe (Cause e) -> Effect Unit)
  -> RIO r e Unit
```

**Implementation.** Extend the Scope's finalizer list to
carry a `Maybe (Cause e) -> Effect Unit` variant alongside
the plain `Effect Unit` variant; `closeScope` is extended to
take a `Maybe (Cause e)` and thread it to Exit-aware
finalizers. Existing `addFinalizer` keeps its signature and
ignores the Cause.

### #32 `Sink` primitives

**Problem.** `Sink` exports `count`, `sum`, `collectAll`,
`head`, `last`, `drain`, `foreach`, `fold`, `foldRIO`,
`foldUntil`, `takeN`. Missing predicate-driven take/drop
variants (`takeWhile`, `dropWhile`), the string accumulator
(`mkString` with a separator), and the effectful find
(`findRIO`, the RIO-suffixed sibling of `find`).

**Shape.**

```purescript
Sink.takeWhile  :: forall r e a. (a -> Boolean) -> Sink r e a (Array a)
Sink.dropWhile  :: forall r e a. (a -> Boolean) -> Sink r e a (Array a)
Sink.mkString   :: forall r e. String -> Sink r e String String
Sink.findRIO    :: forall r e a. (a -> RIO r e Boolean) -> Sink r e a (Maybe a)
```

**Implementation.** Sink-state recursion; `mkString` is fold
over `(<>)` with separator handling.

### #33 `Schedule` polish

**Problem.** Our Schedule has the major shapes (`recurs`,
`spaced`, `exponential`, `fibonacci`, `jittered`, `andThen`,
`bothS`, `mapOutput`, `whileInput/Output`, `untilInput/Output`).
Missing: `compose` (`>>>`-style composition of schedules),
`mapInput` (mirror of `mapOutput`), `passthrough` (identity
schedule returning input), `elapsed` (cumulative elapsed time),
`delays` (output the timing sequence).

**Shape.**

```purescript
Schedule.compose     :: forall a b c. Schedule a b -> Schedule b c -> Schedule a c
Schedule.mapInput    :: forall a a' b. (a' -> a) -> Schedule a b -> Schedule a' b
Schedule.passthrough :: forall a. Schedule a a
Schedule.elapsed     :: forall a. Schedule a Milliseconds
Schedule.delays      :: forall a b. Schedule a b -> Schedule a Milliseconds
```

**Implementation.** All library-only; thread the output of
the first schedule into the input of the second for `compose`;
`elapsed` accumulates the `Milliseconds` of each `Step`.

### #34 Conditional execution + `Stream.partitioned`

**Status:** Shipped in `RIO.Fiber.Core` (`whenRIO` / `unlessRIO` /
`iterate` / `loop`) and `RIO.Fiber.Stream` (`partitioned`).
Shipped shape diverges for `partitioned`: the return type is
`RIO r e { yes :: Stream r e a, no :: Stream r e a }` (not a bare
record), because it allocates two queues and forks a producer.
The original proposal is preserved below.

**Problem.** `when`/`unless` lifted to take an RIO-valued
condition, explicit looping combinators (`iterate`, `loop`)
that some idioms prefer, and `Stream.partitioned` (split a
stream by predicate into two output streams).

**Shape.**

```purescript
whenRIO   :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
unlessRIO :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
iterate   :: forall r e a. a -> (a -> Boolean) -> (a -> RIO r e a) -> RIO r e a
loop      :: forall r e s a. s -> (s -> Boolean) -> (s -> s) -> (s -> RIO r e a) -> RIO r e (Array a)
Stream.partitioned
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> { yes :: Stream r e a, no :: Stream r e a }
```

**Implementation.** All library combinators.

### #35 `Cause` search (`find` / `contains`)

**Problem.** Observability sometimes needs to search a
`Cause` tree for a sub-cause matching a predicate (does it
contain an interrupt anywhere? a particular tagged failure?).

**Shape.**

```purescript
Cause.find     :: forall e. (Cause e -> Boolean) -> Cause e -> Maybe (Cause e)
Cause.contains :: forall e. (Cause e -> Boolean) -> Cause e -> Boolean
```

**Implementation.** Pure left-to-right tree traversals; if
the predicate matches the current node, return it, otherwise
recurse into `Then` / `Both` children. Pure library code, no
ADT changes.

**Deferred: `Cause.annotate`.** Effect-TS additionally
supports per-leaf annotations via an extra `Annotated`
constructor. Porting that here means extending the `Cause`
ADT and updating every pattern match plus the FFI conversion
in `Internal.js` (`causeToJS` / `jsToCause`) and every
finalizer-composition path in the runtime. That is an
invasive change for a low-frequency observability feature, so
it has its own follow-up ticket rather than landing here.

### #36 `Fiber.poll`

**Problem.** No non-blocking "is this fiber done?" call.
`join` blocks; `observeFiber` is callback-shaped.

**Shape.**

```purescript
Fiber.poll :: forall r e a. Fiber e a -> RIO r e (Maybe (Outcome e a))
```

**Implementation.** Built from existing `Internal.fiberIsDone`
+ `Internal.fiberOutcome` primitives; no FFI changes required.
Landed in `RIO.Fiber.Core` alongside the control-flow
combinators (see revised #25).

## Suggested batching, updated again

**Batch 1.6 (Tier A).** Items #25, #26, #27. RIO ergonomics +
Layer composition + STM primitives. Together they close the
ergonomics, wiring, and atomic-composability gaps.

**Batch 1.7 (Tier B).** Items #28, #29, #30, #31, #32. Stream
constructors, stream ergonomics, Random depth, Scope's
Cause-aware finalizers, Sink primitives.

**Batch 1.8 (Tier C polish).** Items #33, #34, #35, #36.
