# rio-fiber ecosystem guide

This guide ties together `Stream`, `Pipe`, `Sink`, and the `STM`
modules. The README's per-module list answers "what's here." This
guide answers "how do the pieces actually fit together."

It assumes you've read the rio-fiber README and have a working
mental model of `RIO r e a` and the fiber runtime.

## The pull triangle: Stream, Pipe, Sink

`rio-fiber` pulls streams: the downstream consumer drives the
upstream producer one element at a time. The three core types
fall out of one shape (an `RIO r e ...` action) specialized to
three positions:

| Type | Shape | Role |
|---|---|---|
| `Stream r e a` | `RIO r e (Step r e a)` | producer (yield `a` or end) |
| `Pipe r e i o` | `RIO r e (PipeLoop r e i o)` | i-to-o transducer |
| `Sink r e i o` | `RIO r e (SinkLoop r e i o)` | terminating consumer (produce one `o`) |

A pipeline is a `Stream` spliced through zero or more `Pipe`s into
a `Sink` (or one of the convenience runners `run` / `runCollect` /
`fold` / `forEach`). Everything is built up as a value first; no
work happens until you run the result.

```purescript
import RIO.Fiber.Stream as Stream
import RIO.Fiber.Pipe as Pipe
import RIO.Fiber.Sink as Sink

-- Sum the squares of the even numbers in 1..100.
sumOfEvenSquares :: RIO () () Int
sumOfEvenSquares =
  Stream.runSink
    ( Stream.via
        (Stream.via
          (Stream.fromArray (Array.range 1 100))
          (Pipe.filter even))
        (Pipe.map (\n -> n * n))
    )
    Sink.sum
```

`Stream.via` splices a pipe into a stream; `Stream.runSink` plugs
a sink onto the end. Pipes compose with `Pipe.andThen` if you want
to build the whole transducer before splicing.

### When to choose each type

- **Stream** when you have a producer. Anything that emits one
  element at a time, with backpressure baked into the pull, is a
  Stream. Constructors include `fromArray`, `repeatRIO`,
  `fromQueue`, `fromTQueue`, and `acquireReleaseStream` for
  resource-bracketed sources.

- **Pipe** when the same transformation has to work in two
  different pipelines. A `Pipe r e i o` is a first-class value;
  you can hold it in a `Map`, pass it through layers, or wire
  it differently per environment. If the transformation is
  one-off, the `Stream.map` / `Stream.filter` shortcuts inline it
  without needing the pipe machinery.

- **Sink** when you need a *composable* terminator. `Sink.fold`,
  `Sink.takeN`, `Sink.head`, and `Sink.collectAll` all share
  `map` and `contramap` so they compose into bigger sinks. For
  the common cases (`drain`, `fold`, `forEach`, `runCollect`)
  the bare runners on `Stream` are shorter.

## Concurrency-shaped operators

The pull triangle is single-threaded by default: each pull blocks
on the previous one's result. The runtime adds concurrency at
specific points:

- `Stream.mapPar n f` runs up to `n` mapping operations in flight,
  preserving order on the output.
- `Stream.merge a b` interleaves two streams in whatever order
  their pulls happen to resolve.
- `Stream.zipPar` zips two streams in parallel: both upstream
  pulls run concurrently before the zipper sees them.
- `Stream.broadcast n s` returns `n` streams that each receive
  every element. `Stream.share` is the dynamic-fanout variant
  built on `Hub`.

These all live on the producer side. The consumer side stays
single-threaded by construction; if you want parallel consumption,
fan out to a queue and stand up multiple consumers manually.

## The Queue / Hub seam

`Stream.fromQueue` and `Stream.fromTQueue` are the seam between
"some other fiber is pushing values" and "this stream pulls."
The pattern looks like:

```purescript
import RIO.Fiber.Queue as Q
import RIO.Fiber.Stream as Stream

watchAndReport :: RIO r () Unit
watchAndReport = do
  q <- liftEffect (Q.bounded 64)

  -- Producer fiber: push every event into the queue.
  _ <- F.fork (forever (push q))

  -- Consumer: drain into a sink as a stream.
  Stream.run (Stream.via (Stream.fromQueue q) (Pipe.map render))
```

`Hub` is a fan-out variant of `Queue`: every subscriber sees
every published value. `Stream.share` is a thin convenience over
`Hub` for the "split this stream into N downstream consumers"
case.

## STM: event-loop atomicity

`RIO.Fiber.STM` is software transactional memory built on the
fiber event loop. Each STM block runs atomically with respect to
all other STM blocks because the runtime's step loop is
single-threaded: there's no preemption inside an STM commit, so
no version check or retry loop is needed.

The user-facing API is small:

- `STM.atomically :: STM a -> RIO r e a` runs a transaction.
- `STM.retry :: STM a` voluntarily suspends until a watched
  `TVar` changes, then re-runs the transaction.
- `STM.orElse :: STM a -> STM a -> STM a` runs the left branch;
  if it retries, runs the right branch instead.
- `STM.check :: Boolean -> STM Unit` retries unless the predicate
  holds.

Plus the STM-aware structures: `TVar`, `TMVar`, `TChan`, `TQueue`,
`TArray`.

### A typical pattern: a counter with bounded capacity

```purescript
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TVar as TVar

makeCounter :: Int -> RIO r e (RIO r e Unit)
makeCounter cap = do
  v <- STM.atomically (TVar.new 0)
  pure do
    STM.atomically do
      n <- TVar.read v
      STM.check (n < cap)
      TVar.write v (n + 1)
```

`STM.check (n < cap)` parks the fiber via `retry` until some
*other* transaction commits a write that changes `v`. No
busy-spin: the runtime tracks which TVars each retried
transaction observed and only wakes it when one of them changes.

### Composing transactions

The shape every other library calls "blocking primitives" is
just one `STM` value here. `orElse` is the choice; `retry` is
the wait. A non-blocking `tryTake` on a `TMVar` is
`tryTakeTMVar v = takeTMVar v `STM.orElse` pure Nothing`.

This composes through `STM.atomically` because the whole
`STM a` value runs as one atomic step. You never see a
half-committed state from another fiber, and there's no need
for nested locking.

## Streams meet STM

The most useful intersection is `Stream.fromTQueue`. A
`TQueue` is an STM queue: writers and readers compose with
other STM transactions, so you can atomically "push to two
queues and update a counter" in one step. Wrapping the queue
in `Stream.fromTQueue` gives you a stream that pulls one
committed write at a time, with backpressure handled by the
queue's bounded capacity.

```purescript
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TQueue as TQ
import RIO.Fiber.Stream as Stream

example :: RIO r () (Array Int)
example = do
  q <- STM.atomically (TQ.bounded 16)

  -- Producer fiber.
  _ <- F.fork do
    for_ (Array.range 1 100) \n ->
      STM.atomically (TQ.writeTQueue q n)
    STM.atomically (TQ.closeTQueue q)

  -- Consumer pulls one element per STM step.
  Stream.runCollect (Stream.fromTQueue q)
```

The producer's `STM.atomically` step is a single event-loop
turn; the consumer's pull is another. Between them the queue
is the only synchronization point, and it composes with any
other STM transaction the producer or consumer wants to
combine.

## Scopes and cleanup

Anything that owns a resource (a forked fiber, a file handle,
an allocator) lives in a `Scope`. The two common entry points:

- `Scope.scoped` introduces a fresh scope around a body. When
  the body exits (success, failure, defect, or interrupt) the
  scope's finalizers run in LIFO order.
- `acquireReleaseStream` is the streaming variant: the stream
  acquires when its first pull happens and releases when the
  caller's surrounding scope closes.

`forkScoped` and `forkSupervised` are the structured-
concurrency forks. `forkSupervised` ties the child to the
nearest `supervised` block via a `FiberRef`, so a deeply
nested call site doesn't need to thread the scope explicitly.

## Putting it together

A small end-to-end example: read events from a TQueue, batch
them in groups of ten with a 50ms timeout, and write each
batch to a sink.

```purescript
import Data.Time.Duration (Milliseconds(..))
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TQueue as TQ
import RIO.Fiber.Stream as Stream
import RIO.Fiber.Pipe as Pipe
import RIO.Fiber.Sink as Sink

batched :: RIO r () Int
batched = do
  q <- STM.atomically (TQ.bounded 1024)

  -- assume producer running somewhere

  Stream.runSink
    ( Stream.via
        (Stream.fromTQueue q)
        ( Pipe.chunked 10
            `Pipe.andThen` Pipe.map handleBatch
        )
    )
    Sink.count
```

`Pipe.chunked 10` groups consecutive elements into arrays of
ten (and flushes a partial tail on end-of-stream). `Pipe.map
handleBatch` runs a per-batch effect. `Sink.count` reports the
number of batches that flowed through. Each piece is a value;
nothing executes until `runSink` plugs the sink onto the end.

## See also

- `rio-fiber/README.md` for the per-module reference.
- `RIO.Fiber.Aff` for the bridge to Aff-shaped code.
- `RIO.Fiber.Cause` for the failure algebra all of these
  combinators thread through unchanged.
