## Streams

`RIO.Aff.Stream` / `RIO.Fiber.Stream` is a pull-based effectful
stream sitting on top of `RIO`. Each step is a single `RIO`
action that either yields the next value paired with the rest
of the stream, or signals end-of-stream. The stream itself has
no schedule of its own: nothing happens until a runner pulls
from it.

> **Naming convention.** Code samples below use unqualified
> `RIO.Stream.*` / `RIO.Sink` / `RIO.Channel` / `RIO.Concurrency`
> shorthand for readability. The live imports are
> `RIO.Aff.Stream.*` / `RIO.Aff.Sink` / `RIO.Aff.Channel` /
> `RIO.Aff.Concurrency` (rio-aff) or `RIO.Fiber.Stream` /
> `RIO.Fiber.Sink` / `RIO.Fiber.Channel` /
> `RIO.Fiber.Concurrency` (rio-fiber); the substitution is
> mechanical. One structural
> difference worth calling out: rio-aff splits the surface into
> `RIO.Aff.Stream`, `RIO.Aff.Stream.Par`,
> `RIO.Aff.Stream.Concurrent`, `RIO.Aff.Stream.Resource`, and
> `RIO.Aff.Stream.Timed`. rio-fiber consolidates the whole
> surface into a single `RIO.Fiber.Stream` module (parallel,
> resource-safe, and time-based combinators all live there).
> The combinator names are mostly the same in both, with a few
> consistent renames worth knowing about. rio-aff spells the
> effectful-callback variants with an `M` suffix; rio-fiber
> spells them with a `RIO` suffix: `mapM` / `foldM` / `findM` /
> `unfoldM` / `repeatM` in rio-aff become `mapRIO` / `foldRIO` /
> `findRIO` / `unfoldRIO` / `repeatRIO` in rio-fiber. The
> single-element constructor is also renamed: rio-aff's `single`
> is rio-fiber's `emit`. Two stream runners also diverge in
> name: rio-aff's `runDrain` and `runFold` are rio-fiber's `run`
> and `fold` (`runFoldM` keeps the same name in both). The
> `runFold` / `fold` rename also flips argument order: rio-aff
> takes `seed` first and `step` second (`runFold seed step
> stream`), rio-fiber takes `step` first and `seed` second
> (`fold step seed stream`). A handful of `RIO.Sink` primitives
> also diverge: rio-aff's `collect`, `foldL`, `foldM`,
> `mapResult`, and `zipParWith` are rio-fiber's `collectAll`,
> `fold`, `foldRIO`, `map`, and `zipWithPar`; the `foldM` /
> `foldRIO` Sink rename has the same `seed`-first vs `step`-
> first argument flip as the stream-runner pair. Where a code
> sample below uses an rio-aff name, rio-fiber readers should
> substitute the matching name from this list.

```purescript
data Step r e a
  = Yield a (Stream r e a)
  | Done

newtype Stream r e a = Stream (RIO r e (Step r e a))
```

That signature tells you everything about the cost model: a
stream is just an `RIO` action whose result describes the next
step. Mapping a function over a stream costs an allocation per
yield, not a fresh fiber. Failing partway propagates as a
typed-error `Left` on the underlying `RIO`, the same as any
other `RIO` action.

## Constructors

The simplest values:

```purescript
empty       :: forall r e a. Stream r e a
single      :: forall r e a. a -> Stream r e a
fromArray   :: forall r e a. Array a -> Stream r e a
```

For effectful sources, `unfoldM` and `repeatM` lift an `RIO`
into a stream:

```purescript
unfoldM
  :: forall r e s a
   . s
  -> (s -> RIO r e (Maybe (Tuple a s)))
  -> Stream r e a

repeatM :: forall r e a. RIO r e a -> Stream r e a
```

`unfoldM` is the building block for "drain a cursor": each
step returns either the next value plus a new seed, or
`Nothing` to terminate. `repeatM` is the infinite variant for
tickers or polling loops.

## Transforms

```purescript
map     :: (a -> b) -> Stream r e a -> Stream r e b
filter  :: (a -> Boolean) -> Stream r e a -> Stream r e a
mapM    :: (a -> RIO r e b) -> Stream r e a -> Stream r e b
take    :: Int -> Stream r e a -> Stream r e a
drop    :: Int -> Stream r e a -> Stream r e a
concat  :: Stream r e a -> Stream r e a -> Stream r e a
flatMap :: Stream r e a -> (a -> Stream r e b) -> Stream r e b
```

`mapM` is the effectful map; each element produces a new `RIO`
action whose result becomes the new yielded value. `flatMap`
substitutes each element with an inner stream and concatenates
them.

## Runners

Streams describe a computation; runners execute it:

```purescript
runDrain   :: Stream r e a -> RIO r e Unit
runCollect :: Stream r e a -> RIO r e (Array a)
runFold    :: b -> (b -> a -> b) -> Stream r e a -> RIO r e b
runFoldM   :: b -> (b -> a -> RIO r e b) -> Stream r e a -> RIO r e b
```

`runDrain` walks the stream for its effects and discards
values. `runCollect` accumulates into an array (don't reach for
it on infinite streams). `runFold` / `runFoldM` are the
left-fold variants.

A complete example:

```purescript
import RIO.Stream

program :: forall r. RIO r () Int
program = runFold 0 (+) (map (_ * 2) (fromArray [ 1, 2, 3 ]))

-- program evaluates to 12
```

## Parallel combinators (`RIO.Stream.Par`)

A pull-based stream is fundamentally single-channel: each step
reads from one upstream. `RIO.Stream.Par` adds the
fan-in / fan-out combinators.

```purescript
mergeAll :: Array (Stream r e a) -> Stream r e a
merge    :: Stream r e a -> Stream r e a -> Stream r e a
mergeMap :: (a -> Stream r e b) -> Stream r e a -> Stream r e b
```

`mergeAll` runs every input stream on its own fiber and yields
values in the order they land on a shared bounded queue. Output
order is non-deterministic across inputs but each input's
internal order is preserved.

```purescript
import RIO.Stream.Par (mergeAll)

events = mergeAll
  [ ticker (Milliseconds 100.0)
  , consumer kafkaPartition0
  , consumer kafkaPartition1
  ]
```

`mergeMap` is the `flatMap`-shaped variant: drains the outer
stream, then runs `mergeAll` over the inners. The outer is
materialised eagerly, so this combinator is not suitable for an
infinite outer source.

```purescript
broadcast :: Int -> Int -> Stream r e a -> RIO r e (Array (Stream r e a))
partition :: Int -> Int -> (a -> Int) -> Stream r e a -> RIO r e (Array (Stream r e a))
```

`broadcast n bufferSize` fans one upstream out to `n` consumer
streams, each with a per-consumer bounded queue. Every consumer
sees every element. The slowest consumer applies backpressure
to the producer.

`partition n bufferSize toBucket` routes each element to
exactly one bucket via `toBucket x \`mod\` n`. The N consumers
see disjoint slices of the input.

```purescript
import RIO.Stream.Par (broadcast)
import RIO.Concurrency (fork, join)

streamToTwoSinks upstream = do
  consumers <- broadcast 2 16 upstream
  fibers <- traverse
    (\s -> fork (runDrain (mapM logEvent s)))
    consumers
  traverse_ join fibers
```

### Failure model

All four parallel combinators share one rule: the *first* typed
failure or defect observed in any producer shuts the shared
queue (or every queue, for `broadcast` / `partition`) down.
Sibling producers continue running until they find the queue
closed and exit; their would-be failures are dropped.

If you want every concurrent failure preserved as a tree, drain
each branch separately through `RIO.Aff.Cause.parTraverseCause`
(or, on rio-fiber, through `attemptCause` and the cause-tree
constructors directly).

## Resource-safe streams (`RIO.Stream.Resource`)

A stream backed by an OS resource (a file handle, a Postgres
cursor, a network socket) needs that resource to live long
enough for every pull and to be released regardless of how the
stream ends. `RIO.Stream.Resource.bracketStream` ties the
resource's lifetime to the enclosing `scoped` block.

```purescript
bracketStream
  :: forall r e a
   . RIO (scope :: Scope | r) e a
  -> (a -> Aff Unit)
  -> Stream (scope :: Scope | r) e a
```

The stream yields the acquired resource as a single element,
then ends. Use `flatMap` to thread the resource through a
multi-element downstream:

```purescript
import RIO.Stream.Resource (bracketStream)

readLines :: String -> Stream (scope :: Scope | r) e String
readLines path = flatMap
  (bracketStream (openFile path) closeFile)
  (\handle -> linesFrom handle)

program = scoped do
  contents <- runCollect (readLines "/etc/hosts")
  pure contents
```

The file closes when the `scoped` block exits, on every
termination path: success, typed failure, defect, or fiber
kill. If `acquire` itself fails, the finalizer is never
registered (there is nothing to release) and the failure
propagates unchanged.

This is the scope-as-lifetime model ZIO uses; `ZStream`
requires `Scope` in its environment row when the stream owns
resources.

## Comparison to ZStream / Effect

The shape is intentionally smaller than ZIO's `ZStream`:

| ZStream                       | `RIO.Stream`                  |
| ----------------------------- | ----------------------------- |
| `ZStream.fromIterable`        | `fromArray`                   |
| `ZStream.unfold` / `unfoldZIO`| `unfoldM`                     |
| `ZStream.repeatZIO`           | `repeatM`                     |
| `ZStream.map` / `mapZIO`      | `map` / `mapM`                |
| `ZStream.filter`              | `filter`                      |
| `ZStream.take` / `drop`       | `take` / `drop`               |
| `ZStream.concat`              | `concat`                      |
| `ZStream.flatMap`             | `flatMap`                     |
| `ZStream.run`                 | `runDrain` / `runFold` / etc. |
| `ZStream.mergeAll`            | `RIO.Stream.Par.mergeAll`     |
| `ZStream.flatMapPar`          | `RIO.Stream.Par.mergeMap`     |
| `ZStream.broadcast`           | `RIO.Stream.Par.broadcast`    |
| `ZStream.partition`           | `RIO.Stream.Par.partition`    |
| `ZStream.scoped`              | `RIO.Stream.Resource.bracketStream` |
| `ZSink` (subset)              | `RIO.Sink`                    |

## Composable consumers (`RIO.Sink`)

`RIO.Sink` ships first-class terminating consumers. A
`Sink r e i a` consumes some prefix of `i`s from a `Stream` and
produces an `a`.

**rio-aff** spells the sink as a `Need k finish | Halt a` ADT:

```purescript
data Step r e i a
  = Need (i -> Sink r e i a) (RIO r e a)
  | Halt a

newtype Sink r e i a = Sink (RIO r e (Step r e i a))

runSink :: forall r e i a. Sink r e i a -> Stream r e i -> RIO r e a
```

`Need k finish` is "give me the next `i` via `k`, or run `finish`
on end-of-stream". `Halt a` finalises the sink early; the runner
stops pulling and any `scoped` finalizers in the stream still
release.

**rio-fiber** spells the same idea as a callback-record loop
rather than an ADT:

```purescript
type SinkLoop r e i o =
  { step :: i -> RIO r e (Maybe o)
  , done :: RIO r e o
  }

newtype Sink r e i o = Sink (RIO r e (SinkLoop r e i o))

runSink :: forall r e i o. Stream r e i -> Sink r e i o -> RIO r e o
```

`step i` returns `Just o` to halt early with that value or
`Nothing` to keep consuming; `done` is the end-of-stream
finaliser. The `runSink` argument order also flips: rio-aff is
`runSink sink stream`, rio-fiber is `runSink stream sink`.

Primitives:

```purescript
drain   :: Sink r e i Unit
head    :: Sink r e i (Maybe i)
last    :: Sink r e i (Maybe i)
count   :: Sink r e i Int
collect :: Sink r e i (Array i)
foldL   :: a -> (a -> i -> a) -> Sink r e i a
foldM   :: a -> (a -> i -> RIO r e a) -> Sink r e i a
take    :: Int -> Sink r e i (Array i)
find    :: (i -> Boolean) -> Sink r e i (Maybe i)
any     :: (i -> Boolean) -> Sink r e i Boolean
all     :: (i -> Boolean) -> Sink r e i Boolean
```

Combinators:

```purescript
mapResult  :: (a -> b) -> Sink r e i a -> Sink r e i b
mapInput   :: (j -> i) -> Sink r e i a -> Sink r e j a
filterIn   :: (i -> Boolean) -> Sink r e i a -> Sink r e i a
andThen    :: Sink r e i a -> (a -> Sink r e i b) -> Sink r e i b
zipPar     :: Sink r e i a -> Sink r e i b -> Sink r e i (Tuple a b)
zipParWith :: (a -> b -> c) -> Sink r e i a -> Sink r e i b -> Sink r e i c
```

`take`, `find`, `any`, and `all` short-circuit through `Halt`, so
they finalise cleanly against infinite streams without leaking
fibers:

```purescript
import RIO.Sink as Sink

firstTenEvens :: forall r e. Stream r e Int -> RIO r e (Array Int)
firstTenEvens = Sink.runSink
  (Sink.filterIn (\n -> n `mod` 2 == 0) (Sink.take 10))
```

`andThen` sequences two sinks against the same stream position.
The first sink runs to completion; its result is fed into the
continuation, which produces the second sink to consume the
remainder.

```purescript
import RIO.Sink as Sink

headAndRest = Sink.head `Sink.andThen` \mFirst ->
  Sink.mapResult (\rest -> { first: mFirst, rest }) Sink.collect
```

`zipPar` runs two sinks in lockstep against the same stream.
Each input is offered to both sinks; both step in one fiber,
sequentially, before the next stream pull. The combined sink
halts when *both* sides have halted; if one halts early its
final value is remembered and only the other continues to see
inputs. On end-of-stream, both finishers run.

```purescript
import RIO.Sink as Sink

-- count and total in a single pass
countAndTotal = Sink.zipParWith
  (\count total -> { count, total })
  Sink.count
  (Sink.foldL 0 (+))
```

`zipPar` is *not* `broadcast`: there are no separate fibers and
no per-consumer queue. Use `RIO.Stream.Par.broadcast` when each
consumer needs its own buffered backpressure boundary; use
`zipPar` when you want a single-pass combination on one fiber.

The full design discussion lives in `docs/sink-design.md`.
A minimal pull-based `RIO.Channel r e i o d` ships alongside
`Stream` and `Sink` (with `fromStream` / `fromSink` bridges,
`pipe`, and `run`) so stream-to-stream transducers are
expressible as first-class values when the standard
`Stream.mapM` / `Sink.andThen` shapes are not enough.

## Worked examples

- `examples/stream-pipeline/` builds three partition sources,
  merges them via `RIO.Stream.Par.mergeAll`, and fans the
  merged stream out to two consumers via
  `RIO.Stream.Par.broadcast` (one printer, one per-source
  aggregator).
- `examples/sink-analytics/` runs five small sinks (`count`,
  `filterIn isError count`, `mapInput latencyMs` over a
  max-fold, a path-set fold, and `find` for the first slow
  request) over a synthetic HTTP request log, composed with
  `Sink.zipPar` and run via `Sink.runSink`. One stream pass
  produces the full summary.

## Pointers

- Source:
  rio-aff:
  [`rio-aff/src/RIO/Aff/Stream.purs`](../rio-aff/src/RIO/Aff/Stream.purs)
  (the pull-based core),
  [`rio-aff/src/RIO/Aff/Stream/Par.purs`](../rio-aff/src/RIO/Aff/Stream/Par.purs)
  (`mergeAll`, `broadcast`), and
  [`rio-aff/src/RIO/Aff/Sink.purs`](../rio-aff/src/RIO/Aff/Sink.purs)
  (one-pass consumers and `zipPar`). rio-fiber:
  [`rio-fiber/src/RIO/Fiber/Stream.purs`](../rio-fiber/src/RIO/Fiber/Stream.purs)
  (consolidated module) and
  [`rio-fiber/src/RIO/Fiber/Sink.purs`](../rio-fiber/src/RIO/Fiber/Sink.purs).
- Spec coverage:
  [`rio-aff/test/Test/RIO/Aff/StreamSpec.purs`](../rio-aff/test/Test/RIO/Aff/StreamSpec.purs)
  (construction / transforms / runners) and
  [`rio-aff/test/Test/RIO/Aff/SinkSpec.purs`](../rio-aff/test/Test/RIO/Aff/SinkSpec.purs)
  (sink primitives and `zipPar` semantics).
- Concurrency primitives the parallel combinators build on:
  [`docs/06-concurrency.md`](./06-concurrency.md).
- Sink design notes:
  [`docs/sink-design.md`](./sink-design.md).
- `RIO.Aff.Channel` / `RIO.Fiber.Channel` source and tests:
  [`rio-aff/src/RIO/Aff/Channel.purs`](../rio-aff/src/RIO/Aff/Channel.purs),
  [`rio-fiber/src/RIO/Fiber/Channel.purs`](../rio-fiber/src/RIO/Fiber/Channel.purs),
  [`rio-aff/test/Test/RIO/Aff/ChannelSpec.purs`](../rio-aff/test/Test/RIO/Aff/ChannelSpec.purs).
