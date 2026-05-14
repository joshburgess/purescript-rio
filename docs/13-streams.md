## Streams

`RIO.Stream` is a pull-based effectful stream sitting on top of
`RIO`. Each step is a single `RIO` action that either yields the
next value paired with the rest of the stream, or signals
end-of-stream. The stream itself has no schedule of its own:
nothing happens until a runner pulls from it.

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
each branch separately through `RIO.Cause.parTraverseCause`.

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

## Comparison to ZStream / Effect-TS

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
produces an `a`. The shape is `Need k finish | Halt a`:

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

The full design discussion, including what is intentionally not
shipped (a Channel algebra), lives in `docs/sink-design.md`.
