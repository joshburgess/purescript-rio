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

What's intentionally missing: a `Sink` / `Channel` style for
composable terminating consumers. Sinks are the next major
addition (see `FUTURE_WORK.md`) and warrant a focused design
pass because the wire-level shape, fusion story, and parallel
sink combinators are all intertwined.

In the meantime, the existing `runFold` / `runFoldM` /
`runDrain` runners cover the common terminating-consumer cases.
For multi-output consumers or `Sink.zipPar`-style composition,
either compose `runFoldM` against multiple `Ref`s or wait for
the dedicated `Sink` design.
