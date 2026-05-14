## `RIO.Sink` design proposal

This is a design pass for a `Sink` layer on top of `RIO.Stream`,
written before any code so the wire-level shape, fusion story,
and parallel-sink combinators can be agreed on while everything
is still cheap to change.

It does **not** propose a full ZIO `Channel`. Channels in ZIO are
parameterized over six type parameters because they unify
streams, sinks, and pipes into one bidirectional algebra. The
unification has clear theoretical appeal but pays for it with a
surface area that's hard for a casual reader to inspect. The
recommendation here is to ship a focused `Sink` that covers the
real-world terminating-consumer cases, and to only revisit
`Channel` if and when a concrete need for stream-to-stream
transducers shows up that `mapM` / `flatMap` cannot already
express.

## What we already have

`RIO.Stream` ships three terminating runners today:

```purescript
runDrain   :: Stream r e a -> RIO r e Unit
runCollect :: Stream r e a -> RIO r e (Array a)
runFold    :: b -> (b -> a -> b) -> Stream r e a -> RIO r e b
runFoldM   :: b -> (b -> a -> RIO r e b) -> Stream r e a -> RIO r e b
```

These cover the common cases (drain for side-effects, collect
for tests, fold for aggregation). What they do **not** cover:

1. **Composable consumers**. There is no first-class value that
   represents "consume a stream into an `a`"; each runner is a
   distinct top-level function.
2. **Early termination from the consumer side**. `runFoldM` walks
   the whole stream even when the accumulator has already settled.
   Returning `Done` from a step is not expressible in its
   signature.
3. **Parallel consumption**. There is no `zipPar`-style
   combinator that runs two consumers against the same stream
   without materialising the elements twice.

A `Sink` value addresses all three.

## Proposed shape

```purescript
data SinkStep r e i a
  = Continue (Sink r e i a)
  | Final a

newtype Sink r e i a = Sink (RIO r e (SinkInit r e i a))

data SinkInit r e i a
  = SinkInitial a (i -> Sink r e i a)
```

A `Sink r e i a` is an effectful description of "I will consume
some prefix of `i`s and produce an `a`". `SinkInitial` carries
two things:

- the value the sink would return **if the stream is already
  empty** (the "zero" / identity)
- a step function: given the next `i`, produce a new `Sink`
  whose `SinkInitial` either yields another step (`Continue`-like)
  or signals end-of-consumption by returning a fully-specified
  initial value with whatever step continuation makes sense
  (typically one that ignores further input)

In practice the inner shape is closer to:

```purescript
newtype Sink r e i a = Sink (RIO r e (Step r e i a))

data Step r e i a
  = Need a (i -> Sink r e i a)  -- has a default a; please give me more i
  | Halt a                       -- finalised; ignore remaining input
```

`Need a k` is "if the stream ends now, I return `a`; otherwise
push `i` into `k` for the next sink". `Halt a` is "I'm done,
short-circuit the rest of the stream".

This is the shape ZIO 1.x's `ZSink` had before Channels, and the
one Conduit / Pipes-Sink expose. It's small, it composes well,
and it's strictly more expressive than `runFoldM`.

## The runner

```purescript
runSink :: forall r e i a. Sink r e i a -> Stream r e i -> RIO r e a
```

`runSink sink stream` pulls from `stream` and feeds each yielded
value into the sink's step. The loop stops on:

- the stream signalling `Done` (return the sink's current
  default `a`)
- the sink signalling `Halt a` (ignore the rest of the stream;
  resources still release via `scoped`)

Note the argument order: `Sink` first, `Stream` second. This
mirrors `>>=` (consumer comes after producer) but makes
`runSink mySink` partially applicable for re-use across
streams, which is the whole point of having a Sink value.

## Primitive sinks

The minimum interesting set:

```purescript
-- terminal values
drain     :: forall r e i. Sink r e i Unit
head      :: forall r e i. Sink r e i (Maybe i)
last      :: forall r e i. Sink r e i (Maybe i)
count     :: forall r e i. Sink r e i Int
collect   :: forall r e i. Sink r e i (Array i)

-- folds
foldL     :: forall r e i a. a -> (a -> i -> a) -> Sink r e i a
foldM     :: forall r e i a. a -> (a -> i -> RIO r e a) -> Sink r e i a

-- short-circuiting
take      :: forall r e i. Int -> Sink r e i (Array i)
find      :: forall r e i. (i -> Boolean) -> Sink r e i (Maybe i)
any       :: forall r e i. (i -> Boolean) -> Sink r e i Boolean
all       :: forall r e i. (i -> Boolean) -> Sink r e i Boolean
```

`take n` and `find p` are the headline examples for the
`Halt`-case. Today's `runFoldM` can express `count` but cannot
express `take 5` against an infinite stream without leaking
fibers; `Sink.take 5` finalises cleanly via `Halt`.

## Combinators

```purescript
mapResult :: (a -> b) -> Sink r e i a -> Sink r e i b
mapInput  :: (j -> i) -> Sink r e i a -> Sink r e j a
filterIn  :: (i -> Boolean) -> Sink r e i a -> Sink r e i a
```

`mapResult` is `Functor`; `mapInput` is contravariant on the
input side. `filterIn` is the obvious convenience.

Parallel composition (the real reason this layer exists):

```purescript
zipPar    :: Sink r e i a -> Sink r e i b -> Sink r e i (Tuple a b)
zipParWith
  :: (a -> b -> c)
  -> Sink r e i a
  -> Sink r e i b
  -> Sink r e i c
```

`zipPar a b` runs `a` and `b` against the same stream. Each
input element is offered to both sinks; both step functions run
on the same fiber, sequentially, before the next stream pull.
This is *not* "two fibers each draining their own copy" — that's
what `RIO.Stream.Par.broadcast` is for. `zipPar` is the
single-fiber, single-pass variant. It halts when **both** sinks
have halted; if one halts early its previous default `a` is
remembered.

Sequential composition (Sinks form a `Monad` on the output side
only — input remains a parameter):

```purescript
andThen
  :: Sink r e i a
  -> (a -> Sink r e i b)
  -> Sink r e i b
```

`andThen` runs the first sink to completion, then resumes from
the same stream position with the second. `Bind` for `Sink r e i`
follows the same pattern as `runFoldM` chained through `>>=`.

## Failure model

Sinks live in `RIO r e`, so each step can raise on any of the
error rows. The runner surfaces typed failures unchanged. If
the upstream `Stream` raises, the sink's current default `a` is
discarded and the failure propagates — same shape as `runFoldM`
today.

`zipPar` follows the existing first-failure-wins rule from
`RIO.Stream.Par`: the first sink to raise wins; the sibling's
step is not consulted again.

## Interaction with `RIO.Stream.Par`

`broadcast n bufferSize stream` is the existing primitive that
hands one stream to `n` concurrent consumers, each on its own
fiber, with end-to-end backpressure. The new `Sink` layer
composes with it:

```purescript
runMany :: forall r e i a. Sink r e i a -> Int -> Stream r e i -> RIO r e (Array a)
runMany sink n stream = do
  consumers <- broadcast n bufferSize stream
  fibers <- traverse (\s -> fork (runSink sink s)) consumers
  traverse join fibers
```

`zipPar` and `runMany` are two distinct operations: `zipPar` is
"two different sinks, one fiber, one pull per element";
`runMany` is "same sink shape, N fibers, N pulls per element".
Picking between them is a backpressure decision, not a syntax
decision.

## What this does **not** propose

- `Channel` as a six-parameter algebra. The convenience of
  unifying streams, sinks, and pipes does not pay for its
  surface area in the demo-sized library this is targeting.
- A full ZIO `ZSink.fromQueue` / `ZSink.fromHub` family. Once
  `Sink` exists, those are 5-line aliases over `Sink.foldM`
  pointing at the `RIO.Queue` / `RIO.Hub` modules; ship them
  in a follow-up only when an example actually needs them.
- A push-based variant. The pull-based `Stream` already in the
  library composes with this pull-based `Sink` directly. A
  push-based `Stream` would be a different design conversation.

## Recommended landing order

1. Land `RIO.Sink` with `drain`, `head`, `last`, `count`,
   `collect`, `foldL`, `foldM`, `take`, `find`, `any`, `all`,
   `mapResult`, `mapInput`, `filterIn`, `andThen`, and the
   `runSink` runner.
2. Land `zipPar` / `zipParWith` once (1) is stable.
3. Update `RIO.Stream` runners to delegate: `runDrain = runSink
   Sink.drain`, etc. The old top-level names stay as ergonomic
   aliases.
4. Update `docs/13-streams.md` with a Sink section and the
   ZSink-comparison row that today reads "TODO".

If a real Channel use-case shows up after that (a stream-to-
stream transducer that `mapM` / `flatMap` cannot express),
revisit the design then with that concrete shape in mind.
