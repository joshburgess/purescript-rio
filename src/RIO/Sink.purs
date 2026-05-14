-- | Composable terminating consumers for `RIO.Stream`.
-- |
-- | A `Sink r e i a` is a value that knows how to consume some
-- | prefix of `i`s pulled from a stream and produce an `a`. Where
-- | the runners in `RIO.Stream` (`runDrain`, `runFold`, etc.) are
-- | distinct top-level functions, a `Sink` is a first-class
-- | reusable value:
-- |
-- | * Compose with `mapResult`, `mapInput`, `andThen`.
-- | * Short-circuit a long or infinite stream via `take`, `find`,
-- |   `any`, `all`.
-- | * Run against any compatible stream with `runSink`.
-- |
-- | The shape is `Need k finish | Halt a`. `Need k finish` means
-- | "give me the next `i` via `k`, or run `finish` if the stream
-- | is empty". `Halt a` finalises the sink and the runner stops
-- | pulling. `finish` is an `RIO r e a` so end-of-stream handling
-- | can itself be effectful (and so `andThen` can thread a
-- | continuation through it).
-- |
-- | ```purescript
-- | -- the first 10 even numbers
-- | runSink (filterIn even (take 10)) (fromArray [ 1 .. 1000 ])
-- | ```
-- |
-- | User-facing reference: `docs/13-streams.md` ("Composable
-- | consumers"). Design rationale (why `Need k finish`, why
-- | single-fiber `zipPar`, why no Channel): `docs/sink-design.md`.
module RIO.Sink
  ( Sink(..)
  , Step(..)
  , unSink
  , runSink
  , drain
  , head
  , last
  , count
  , collect
  , foldL
  , foldM
  , take
  , find
  , any
  , all
  , mapResult
  , mapInput
  , filterIn
  , andThen
  , zipPar
  , zipParWith
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import RIO.Core (RIO)
import RIO.Stream (Stream)
import RIO.Stream as Stream

-- | One step of a sink. `Need k finish` is "feed me the next input
-- | via `k`, or run `finish` if the stream is empty". `Halt a` is
-- | "I'm finalised, ignore the rest of the stream".
data Step r e i a
  = Need (i -> Sink r e i a) (RIO r e a)
  | Halt a

-- | A composable, terminating consumer of `i`s producing an `a`.
newtype Sink :: Row Type -> Row Type -> Type -> Type -> Type
newtype Sink r e i a = Sink (RIO r e (Step r e i a))

unSink :: forall r e i a. Sink r e i a -> RIO r e (Step r e i a)
unSink (Sink s) = s

-- | Run a sink against a stream. Pulls from `stream` and feeds
-- | each yielded value into the sink's step. Stops on stream
-- | exhaustion (runs the sink's `finish` action) or on the sink
-- | halting (ignores the rest of the stream; scoped finalizers
-- | still release).
runSink
  :: forall r e i a
   . Sink r e i a
  -> Stream r e i
  -> RIO r e a
runSink sink stream = do
  step <- unSink sink
  case step of
    Halt a -> pure a
    Need k finish -> do
      sstep <- Stream.unStream stream
      case sstep of
        Stream.Done -> finish
        Stream.Yield i rest -> runSink (k i) rest

-- | Drain the stream for its effects; return `unit`.
drain :: forall r e i. Sink r e i Unit
drain = Sink (pure (Need (\_ -> drain) (pure unit)))

-- | The first element, or `Nothing` if the stream is empty.
head :: forall r e i. Sink r e i (Maybe i)
head = Sink
  (pure (Need (\i -> Sink (pure (Halt (Just i)))) (pure Nothing)))

-- | The last element, or `Nothing` if the stream is empty.
last :: forall r e i. Sink r e i (Maybe i)
last = go Nothing
  where
  go acc = Sink (pure (Need (\i -> go (Just i)) (pure acc)))

-- | The number of elements pulled.
count :: forall r e i. Sink r e i Int
count = go 0
  where
  go n = Sink (pure (Need (\_ -> go (n + 1)) (pure n)))

-- | Every element pulled, in input order.
collect :: forall r e i. Sink r e i (Array i)
collect = go []
  where
  go acc = Sink (pure (Need (\i -> go (Array.snoc acc i)) (pure acc)))

-- | Left fold with a pure step.
foldL :: forall r e i a. a -> (a -> i -> a) -> Sink r e i a
foldL seed step = go seed
  where
  go acc = Sink (pure (Need (\i -> go (step acc i)) (pure acc)))

-- | Left fold with an effectful step.
foldM
  :: forall r e i a
   . a
  -> (a -> i -> RIO r e a)
  -> Sink r e i a
foldM seed step = go seed
  where
  go acc = Sink (pure (Need next (pure acc)))
    where
    next i = Sink do
      acc' <- step acc i
      unSink (go acc')

-- | The first `n` elements as an array. If the stream has fewer
-- | than `n` elements, returns whatever was collected.
take :: forall r e i. Int -> Sink r e i (Array i)
take n
  | n <= 0 = Sink (pure (Halt []))
  | otherwise = go [] n
      where
      go acc 0 = Sink (pure (Halt acc))
      go acc remaining = Sink (pure (Need next (pure acc)))
        where
        next i =
          let
            acc' = Array.snoc acc i
            remaining' = remaining - 1
          in
            if remaining' == 0 then Sink (pure (Halt acc'))
            else go acc' remaining'

-- | The first element matching `p`, or `Nothing` if none does.
find :: forall r e i. (i -> Boolean) -> Sink r e i (Maybe i)
find p = Sink (pure (Need step (pure Nothing)))
  where
  step i = if p i then Sink (pure (Halt (Just i))) else find p

-- | Whether any element matches `p`. Short-circuits on the first
-- | match.
any :: forall r e i. (i -> Boolean) -> Sink r e i Boolean
any p = Sink (pure (Need step (pure false)))
  where
  step i = if p i then Sink (pure (Halt true)) else any p

-- | Whether every element matches `p`. Short-circuits on the
-- | first non-match.
all :: forall r e i. (i -> Boolean) -> Sink r e i Boolean
all p = Sink (pure (Need step (pure true)))
  where
  step i = if p i then all p else Sink (pure (Halt false))

-- | Post-process the sink's result.
mapResult :: forall r e i a b. (a -> b) -> Sink r e i a -> Sink r e i b
mapResult f sink = Sink do
  step <- unSink sink
  case step of
    Halt a -> pure (Halt (f a))
    Need k finish ->
      pure (Need (\i -> mapResult f (k i)) (map f finish))

-- | Pre-process the sink's input. Contravariant on `i`.
mapInput :: forall r e i j a. (j -> i) -> Sink r e i a -> Sink r e j a
mapInput f sink = Sink do
  step <- unSink sink
  case step of
    Halt a -> pure (Halt a)
    Need k finish ->
      pure (Need (\j -> mapInput f (k (f j))) finish)

-- | Drop inputs for which the predicate is false before feeding
-- | them to the underlying sink.
filterIn :: forall r e i a. (i -> Boolean) -> Sink r e i a -> Sink r e i a
filterIn p sink = Sink do
  step <- unSink sink
  case step of
    Halt a -> pure (Halt a)
    Need k finish ->
      pure
        ( Need
            ( \i ->
                if p i then filterIn p (k i)
                else filterIn p sink
            )
            finish
        )

-- | Sequence two sinks: run the first against the stream, then
-- | resume with the second from the same stream position. The
-- | second sink is built from the first's result.
-- |
-- | If the stream ends while the first sink is still consuming,
-- | the first sink's `finish` runs, the result is fed into `k`,
-- | and the resulting second sink is run against an empty stream
-- | (so it returns its own `finish` value).
andThen
  :: forall r e i a b
   . Sink r e i a
  -> (a -> Sink r e i b)
  -> Sink r e i b
andThen sink k = Sink do
  step <- unSink sink
  case step of
    Halt a -> unSink (k a)
    Need k' finish ->
      pure
        ( Need
            (\i -> andThen (k' i) k)
            (finish >>= \a -> runSink (k a) Stream.empty)
        )

-- | Run two sinks in lockstep against the same stream. Each
-- | input is offered to both sinks; both step in one fiber,
-- | sequentially, before the next stream pull. The combined sink
-- | halts when both sides have halted; if one halts early its
-- | final value is remembered and only the other continues to
-- | see inputs.
-- |
-- | This is *not* `broadcast`: there are no separate fibers and
-- | no per-consumer queue. Use `broadcast` from `RIO.Stream.Par`
-- | for the multi-fiber, per-consumer-buffer variant.
zipPar
  :: forall r e i a b
   . Sink r e i a
  -> Sink r e i b
  -> Sink r e i (Tuple a b)
zipPar sa sb = Sink do
  stepA <- unSink sa
  stepB <- unSink sb
  pure (combineSteps stepA stepB)

-- | `zipPar` with a combining function instead of `Tuple`.
zipParWith
  :: forall r e i a b c
   . (a -> b -> c)
  -> Sink r e i a
  -> Sink r e i b
  -> Sink r e i c
zipParWith f sa sb =
  mapResult (\(Tuple a b) -> f a b) (zipPar sa sb)

combineSteps
  :: forall r e i a b
   . Step r e i a
  -> Step r e i b
  -> Step r e i (Tuple a b)
combineSteps (Halt a) (Halt b) = Halt (Tuple a b)
combineSteps (Halt a) (Need kb finB) =
  Need
    ( \i -> Sink do
        stepB' <- unSink (kb i)
        pure (combineSteps (Halt a) stepB')
    )
    (map (Tuple a) finB)
combineSteps (Need ka finA) (Halt b) =
  Need
    ( \i -> Sink do
        stepA' <- unSink (ka i)
        pure (combineSteps stepA' (Halt b))
    )
    (map (\a -> Tuple a b) finA)
combineSteps (Need ka finA) (Need kb finB) =
  Need
    ( \i -> Sink do
        stepA' <- unSink (ka i)
        stepB' <- unSink (kb i)
        pure (combineSteps stepA' stepB')
    )
    (Tuple <$> finA <*> finB)
