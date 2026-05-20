-- | Composable terminating consumers for `RIO.Aff.Stream`.
-- |
-- | A `Sink r e i a` is a value that knows how to consume some
-- | prefix of `i`s pulled from a stream and produce an `a`. Where
-- | the runners in `RIO.Aff.Stream` (`runDrain`, `runFold`, etc.) are
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
-- | single-fiber `zipPar`, how `Sink` relates to `RIO.Aff.Channel`):
-- | `docs/sink-design.md`.
module RIO.Aff.Sink
  ( Sink(..)
  , Step(..)
  , unSink
  , runSink
  , aggregate
  , transduce
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
  , contramapM
  , foreach
  , sum
  , product
  , minimum
  , maximum
  , mconcat
  , andThen
  , zipPar
  , zipParWith
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import RIO.Aff.Core (RIO)
import RIO.Aff.Stream (Stream)
import RIO.Aff.Stream as Stream

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

-- | Repeatedly run `sink` against `stream`, emitting each
-- | sink result as a stream element. Each cycle starts from a
-- | fresh instance of `sink`; when one halts the next cycle
-- | resumes from the current stream position.
-- |
-- | Semantics:
-- |
-- |  * If the input stream is empty, the output stream is empty
-- |    (no phantom "empty chunk" is emitted).
-- |  * If the stream ends while the sink is mid-consumption, the
-- |    sink's `finish` runs and its value is emitted as the
-- |    final element.
-- |  * If the sink halts without ever consuming, one virtual
-- |    input is dropped per emitted value to guarantee progress;
-- |    the output stream then mirrors the input length.
-- |
-- | Mirrors ZIO's `ZStream.aggregate` and Effect-TS
-- | `Stream.aggregate`. The classic batching idiom is
-- | `aggregate (take n)` to chop the upstream into `n`-sized
-- | chunks.
-- |
-- | ```purescript
-- | chunked :: Stream r e (Array Int)
-- | chunked = aggregate (take 3) (fromArray [1, 2, 3, 4, 5, 6, 7])
-- | -- chunks: [[1,2,3], [4,5,6], [7]]
-- | ```
aggregate
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> Stream r e b
aggregate sinkTpl inStream = Stream.Stream (newCycle inStream)
  where
  newCycle :: Stream r e a -> RIO r e (Stream.Step r e b)
  newCycle stream = do
    sstep <- unSink sinkTpl
    case sstep of
      Halt b -> do
        instep <- Stream.unStream stream
        case instep of
          Stream.Done -> pure Stream.Done
          Stream.Yield _ rest ->
            pure (Stream.Yield b (Stream.Stream (newCycle rest)))
      Need k _ -> do
        instep <- Stream.unStream stream
        case instep of
          Stream.Done -> pure Stream.Done
          Stream.Yield a rest -> continuing (k a) rest

  continuing :: Sink r e a b -> Stream r e a -> RIO r e (Stream.Step r e b)
  continuing sink stream = do
    sstep <- unSink sink
    case sstep of
      Halt b ->
        pure (Stream.Yield b (Stream.Stream (newCycle stream)))
      Need k finish -> do
        instep <- Stream.unStream stream
        case instep of
          Stream.Done -> do
            b <- finish
            pure (Stream.Yield b Stream.empty)
          Stream.Yield a rest -> continuing (k a) rest

-- | Alias for `aggregate` matching ZIO's older `transduce`
-- | naming. Provided so code ported from ZIO snippets reads
-- | the same; reach for `aggregate` in new code.
transduce
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> Stream r e b
transduce = aggregate

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

-- | Effectful contravariant input map. Each input is first
-- | transformed by the effectful function before being fed to the
-- | underlying sink. The transformation runs in `RIO`, so it can
-- | read services, raise typed failures, or sleep.
contramapM
  :: forall r e i j a
   . (j -> RIO r e i)
  -> Sink r e i a
  -> Sink r e j a
contramapM f sink = Sink do
  step <- unSink sink
  case step of
    Halt a -> pure (Halt a)
    Need k finish ->
      pure
        ( Need
            ( \j -> Sink do
                i <- f j
                unSink (contramapM f (k i))
            )
            finish
        )

-- | Run an effectful action on every element pulled. Returns
-- | `unit` when the stream ends. Useful for sending each item to
-- | a queue or hub.
foreach :: forall r e i. (i -> RIO r e Unit) -> Sink r e i Unit
foreach f = Sink
  ( pure
      ( Need
          ( \i -> Sink do
              f i
              unSink (foreach f)
          )
          (pure unit)
      )
  )

-- | Sum every element. Empty stream sums to `zero`.
sum :: forall r e i. Semiring i => Sink r e i i
sum = foldL zero (+)

-- | Multiply every element. Empty stream products to `one`.
product :: forall r e i. Semiring i => Sink r e i i
product = foldL one (*)

-- | The minimum element, or `Nothing` on an empty stream.
minimum :: forall r e i. Ord i => Sink r e i (Maybe i)
minimum = go Nothing
  where
  go acc = Sink (pure (Need (step acc) (pure acc)))
  step acc i = case acc of
    Nothing -> go (Just i)
    Just curr -> go (Just (min curr i))

-- | The maximum element, or `Nothing` on an empty stream.
maximum :: forall r e i. Ord i => Sink r e i (Maybe i)
maximum = go Nothing
  where
  go acc = Sink (pure (Need (step acc) (pure acc)))
  step acc i = case acc of
    Nothing -> go (Just i)
    Just curr -> go (Just (max curr i))

-- | Concatenate every element under its `Monoid`. Empty stream
-- | yields `mempty`.
mconcat :: forall r e i. Monoid i => Sink r e i i
mconcat = foldL mempty append

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
-- | no per-consumer queue. Use `broadcast` from `RIO.Aff.Stream.Par`
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
