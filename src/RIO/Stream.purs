-- | A minimal pull-based stream.
-- |
-- | A `Stream r e a` is a `RIO r e`-flavoured iterator: each step
-- | either yields a value and a continuation, or signals
-- | end-of-stream. All operations are written as direct
-- | recursions on the step function rather than going through a
-- | free encoding, so traces stay readable and there is no fusion
-- | machinery to reason about.
-- |
-- | This module is the base pull layer: one input channel,
-- | direct-recursion combinators, no fusion machinery. Composable
-- | consumers live in `RIO.Sink`; parallel combinators (merge,
-- | broadcast, partition) live in `RIO.Stream.Par`; dynamic
-- | broadcast over a `RIO.Hub` lives in `RIO.Stream.Concurrent`;
-- | resource-acquiring single-element streams live in
-- | `RIO.Stream.Resource`. The shape here is enough on its own for
-- | "pull rows from a Postgres cursor and write JSON to stdout"
-- | without rewriting `for_` over an `Array`.
-- |
-- | ```purescript
-- | -- pull every row out of a query result, transform, drain
-- | runDrain
-- |   ( fromArray [1, 2, 3, 4, 5]
-- |       # filter (\n -> n `mod` 2 == 0)
-- |       # map (_ * 10)
-- |       # mapM (\n -> logInfo (show n) *> pure n)
-- |   )
-- | ```
module RIO.Stream
  ( Stream(..)
  , Step(..)
  , unStream
  , append
  , chunk
  , collectSome
  , concat
  , cons
  , distinct
  , drop
  , dropUntil
  , dropWhile
  , empty
  , filter
  , filterM
  , find
  , flatMap
  , flatten
  , forever
  , fromArray
  , fromHub
  , fromQueue
  , groupBy
  , haltWhen
  , intoHub
  , intoQueue
  , interruptWhen
  , head
  , intersperse
  , iterate
  , iterateM
  , last
  , map
  , mapAccum
  , mapM
  , range
  , repeat
  , repeatM
  , runCollect
  , runDrain
  , runFold
  , runFoldM
  , scan
  , scanM
  , single
  , sliding
  , tap
  , tapError
  , take
  , takeUntil
  , takeWhile
  , tick
  , unfoldM
  , zip
  , zipWith
  , zipWithIndex
  ) where

import Prelude hiding (map)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect.Aff (Milliseconds)

import RIO.Clock (Clock)
import RIO.Clock as Clock
import RIO.Concurrency (race)
import RIO.Core (RIO)
import RIO.Deferred (Deferred, awaitDeferred, pollDeferred)
import RIO.Error (rethrow)
import RIO.Error as Error
import RIO.Hub (Hub)
import RIO.Hub as Hub
import RIO.Queue (Queue)
import RIO.Queue as Queue

-- | One step of a stream: either a value paired with the rest of
-- | the stream, or end-of-stream.
data Step r e a
  = Yield a (Stream r e a)
  | Done

-- | A pull-based stream. Run the inner `RIO` to get the next step.
-- |
-- | The constructor and `unStream` are exposed so that companion
-- | modules (e.g. `RIO.Stream.Par`) can build new combinators that
-- | need to step the underlying `RIO`. End-user code should reach
-- | for the combinators in this module rather than peeling the
-- | newtype directly.
newtype Stream :: Row Type -> Row Type -> Type -> Type
newtype Stream r e a = Stream (RIO r e (Step r e a))

unStream :: forall r e a. Stream r e a -> RIO r e (Step r e a)
unStream (Stream s) = s

-- | The empty stream.
empty :: forall r e a. Stream r e a
empty = Stream (pure Done)

-- | A single-element stream.
single :: forall r e a. a -> Stream r e a
single a = Stream (pure (Yield a empty))

-- | Prepend an element to a stream. The result yields `a` first,
-- | then every element of `s`. Constant-time: no work is done on
-- | the tail until it is pulled.
cons :: forall r e a. a -> Stream r e a -> Stream r e a
cons a s = Stream (pure (Yield a s))

-- | Append an element after every element of a stream. The result
-- | yields the input first, then `a` once the input has finished.
-- |
-- | Lazy: the input is not run until pulled. On an infinite input
-- | the appended element is never reached, which is consistent
-- | with `concat s (single a)`.
append :: forall r e a. Stream r e a -> a -> Stream r e a
append s a = Stream do
  step <- unStream s
  case step of
    Done -> pure (Yield a empty)
    Yield x rest -> pure (Yield x (append rest a))

-- | A stream of the elements of an array, in input order.
fromArray :: forall r e a. Array a -> Stream r e a
fromArray xs = case Array.uncons xs of
  Nothing -> empty
  Just { head, tail } -> Stream (pure (Yield head (fromArray tail)))

-- | Build a stream by repeatedly stepping a seed state. Each step
-- | either returns a new value plus the next seed, or `Nothing` to
-- | end the stream.
unfoldM
  :: forall r e s a
   . s
  -> (s -> RIO r e (Maybe (Tuple a s)))
  -> Stream r e a
unfoldM seed step = Stream do
  m <- step seed
  case m of
    Nothing -> pure Done
    Just (Tuple a seed') -> pure (Yield a (unfoldM seed' step))

-- | An infinite stream of values produced by a `RIO` action.
repeatM :: forall r e a. RIO r e a -> Stream r e a
repeatM ra = Stream do
  a <- ra
  pure (Yield a (repeatM ra))

-- | An infinite stream of the same value, repeated forever.
-- |
-- | Defined via `unfoldM` so the recursive step only fires when a
-- | consumer pulls; a plain `Stream (pure (Yield a (repeat a)))`
-- | would loop forever during construction under strict argument
-- | evaluation.
repeat :: forall r e a. a -> Stream r e a
repeat a = unfoldM unit \_ -> pure (Just (Tuple a unit))

-- | Integers from `start` to `end` inclusive. If `end < start` the
-- | stream is empty.
range :: forall r e. Int -> Int -> Stream r e Int
range start end = unfoldM start \n ->
  if n > end then pure Nothing
  else pure (Just (Tuple n (n + 1)))

-- | Infinite stream `seed, f seed, f (f seed), ...`. Pure iteration.
iterate :: forall r e a. a -> (a -> a) -> Stream r e a
iterate seed f = unfoldM seed \n -> pure (Just (Tuple n (f n)))

-- | Infinite stream `seed, f seed, f (f seed), ...` with an
-- | effectful step. The step runs once per element pulled.
iterateM :: forall r e a. a -> (a -> RIO r e a) -> Stream r e a
iterateM seed f = unfoldM seed \n -> do
  next <- f n
  pure (Just (Tuple n next))

-- | Stream of `take`s from a `RIO.Queue`. Yields each item the
-- | queue delivers and terminates when the queue is shut down.
fromQueue :: forall r e a. Queue a -> Stream r e a
fromQueue q = Stream do
  m <- Queue.take q
  case m of
    Nothing -> pure Done
    Just a -> pure (Yield a (fromQueue q))

-- | Stream of items from a fresh subscription to a `RIO.Hub`. A
-- | private queue is allocated lazily on the first pull and held
-- | for the lifetime of the stream. Terminates when the queue is
-- | shut down (which `subscribe`'s `unsubscribe` does not do, so
-- | the stream is infinite by default; pair with `take`,
-- | `takeWhile`, or scope-managed teardown for a finite consumer).
fromHub :: forall r e a. Hub a -> Stream r e a
fromHub hub = Stream do
  sub <- Hub.subscribe hub
  unStream (fromQueue sub.queue)

-- | Drain the stream into a `RIO.Queue`, offering each yielded
-- | element in order. Returns once the stream ends or the queue is
-- | shut down (an `offer` returning `false` stops the drain
-- | immediately; the unyielded tail of the stream is discarded but
-- | any scoped finalizers still release).
-- |
-- | Pairs with `fromQueue` for stream-to-queue handoff. On bounded
-- | queues this naturally backpressures: `offer` blocks at capacity,
-- | which slows the stream pull.
intoQueue :: forall r e a. Queue a -> Stream r e a -> RIO r e Unit
intoQueue q s = do
  step <- unStream s
  case step of
    Done -> pure unit
    Yield a rest -> do
      ok <- Queue.offer q a
      if ok then intoQueue q rest
      else pure unit

-- | Drain the stream into a `RIO.Hub`, publishing each yielded
-- | element to every current subscriber. Returns once the stream
-- | ends; the stream's own back-pressure mechanism applies to the
-- | rate of publication.
-- |
-- | Pairs with `fromHub` for stream-to-hub fan-out. Subscribers
-- | added after publication has begun see only subsequent items;
-- | subscribe before starting `intoHub` to receive the full run.
intoHub :: forall r e a. Hub a -> Stream r e a -> RIO r e Unit
intoHub hub s = do
  step <- unStream s
  case step of
    Done -> pure unit
    Yield a rest -> do
      Hub.publish hub a
      intoHub hub rest

-- | Emit `unit` every `interval` milliseconds, sleeping on the
-- | `Clock` service between yields. Infinite.
tick
  :: forall r e
   . Milliseconds
  -> Stream (clock :: Clock | r) e Unit
tick interval = Stream do
  Clock.sleep interval
  pure (Yield unit (tick interval))

-- | Map a pure function over every element.
map :: forall r e a b. (a -> b) -> Stream r e a -> Stream r e b
map f s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield (f a) (map f rest))

-- | Map an effectful function over every element.
mapM
  :: forall r e a b
   . (a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
mapM f s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> do
      b <- f a
      pure (Yield b (mapM f rest))

-- | Map while threading a running accumulator through each element.
-- |
-- | The step function receives the current accumulator and the
-- | element, and returns the next accumulator paired with the
-- | emitted value. Useful for stateful transforms (numbering, deltas,
-- | running sums that emit per-step values) where `scan` would only
-- | expose the accumulator and `map` carries no state.
mapAccum
  :: forall r e s a b
   . s
  -> (s -> a -> Tuple s b)
  -> Stream r e a
  -> Stream r e b
mapAccum seed step s = Stream do
  inner <- unStream s
  case inner of
    Done -> pure Done
    Yield a rest -> case step seed a of
      Tuple seed' b -> pure (Yield b (mapAccum seed' step rest))

-- | Run an effect for each element, then pass the element through
-- | unchanged. Useful for tracing, metrics, or side-channel logging
-- | inside a pipeline without disturbing the values flowing through.
tap
  :: forall r e a
   . (a -> RIO r e Unit)
  -> Stream r e a
  -> Stream r e a
tap f s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> do
      f a
      pure (Yield a (tap f rest))

-- | Run an effect on every typed failure surfaced while pulling the
-- | stream, then re-raise the failure unchanged. Defects flow
-- | through without invoking the handler (same policy as
-- | `RIO.Error.tapError`).
-- |
-- | The handler fires per failed pull, so a stream that recovers
-- | partway is observable element-by-element.
tapError
  :: forall r e a
   . (Variant e -> RIO r e Unit)
  -> Stream r e a
  -> Stream r e a
tapError f s = Stream do
  step <- Error.tapError f (unStream s)
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield a (tapError f rest))

-- | Keep elements for which the predicate is true.
filter
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
filter p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then pure (Yield a (filter p rest))
      else unStream (filter p rest)

-- | Filter with an effectful predicate. The predicate runs in the
-- | same `RIO r e` as the stream's pull effect, so it can read
-- | services, hit refs, or itself fail with a typed error (which
-- | aborts the stream).
filterM
  :: forall r e a
   . (a -> RIO r e Boolean)
  -> Stream r e a
  -> Stream r e a
filterM p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> do
      keep <- p a
      if keep then pure (Yield a (filterM p rest))
      else unStream (filterM p rest)

-- | Filter + map in one pass: keep the elements where the function
-- | returns `Just`, replacing them with the inner value. Elements
-- | producing `Nothing` are dropped silently. The `filterMap`
-- | from `Data.Array`, lifted to a stream.
collectSome
  :: forall r e a b
   . (a -> Maybe b)
  -> Stream r e a
  -> Stream r e b
collectSome f s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> case f a of
      Just b -> pure (Yield b (collectSome f rest))
      Nothing -> unStream (collectSome f rest)

-- | Concatenate two streams: drain the first, then drain the
-- | second.
concat :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
concat l r = Stream do
  step <- unStream l
  case step of
    Done -> unStream r
    Yield a rest -> pure (Yield a (concat rest r))

-- | Replace each element with a stream and concatenate.
flatMap
  :: forall r e a b
   . Stream r e a
  -> (a -> Stream r e b)
  -> Stream r e b
flatMap s f = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> unStream (concat (f a) (flatMap rest f))

-- | Take the first `n` elements.
take :: forall r e a. Int -> Stream r e a -> Stream r e a
take n s
  | n <= 0 = empty
  | otherwise = Stream do
      step <- unStream s
      case step of
        Done -> pure Done
        Yield a rest -> pure (Yield a (take (n - 1) rest))

-- | Gracefully halt the stream when `sentinel` is fired. Each
-- | step polls the deferred before pulling: if it is still
-- | empty, the upstream is pulled normally; if it has been
-- | filled with success, the stream emits `Done`; if it has
-- | been filled with a typed failure, that failure is raised
-- | on the error row.
-- |
-- | "Graceful" means the *next* pull observes the halt; an
-- | in-flight pull is allowed to complete. For the variant that
-- | races the pull against the sentinel and interrupts the
-- | upstream as soon as the sentinel fires, see
-- | `interruptWhen`.
-- |
-- | Mirrors ZIO `ZStream.haltWhen` and Effect-TS
-- | `Stream.haltWhen`.
-- |
-- | ```purescript
-- | stop <- makeDeferred
-- | -- ... arrange for someone to `succeedDeferred stop unit`
-- | runDrain (haltWhen stop (forever pollOnce))
-- | ```
haltWhen :: forall r e a. Deferred e Unit -> Stream r e a -> Stream r e a
haltWhen sentinel s = Stream do
  poll <- pollDeferred sentinel
  case poll of
    Just (Left v) -> rethrow v
    Just (Right _) -> pure Done
    Nothing -> do
      step <- unStream s
      case step of
        Done -> pure Done
        Yield a rest -> pure (Yield a (haltWhen sentinel rest))

-- | Interrupt the stream when `sentinel` is fired by racing the
-- | sentinel's completion against each upstream pull. If the
-- | sentinel wins, the in-flight pull is cancelled (under `race`
-- | semantics) and the stream emits `Done`; if the sentinel
-- | fails, that failure is raised on the error row.
-- |
-- | The difference from `haltWhen`: `interruptWhen` will
-- | terminate a pull that has already started; `haltWhen` will
-- | only observe the halt between pulls.
-- |
-- | Mirrors ZIO `ZStream.interruptWhen` and Effect-TS
-- | `Stream.interruptWhen`.
interruptWhen :: forall r e a. Deferred e Unit -> Stream r e a -> Stream r e a
interruptWhen sentinel s = Stream do
  -- Pre-pull poll: if the sentinel has already fired, observe it
  -- synchronously rather than depending on the race scheduler to
  -- pick the completed side over a synchronous pull.
  poll <- pollDeferred sentinel
  case poll of
    Just (Left v) -> rethrow v
    Just (Right _) -> pure Done
    Nothing -> race
      (awaitDeferred sentinel *> pure Done)
      ( do
          step <- unStream s
          case step of
            Done -> pure Done
            Yield a rest -> pure (Yield a (interruptWhen sentinel rest))
      )

-- | Drop the first `n` elements.
drop :: forall r e a. Int -> Stream r e a -> Stream r e a
drop n s
  | n <= 0 = s
  | otherwise = Stream do
      step <- unStream s
      case step of
        Done -> pure Done
        Yield _ rest -> unStream (drop (n - 1) rest)

-- | Drain the stream, discarding every value.
runDrain :: forall r e a. Stream r e a -> RIO r e Unit
runDrain s = do
  step <- unStream s
  case step of
    Done -> pure unit
    Yield _ rest -> runDrain rest

-- | Drain the stream and collect every value into an array.
runCollect :: forall r e a. Stream r e a -> RIO r e (Array a)
runCollect = go []
  where
  go :: Array a -> Stream r e a -> RIO r e (Array a)
  go acc s = do
    step <- unStream s
    case step of
      Done -> pure acc
      Yield a rest -> go (Array.snoc acc a) rest

-- | Left fold over the stream with a pure accumulator step.
runFold
  :: forall r e a b
   . b
  -> (b -> a -> b)
  -> Stream r e a
  -> RIO r e b
runFold seed step s = do
  st <- unStream s
  case st of
    Done -> pure seed
    Yield a rest -> runFold (step seed a) step rest

-- | Pair two streams elementwise. Ends when either input ends, so
-- | the output length is the minimum of the two inputs.
-- |
-- | Both streams are stepped on each output, in left-then-right
-- | order. There is no buffering: this is a strict zip on a pull
-- | stream.
zip
  :: forall r e a b
   . Stream r e a
  -> Stream r e b
  -> Stream r e (Tuple a b)
zip = zipWith Tuple

-- | Combine two streams elementwise with a function. Ends when
-- | either input ends.
zipWith
  :: forall r e a b c
   . (a -> b -> c)
  -> Stream r e a
  -> Stream r e b
  -> Stream r e c
zipWith f l r = Stream do
  stepL <- unStream l
  case stepL of
    Done -> pure Done
    Yield a restL -> do
      stepR <- unStream r
      case stepR of
        Done -> pure Done
        Yield b restR -> pure (Yield (f a b) (zipWith f restL restR))

-- | Pair each element with its 0-based position in the stream.
zipWithIndex
  :: forall r e a
   . Stream r e a
  -> Stream r e (Tuple Int a)
zipWithIndex = go 0
  where
  go :: Int -> Stream r e a -> Stream r e (Tuple Int a)
  go i s = Stream do
    step <- unStream s
    case step of
      Done -> pure Done
      Yield a rest -> pure (Yield (Tuple i a) (go (i + 1) rest))

-- | Running prefix fold. Emits the seed first, then one element
-- | per input element with the accumulator applied. The output is
-- | always one element longer than the input.
-- |
-- | ```purescript
-- | -- prefix sums of [1, 2, 3] starting at 0 yields [0, 1, 3, 6]
-- | runCollect (scan 0 (+) (fromArray [1, 2, 3]))
-- | ```
scan
  :: forall r e a b
   . b
  -> (b -> a -> b)
  -> Stream r e a
  -> Stream r e b
scan seed step s = Stream do
  pure (Yield seed (scanRest seed step s))
  where
  scanRest :: b -> (b -> a -> b) -> Stream r e a -> Stream r e b
  scanRest acc f t = Stream do
    st <- unStream t
    case st of
      Done -> pure Done
      Yield a rest ->
        let
          acc' = f acc a
        in
          pure (Yield acc' (scanRest acc' f rest))

-- | Effectful variant of `scan`. Each step yields the updated
-- | accumulator and runs in `RIO r e`.
scanM
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
scanM seed step s = Stream do
  pure (Yield seed (go seed step s))
  where
  go :: b -> (b -> a -> RIO r e b) -> Stream r e a -> Stream r e b
  go acc f t = Stream do
    st <- unStream t
    case st of
      Done -> pure Done
      Yield a rest -> do
        acc' <- f acc a
        pure (Yield acc' (go acc' f rest))

-- | Group consecutive elements into arrays of size `n`. The final
-- | chunk may be shorter when the input length is not a multiple
-- | of `n`. `n <= 0` produces an empty stream.
chunk :: forall r e a. Int -> Stream r e a -> Stream r e (Array a)
chunk n s
  | n <= 0 = empty
  | otherwise = chunkGo n [] s

chunkGo :: forall r e a. Int -> Array a -> Stream r e a -> Stream r e (Array a)
chunkGo n acc t = Stream do
  step <- unStream t
  case step of
    Done ->
      if Array.null acc then pure Done
      else pure (Yield acc empty)
    Yield a rest ->
      let
        acc' = Array.snoc acc a
      in
        if Array.length acc' >= n then pure (Yield acc' (chunkGo n [] rest))
        else unStream (chunkGo n acc' rest)

-- | A sliding window of size `n` over the stream. The window
-- | advances by one element per output: every yielded array
-- | overlaps the previous one in `n - 1` positions.
-- |
-- | * When `n <= 0`, the result is empty.
-- | * When the input has fewer than `n` elements, no windows are
-- |   emitted (use `chunk` if you need the partial trailing block).
-- | * Each emitted array has length exactly `n`.
-- |
-- | Memory is bounded by `n`: only the current window is kept.
sliding :: forall r e a. Int -> Stream r e a -> Stream r e (Array a)
sliding n s
  | n <= 0 = empty
  | otherwise = slidingGo n [] s

slidingGo
  :: forall r e a
   . Int
  -> Array a
  -> Stream r e a
  -> Stream r e (Array a)
slidingGo n buf t = Stream do
  step <- unStream t
  case step of
    Done -> pure Done
    Yield a rest ->
      let
        buf' = Array.snoc buf a
        len = Array.length buf'
      in
        if len < n then unStream (slidingGo n buf' rest)
        else if len == n then pure (Yield buf' (slidingGo n buf' rest))
        else
          let
            buf'' = Array.drop 1 buf'
          in
            pure (Yield buf'' (slidingGo n buf'' rest))

-- | Take elements while the predicate holds. Stops at (and does
-- | not emit) the first element for which `p` is false.
takeWhile
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
takeWhile p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then pure (Yield a (takeWhile p rest))
      else pure Done

-- | Drop elements while the predicate holds, then yield the rest
-- | unchanged. The first failing element is included in the
-- | output.
dropWhile
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
dropWhile p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then unStream (dropWhile p rest)
      else pure (Yield a rest)

-- | Take elements until the predicate first holds. The triggering
-- | element *is* emitted (then the stream stops). If the predicate
-- | never holds, the entire input is forwarded.
-- |
-- | This is the inclusive complement of `takeWhile`:
-- |   * `takeWhile p` stops *at* the first element where `p` is
-- |     false; that element is dropped.
-- |   * `takeUntil p` stops *after* the first element where `p` is
-- |     true; that element is kept.
takeUntil
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
takeUntil p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then pure (Yield a empty)
      else pure (Yield a (takeUntil p rest))

-- | Drop elements until the predicate first holds, then yield the
-- | rest. The triggering element is *also dropped*; the first
-- | emitted element is the one *after* the trigger.
-- |
-- | This is the exclusive complement of `dropWhile`:
-- |   * `dropWhile p` keeps the first element where `p` is false.
-- |   * `dropUntil p` keeps the first element after the one where
-- |     `p` is true.
dropUntil
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
dropUntil p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then unStream rest
      else unStream (dropUntil p rest)

-- | Insert `sep` between consecutive elements. The output has
-- | `2n - 1` elements when the input has `n` elements; an empty
-- | input stays empty.
intersperse :: forall r e a. a -> Stream r e a -> Stream r e a
intersperse sep s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield a (prefixed rest))
  where
  prefixed :: Stream r e a -> Stream r e a
  prefixed t = Stream do
    step <- unStream t
    case step of
      Done -> pure Done
      Yield a rest -> pure (Yield sep (Stream (pure (Yield a (prefixed rest)))))

-- | Flatten a stream of streams into one stream. Each inner stream
-- | is drained in full before the next is stepped.
flatten :: forall r e a. Stream r e (Stream r e a) -> Stream r e a
flatten ss = flatMap ss identity

-- | Drop consecutive duplicate elements (keeps the first of each
-- | run). Compares with `Eq`.
distinct :: forall r e a. Eq a => Stream r e a -> Stream r e a
distinct s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield a (go a rest))
  where
  go :: a -> Stream r e a -> Stream r e a
  go prev t = Stream do
    step <- unStream t
    case step of
      Done -> pure Done
      Yield a rest ->
        if a == prev then unStream (go prev rest)
        else pure (Yield a (go a rest))

-- | Group consecutive elements for which the relation holds between
-- | each adjacent pair into the same array. A new chunk is emitted
-- | whenever the relation breaks, and the trailing chunk is emitted
-- | when the input ends.
-- |
-- | ```purescript
-- | -- runs of equal numbers
-- | groupBy (==) (fromArray [ 1, 1, 2, 2, 2, 3 ])
-- |   -- yields [ 1, 1 ], [ 2, 2, 2 ], [ 3 ]
-- | ```
-- |
-- | The relation is evaluated against the immediately preceding
-- | element, not against the first element of the current chunk;
-- | this matches the typical "edges between neighbours" reading.
groupBy
  :: forall r e a
   . (a -> a -> Boolean)
  -> Stream r e a
  -> Stream r e (Array a)
groupBy eq s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> unStream (groupByGo eq a [ a ] rest)

groupByGo
  :: forall r e a
   . (a -> a -> Boolean)
  -> a
  -> Array a
  -> Stream r e a
  -> Stream r e (Array a)
groupByGo eq prev acc t = Stream do
  step <- unStream t
  case step of
    Done -> pure (Yield acc empty)
    Yield a rest ->
      if eq prev a then unStream (groupByGo eq a (Array.snoc acc a) rest)
      else pure (Yield acc (groupByGo eq a [ a ] rest))

-- | Pull the first element of the stream, or `Nothing` if the
-- | stream is empty. The rest of the stream is discarded.
-- |
-- | ```purescript
-- | -- check whether the producer has anything to give right now
-- | maybeFirst <- head (fromQueue q)
-- | ```
head :: forall r e a. Stream r e a -> RIO r e (Maybe a)
head s = do
  step <- unStream s
  case step of
    Done -> pure Nothing
    Yield a _ -> pure (Just a)

-- | Drain the stream and return the last element, or `Nothing` if
-- | the stream is empty. Runs the whole stream; do not call on an
-- | infinite stream.
last :: forall r e a. Stream r e a -> RIO r e (Maybe a)
last = go Nothing
  where
  go :: Maybe a -> Stream r e a -> RIO r e (Maybe a)
  go acc s = do
    step <- unStream s
    case step of
      Done -> pure acc
      Yield a rest -> go (Just a) rest

-- | Pull elements until one matches the predicate, return that
-- | element, and discard the rest. Returns `Nothing` if the stream
-- | ends before a match is found.
-- |
-- | Short-circuits on first match: when the input is infinite this
-- | terminates as soon as the predicate fires.
find
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> RIO r e (Maybe a)
find p s = do
  step <- unStream s
  case step of
    Done -> pure Nothing
    Yield a rest ->
      if p a then pure (Just a)
      else find p rest

-- | Repeat a stream forever. After the inner stream ends the same
-- | stream is drained again.
-- |
-- | Idempotent on an already-infinite stream. On an empty stream
-- | this produces an empty stream (the recursion never yields a
-- | value), not a busy loop.
forever :: forall r e a. Stream r e a -> Stream r e a
forever s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield a (concat rest (forever s)))

-- | Left fold with an effectful accumulator step.
runFoldM
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> RIO r e b
runFoldM seed step s = do
  st <- unStream s
  case st of
    Done -> pure seed
    Yield a rest -> do
      seed' <- step seed a
      runFoldM seed' step rest

