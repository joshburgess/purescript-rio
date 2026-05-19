-- | Pull-based asynchronous streams.
-- |
-- | A `Stream r e a` is built around a single primitive: pull the
-- | next step. Each pull runs in `RIO r e` and either yields the
-- | head plus the continuation, or signals end-of-stream. This keeps
-- | the implementation small while supporting both bounded and
-- | unbounded producers, backpressure (each pull is explicit), and
-- | failure (the pull RIO can raise typed errors or defects).
-- |
-- | The MVP ships construction helpers (`empty`, `emit`, `fromArray`,
-- | `repeatRIO`, `fromQueue`), the obvious transforms (`map`,
-- | `filter`, `take`), and three terminations (`run`, `runCollect`,
-- | `fold`). Concurrency-shaped operators (merge, zipPar, channel)
-- | are left to later phases.
module RIO.Fiber.Stream
  ( Stream(..)
  , Step(..)
  , empty
  , emit
  , fromArray
  , repeatRIO
  , fromQueue
  , map
  , filter
  , take
  , fold
  , forEach
  , run
  , runCollect
  , buffer
  , merge
  , mapPar
  , scan
  , groupBy
  , zipPar
  ) where

import Prelude hiding (map)

import Data.Array (index, range, snoc, uncons, zipWith)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q

-- | A stream is a producer of `Step`s. Each pull is an `RIO` that
-- | yields either the next element + continuation or `Done`.
newtype Stream r e a = Stream (RIO r e (Step r e a))

-- | A single step in the pull protocol.
data Step r e a
  = Done
  | Yield a (Stream r e a)

-- | The empty stream. The first pull immediately signals `Done`.
empty :: forall r e a. Stream r e a
empty = Stream (pure Done)

-- | A stream that yields exactly one element, then halts.
emit :: forall r e a. a -> Stream r e a
emit a = Stream (pure (Yield a empty))

-- | A stream that yields each element of the array in order, then halts.
fromArray :: forall r e a. Array a -> Stream r e a
fromArray xs = Stream
  ( pure case uncons xs of
      Nothing -> Done
      Just { head, tail } -> Yield head (fromArray tail)
  )

-- | A stream that yields the result of running `action` on every
-- | pull. Infinite: bound it with `take` or short-circuit via
-- | interruption.
repeatRIO :: forall r e a. RIO r e a -> Stream r e a
repeatRIO action = Stream do
  a <- action
  pure (Yield a (repeatRIO action))

-- | A stream that pulls from the given queue. Each pull suspends
-- | until an element is available. Infinite: queues have no close
-- | signal, so bound externally.
fromQueue :: forall r e a. Queue a -> Stream r e a
fromQueue q = Stream do
  a <- Q.take q
  pure (Yield a (fromQueue q))

-- | Transform every element with `f`. Lazy: the function runs as
-- | elements are pulled.
map :: forall r e a b. (a -> b) -> Stream r e a -> Stream r e b
map f (Stream pull) = Stream do
  s <- pull
  pure case s of
    Done -> Done
    Yield a rest -> Yield (f a) (map f rest)

-- | Keep only elements satisfying `p`. Pulls the upstream stream
-- | until a matching element appears or it ends.
filter :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
filter p (Stream pull) = Stream (loop pull)
  where
  loop next = do
    s <- next
    case s of
      Done -> pure Done
      Yield a (Stream rest)
        | p a -> pure (Yield a (filter p (Stream rest)))
        | otherwise -> loop rest

-- | Take at most `n` elements from the stream. Non-positive `n`
-- | becomes the empty stream.
take :: forall r e a. Int -> Stream r e a -> Stream r e a
take n s
  | n <= 0 = empty
  | otherwise = case s of
      Stream pull -> Stream do
        step <- pull
        pure case step of
          Done -> Done
          Yield a rest -> Yield a (take (n - 1) rest)

-- | Reduce the stream with `step`, starting from `seed`. Pulls
-- | until the stream signals `Done`.
fold :: forall r e a b. (b -> a -> b) -> b -> Stream r e a -> RIO r e b
fold step seed (Stream pull) = do
  s <- pull
  case s of
    Done -> pure seed
    Yield a rest -> fold step (step seed a) rest

-- | Run an `RIO` action for every element. Returns when the stream
-- | is exhausted.
forEach :: forall r e a. (a -> RIO r e Unit) -> Stream r e a -> RIO r e Unit
forEach f (Stream pull) = do
  s <- pull
  case s of
    Done -> pure unit
    Yield a rest -> do
      f a
      forEach f rest

-- | Drain the stream, discarding every element. Useful for streams
-- | run for their side effects.
run :: forall r e a. Stream r e a -> RIO r e Unit
run = forEach (\_ -> pure unit)

-- | Pull every element into an array. Use only on bounded streams.
runCollect :: forall r e a. Stream r e a -> RIO r e (Array a)
runCollect = fold snoc []

-- | Insert a bounded buffer of size `n` between producer and
-- | consumer. The producer runs in a forked fiber that fills the
-- | buffer; the consumer pulls from it. Lets a fast consumer ride
-- | ahead of a bursty producer (and vice versa) without blocking
-- | each pull on the slowest one.
-- |
-- | The producer signals end-of-stream by enqueuing `Nothing`; if
-- | the producer raises before that, the consumer will block. For
-- | now, callers should ensure the source is total or use this only
-- | with side-effect-free transforms upstream.
buffer :: forall r e a. Int -> Stream r e a -> Stream r e a
buffer n source = Stream do
  q <- F.liftEffect (Q.make (max 1 n) :: _ (Q.Queue (Maybe a)))
  _ <- F.fork do
    forEach (\a -> Q.offer q (Just a)) source
    Q.offer q Nothing
  case fromQueueWithSentinel q of
    Stream pull -> pull
  where
  fromQueueWithSentinel :: Queue (Maybe a) -> Stream r e a
  fromQueueWithSentinel q = Stream do
    m <- Q.take q
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (fromQueueWithSentinel q))

-- | Non-deterministically interleave two streams. Both producers
-- | run concurrently into a shared buffer; the consumer sees an
-- | arbitrary interleaving. The result terminates when both
-- | upstreams have ended.
merge :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
merge sl sr = Stream do
  q <- F.liftEffect (Q.make 16 :: _ (Q.Queue (Maybe a)))
  let
    pump source = do
      forEach (\a -> Q.offer q (Just a)) source
      Q.offer q Nothing
  _ <- F.fork (pump sl)
  _ <- F.fork (pump sr)
  case drainQueueN q 2 of
    Stream pull -> pull
  where
  drainQueueN :: Queue (Maybe a) -> Int -> Stream r e a
  drainQueueN q remaining = Stream do
    m <- Q.take q
    case m of
      Nothing ->
        if remaining <= 1 then pure Done
        else case drainQueueN q (remaining - 1) of
          Stream pull -> pull
      Just a -> pure (Yield a (drainQueueN q remaining))

-- | Map elements with up to `concurrency` workers running in
-- | parallel. Workers are pre-spawned and fed by a round-robin
-- | dispatcher; results are reordered downstream by the same
-- | round-robin, so the output preserves input order. A slow
-- | element blocks downstream emission past its position but does
-- | not stall workers handling later elements.
mapPar
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
mapPar concurrency f source = Stream do
  let n = max 1 concurrency
  reqs <- F.liftEffect (traverse (\_ -> Q.make 1) (range 0 (n - 1)) :: _ (Array (Q.Queue (Maybe a))))
  resps <- F.liftEffect (traverse (\_ -> Q.make 1) (range 0 (n - 1)) :: _ (Array (Q.Queue (Maybe b))))
  let pairs = zipWith Tuple reqs resps
  traverse_ (\(Tuple req resp) -> F.fork (worker req resp)) pairs
  _ <- F.fork (dispatcher 0 source reqs)
  case readResponses 0 resps of
    Stream pull -> pull
  where
  worker req resp = do
    m <- Q.take req
    case m of
      Nothing -> Q.offer resp Nothing
      Just a -> do
        b <- f a
        Q.offer resp (Just b)
        worker req resp

  dispatcher i (Stream pull) reqs = do
    step <- pull
    case step of
      Done -> traverse_ (\q -> Q.offer q Nothing) reqs
      Yield a rest -> case index reqs (i `mod` Array.length reqs) of
        Nothing -> pure unit
        Just q -> do
          Q.offer q (Just a)
          dispatcher (i + 1) rest reqs

  readResponses cursor resps = Stream do
    case index resps (cursor `mod` Array.length resps) of
      Nothing -> pure Done
      Just q -> do
        m <- Q.take q
        case m of
          Nothing -> pure Done
          Just b -> pure (Yield b (readResponses (cursor + 1) resps))

-- | Yield the array in order, then continue with `after`.
fromArrayThen :: forall r e a. Array a -> Stream r e a -> Stream r e a
fromArrayThen xs after = case uncons xs of
  Nothing -> after
  Just { head, tail } ->
    Stream (pure (Yield head (fromArrayThen tail after)))

-- | Running fold: emit `seed`, then for every element `a` emit
-- | `step prev a` where `prev` is the previous emission. The output
-- | stream has one more element than the input plus the seed.
scan :: forall r e a b. (b -> a -> b) -> b -> Stream r e a -> Stream r e b
scan step seed source = Stream (pure (Yield seed (go seed source)))
  where
  go acc (Stream pull) = Stream do
    s <- pull
    pure case s of
      Done -> Done
      Yield a rest ->
        let next = step acc a
        in Yield next (go next rest)

-- | Partition the input into adjacent runs sharing a key. Each
-- | emitted element is a non-empty array; consecutive arrays have
-- | different keys.
groupBy
  :: forall r e a k
   . Eq k
  => (a -> k)
  -> Stream r e a
  -> Stream r e (Array a)
groupBy key (Stream pull) = Stream do
  s <- pull
  case s of
    Done -> pure Done
    Yield a rest -> collect (key a) [ a ] rest
  where
  collect k acc (Stream nextPull) = do
    s <- nextPull
    case s of
      Done -> pure (Yield acc empty)
      Yield a rest
        | key a == k -> collect k (snoc acc a) rest
        | otherwise ->
            pure (Yield acc (Stream (collect (key a) [ a ] rest)))

-- | Run two streams in parallel, pairing elements positionally. The
-- | output ends as soon as either side ends.
zipPar :: forall r e a b. Stream r e a -> Stream r e b -> Stream r e (Tuple a b)
zipPar (Stream pullA) (Stream pullB) = Stream do
  Tuple sa sb <- F.zipPar pullA pullB
  case sa, sb of
    Done, _ -> pure Done
    _, Done -> pure Done
    Yield a restA, Yield b restB ->
      pure (Yield (Tuple a b) (zipPar restA restB))
