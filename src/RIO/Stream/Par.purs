-- | Parallel combinators for `RIO.Stream`.
-- |
-- | The base `RIO.Stream` is intentionally single-channel and
-- | pull-based: each step asks the upstream stream for its next
-- | element. This module adds the "fan in N producers" shape that
-- | a single pull cannot express: `mergeAll` runs every input
-- | stream concurrently and yields values as they arrive on a
-- | shared bounded queue, `merge` is the two-stream convenience,
-- | and `mergeMap` is the `flatMap`-shaped variant that drains
-- | each element's inner stream concurrently.
-- |
-- | `broadcast` is the dual shape: one upstream fanned out to N
-- | consumer streams over bounded per-consumer queues, with the
-- | slowest consumer applying backpressure to the producer.
-- |
-- | `partition` is the routing variant of `broadcast`: each
-- | element goes to exactly one bucket selected by a key function,
-- | so the N consumers see disjoint slices of the input.
-- |
-- | All combinators here share the same failure model: the *first*
-- | typed failure or defect observed in any producer shuts the
-- | shared queue down. Sibling producers are still running at that
-- | point - they'll find the queue closed on their next `offer`
-- | and exit naturally - but their would-be failures are dropped.
-- | If you need every concurrent failure preserved as a tree,
-- | drain each branch separately through
-- | `RIO.Cause.parTraverseCause` instead.
-- |
-- | ```purescript
-- | -- merge three independent producers; output order is whatever
-- | -- arrives on the queue first
-- | runDrain
-- |   ( mergeAll
-- |       [ ticker (Milliseconds 100.0)
-- |       , consumer kafkaPartition0
-- |       , consumer kafkaPartition1
-- |       ]
-- |       # mapM (\msg -> log ("got: " <> show msg))
-- |   )
-- | ```
module RIO.Stream.Par
  ( broadcast
  , merge
  , mergeAll
  , mergeMap
  , partition
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect.Aff (error) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Cause (Cause(..), attemptCause)
import RIO.Concurrency (fork)
import RIO.Internal (RIO(..), mkRIO, rioFail)
import RIO.Queue (Queue)
import RIO.Queue as Queue
import RIO.Stream (Step(..), Stream(..), unStream)

-- | The bounded-queue capacity used by `mergeAll`. Small enough to
-- | apply real backpressure when producers outrun the consumer,
-- | large enough to absorb short bursts without thrashing.
defaultBuffer :: Int
defaultBuffer = 16

-- | Interleave the values of an array of streams. Each input
-- | stream is drained on its own fiber; the output yields values
-- | in the order they land on a shared bounded queue.
-- |
-- | Output order is non-deterministic across inputs but preserves
-- | each input's internal order. An empty input array produces an
-- | empty stream.
mergeAll :: forall r e a. Array (Stream r e a) -> Stream r e a
mergeAll streams =
  let
    n = Array.length streams
  in
    if n == 0 then Stream (pure Done)
    else Stream do
      queue <- liftEffect (Queue.bounded defaultBuffer)
      remainingRef <- liftEffect (Ref.new n)
      failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
      traverse_ (\s -> fork (produce queue remainingRef failureRef s))
        streams
      unStream (consumer queue failureRef)

-- | Two-stream convenience for `mergeAll`.
merge :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
merge a b = mergeAll [ a, b ]

-- | `flatMap`-shaped variant of `mergeAll`. The outer stream is
-- | drained into an array first; each element produces an inner
-- | stream via `f`; all inner streams are then merged with
-- | `mergeAll`.
-- |
-- | This is the natural shape for "fan out each input into a
-- | bounded amount of downstream work" - e.g. each row of a
-- | Postgres cursor turns into a stream of derived records that
-- | should be processed in parallel.
-- |
-- | Caveat: the outer stream is materialised eagerly, which means
-- | this combinator is not suitable for an infinite outer source.
-- | Reach for a `Sink`-style design when that constraint stops
-- | being acceptable.
mergeMap
  :: forall r e a b
   . (a -> Stream r e b)
  -> Stream r e a
  -> Stream r e b
mergeMap f outer = Stream do
  outerValues <- collectOuter outer
  unStream (mergeAll (map f outerValues))
  where
  collectOuter :: Stream r e a -> RIO r e (Array a)
  collectOuter = go []
    where
    go acc s = do
      step <- unStream s
      case step of
        Done -> pure acc
        Yield a rest -> go (Array.snoc acc a) rest

-- | Fan one upstream out to `n` consumer streams, each with its
-- | own bounded buffer of `bufferSize`. Returns the consumer
-- | streams once the producer fiber is running.
-- |
-- | Backpressure is end-to-end: each element is offered to every
-- | subscriber's queue in turn, so the slowest subscriber slows
-- | the producer down (and therefore every other subscriber too).
-- | If you need a slow consumer to fall behind without blocking
-- | the others, allocate `Hub` subscribers directly and skip this
-- | combinator.
-- |
-- | Failure model matches `mergeAll`: the first observed typed
-- | failure or defect on the producer side shuts every subscriber
-- | queue down; each consumer surfaces the same captured cause on
-- | its next pull.
-- |
-- | `n <= 0` returns an empty array immediately and does not touch
-- | the upstream. `bufferSize` is clamped to at least 1.
broadcast
  :: forall r e a
   . Int
  -> Int
  -> Stream r e a
  -> RIO r e (Array (Stream r e a))
broadcast n bufferSize upstream
  | n <= 0 = pure []
  | otherwise = do
      let cap = max 1 bufferSize
      queues <- liftEffect
        (traverse (\_ -> Queue.bounded cap) (Array.range 1 n))
      failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
      _ <- fork (broadcastProducer queues failureRef upstream)
      pure (map (\q -> consumer q failureRef) queues)

-- | Route each upstream element to one of `n` buckets via a key
-- | function. The bucket is `(toBucket x) \`mod\` n` clamped to a
-- | non-negative index, so a key function with arbitrary integer
-- | range routes safely. Each bucket has its own bounded queue of
-- | size `bufferSize` (clamped to at least 1); a full bucket
-- | applies backpressure to the producer.
-- |
-- | Failure model matches `broadcast`: the first observed
-- | producer-side failure or defect shuts every bucket down, and
-- | every consumer surfaces the same captured cause on its next
-- | pull.
-- |
-- | `n <= 0` returns an empty array immediately and does not touch
-- | the upstream.
partition
  :: forall r e a
   . Int
  -> Int
  -> (a -> Int)
  -> Stream r e a
  -> RIO r e (Array (Stream r e a))
partition n bufferSize toBucket upstream
  | n <= 0 = pure []
  | otherwise = do
      let cap = max 1 bufferSize
      queues <- liftEffect
        (traverse (\_ -> Queue.bounded cap) (Array.range 1 n))
      failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
      _ <- fork
        (partitionProducer n queues failureRef toBucket upstream)
      pure (map (\q -> consumer q failureRef) queues)

-- | The producer side of `partition`: drains upstream under
-- | `attemptCause`, routes each element to a single bucket, and
-- | shuts every bucket down on completion or failure.
partitionProducer
  :: forall r e a
   . Int
  -> Array (Queue a)
  -> Ref.Ref (Maybe (Cause e))
  -> (a -> Int)
  -> Stream r e a
  -> RIO r () Unit
partitionProducer n queues failureRef toBucket upstream = do
  outcome <- attemptCause (drainTo upstream)
  case outcome of
    Right _ -> traverse_ Queue.shutdown queues
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      traverse_ Queue.shutdown queues
  where
  drainTo :: Stream r e a -> RIO r e Unit
  drainTo s = do
    step <- unStream s
    case step of
      Done -> pure unit
      Yield a rest -> do
        let raw = toBucket a `mod` n
        let idx = if raw < 0 then raw + n else raw
        case Array.index queues idx of
          Just q -> void (Queue.offer q a)
          Nothing -> pure unit
        drainTo rest

-- | The producer side of `broadcast`: drains upstream under
-- | `attemptCause`. Each value is offered to every subscriber's
-- | queue in input order; a blocked queue applies backpressure to
-- | the whole fan-out. On success, shuts every queue down; on
-- | failure, records the cause and shuts every queue down.
broadcastProducer
  :: forall r e a
   . Array (Queue a)
  -> Ref.Ref (Maybe (Cause e))
  -> Stream r e a
  -> RIO r () Unit
broadcastProducer queues failureRef upstream = do
  outcome <- attemptCause (drainTo upstream)
  case outcome of
    Right _ -> traverse_ Queue.shutdown queues
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      traverse_ Queue.shutdown queues
  where
  drainTo :: Stream r e a -> RIO r e Unit
  drainTo s = do
    step <- unStream s
    case step of
      Done -> pure unit
      Yield a rest -> do
        traverse_ (\q -> Queue.offer q a) queues
        drainTo rest

-- | One producer fiber: drains its input stream onto the shared
-- | queue under `attemptCause`. On success, decrements the
-- | outstanding counter and, if it was the last producer, shuts
-- | the queue down so the consumer sees end-of-stream. On failure
-- | (typed or defect), records the cause and shuts the queue down
-- | immediately so the consumer wakes up and propagates the
-- | failure.
produce
  :: forall r e a
   . Queue a
  -> Ref.Ref Int
  -> Ref.Ref (Maybe (Cause e))
  -> Stream r e a
  -> RIO r () Unit
produce queue remainingRef failureRef s = do
  outcome <- attemptCause (drainInto s)
  case outcome of
    Right _ -> do
      decremented <- liftEffect (Ref.modify (_ - 1) remainingRef)
      when (decremented <= 0) (Queue.shutdown queue)
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      Queue.shutdown queue
  where
  drainInto :: Stream r e a -> RIO r e Unit
  drainInto inner = do
    step <- unStream inner
    case step of
      Done -> pure unit
      Yield a rest -> do
        _ <- Queue.offer queue a
        drainInto rest

-- | The consumer side of `mergeAll`: blocks on the shared queue
-- | until a value is available or the queue is shut down. On
-- | shutdown, propagates the captured cause (if any) so the
-- | downstream stream sees the failure on its next pull.
consumer
  :: forall r e a
   . Queue a
  -> Ref.Ref (Maybe (Cause e))
  -> Stream r e a
consumer queue failureRef = Stream do
  ma <- Queue.take queue
  case ma of
    Just a -> pure (Yield a (consumer queue failureRef))
    Nothing -> do
      mCause <- liftEffect (Ref.read failureRef)
      case mCause of
        Nothing -> pure Done
        Just cause -> propagateCause cause

-- | Re-raise a captured `Cause` on the current `RIO` channel.
-- |
-- | The producer only ever stores leaf causes (`Fail` / `Die`)
-- | because `attemptCause` returns leaves; the `Parallel` and
-- | `Sequential` branches are defensive only and re-raise as a
-- | defect with a clear message so a future regression is loud.
propagateCause :: forall r e a. Cause e -> RIO r e a
propagateCause = case _ of
  Fail v -> mkRIO \_ -> rioFail v
  Die err -> mkRIO \_ -> throwError err
  Parallel _ _ -> mkRIO \_ ->
    throwError
      ( Aff.error
          "RIO.Stream.Par: unexpected Parallel cause from a single producer"
      )
  Sequential _ _ -> mkRIO \_ ->
    throwError
      ( Aff.error
          "RIO.Stream.Par: unexpected Sequential cause from a single producer"
      )
