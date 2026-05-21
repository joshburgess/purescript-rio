-- | Parallel combinators for `RIO.Aff.Stream`.
-- |
-- | The base `RIO.Aff.Stream` is intentionally single-channel and
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
-- | `mapPar` is the bounded-concurrency `map`: each upstream
-- | element is handed to one of `n` worker fibers that all run
-- | the same effectful function, but the output stream still
-- | yields elements in upstream order. Round-robin dispatch plus
-- | round-robin collection is what preserves the order.
-- |
-- | `zipPar` pulls one element from each of two upstreams in
-- | parallel and pairs them; the output ends as soon as either
-- | upstream ends. `zipLatest` and `zipLatestWith` are the
-- | "latest value from each side" variants: each new element from
-- | either side emits a pair with the most recent value from the
-- | other side, after both sides have produced at least once.
-- |
-- | All combinators here share the same failure model: the *first*
-- | typed failure or defect observed in any producer shuts the
-- | shared queue down. Sibling producers are still running at that
-- | point - they'll find the queue closed on their next `offer`
-- | and exit naturally - but their would-be failures are dropped.
-- | If you need every concurrent failure preserved as a tree,
-- | drain each branch separately through
-- | `RIO.Aff.Cause.parTraverseCause` instead.
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
module RIO.Aff.Stream.Par
  ( broadcast
  , mapPar
  , mapRIOPar
  , merge
  , mergeAll
  , mergeMap
  , partition
  , partitionEither
  , partitioned
  , zipLatest
  , zipLatestWith
  , zipPar
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (error) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Aff.Cause (Cause(..), attemptCause)
import RIO.Aff.Concurrency (fork)
import RIO.Aff.Concurrency (zipPar) as Concurrency
import RIO.Aff.Internal (RIO(..), mkRIO, rioFail)
import RIO.Aff.Queue (Queue)
import RIO.Aff.Queue as Queue
import RIO.Aff.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import RIO.Aff.Stream (Step(..), Stream(..), unStream)

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

-- | Split a stream of `Either l x` into two streams: `lefts`
-- | carries every `Left l`, `rights` carries every `Right x`. The
-- | source is consumed once by a forked pump that routes each
-- | element into the appropriate output queue and propagates
-- | end-of-stream to both sides on completion.
-- |
-- | Each output queue is bounded (capacity `defaultBuffer`). A slow
-- | consumer on one side backpressures the pump, which in turn
-- | pauses the other; consume both streams in parallel to keep
-- | things flowing.
-- |
-- | Failure model matches `mergeAll`: the first observed
-- | producer-side typed failure or defect shuts both queues down,
-- | and either consumer surfaces the same captured cause on its
-- | next pull.
partitionEither
  :: forall r e l x
   . Stream r e (Either l x)
  -> RIO r e { lefts :: Stream r e l, rights :: Stream r e x }
partitionEither upstream = do
  leftQ <- liftEffect (Queue.bounded defaultBuffer)
  rightQ <- liftEffect (Queue.bounded defaultBuffer)
  failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
  _ <- fork (partitionEitherProducer leftQ rightQ failureRef upstream)
  pure
    { lefts: consumer leftQ failureRef
    , rights: consumer rightQ failureRef
    }

-- | Split a stream into two by a boolean predicate: `yes` carries
-- | every element for which `p a` is `true`, `no` carries the rest.
-- | The source is consumed once by a forked pump that routes each
-- | element into the appropriate output queue and propagates
-- | end-of-stream to both sides on completion.
-- |
-- | Each output queue is bounded (capacity `defaultBuffer`). A slow
-- | consumer on one side backpressures the pump, which in turn
-- | pauses the other; consume both streams in parallel to keep
-- | things flowing.
-- |
-- | Failure model matches `mergeAll`: the first observed producer-
-- | side typed failure or defect shuts both queues down, and either
-- | consumer surfaces the same captured cause on its next pull.
partitioned
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> RIO r e { yes :: Stream r e a, no :: Stream r e a }
partitioned p upstream = do
  yesQ <- liftEffect (Queue.bounded defaultBuffer)
  noQ <- liftEffect (Queue.bounded defaultBuffer)
  failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
  _ <- fork (partitionedProducer p yesQ noQ failureRef upstream)
  pure
    { yes: consumer yesQ failureRef
    , no: consumer noQ failureRef
    }

-- | Bounded-concurrency `map`: apply `f` to each upstream element
-- | with at most `n` invocations running concurrently. Output order
-- | matches upstream order.
-- |
-- | Each upstream element is dispatched round-robin to one of `n`
-- | worker fibers; the collector reads results back in the same
-- | round-robin sequence so the ordering is preserved even when
-- | workers finish in a different order. `n` is clamped to at
-- | least 1.
-- |
-- | Failure model: the first typed failure or defect from any
-- | worker (or from the upstream pull itself) shuts the pipeline
-- | down and is propagated downstream on the next pull. Siblings
-- | that are already running finish into a closed queue; their
-- | results are dropped.
mapPar
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
mapPar n f upstream = Stream do
  let k = max 1 n
  reqs <- liftEffect
    (traverse (\_ -> Queue.bounded 1) (Array.range 1 k))
  resps <- liftEffect
    (traverse (\_ -> Queue.bounded 1) (Array.range 1 k))
  failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
  _ <- fork (mapParDispatch reqs upstream failureRef)
  for_ (Array.zip reqs resps) \(Tuple req resp) ->
    fork (mapParWorker f req resp failureRef)
  unStream (mapParCollect resps 0 k failureRef)

-- | Parallel `mapM` that does NOT preserve input order. Each worker
-- | shoves its completed result onto a shared bounded output queue
-- | as soon as it's ready, so a fast element can overtake a slow one
-- | ahead of it. Use this when the downstream consumer is order-
-- | insensitive (folding into a `Set`, sending to a sink keyed by the
-- | element).
-- |
-- | `concurrency` is clamped to at least 1; passing 1 collapses to
-- | sequential mapping (no reordering happens because there is no
-- | parallelism).
-- |
-- | Failure model matches `mapPar`: the first typed failure or defect
-- | from any worker (or from the upstream pull) shuts the pipeline
-- | down and is propagated downstream on the next pull. Siblings
-- | already in flight finish into a closed queue and their results
-- | are dropped.
mapRIOPar
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
mapRIOPar concurrency f upstream = Stream do
  let k = max 1 concurrency
  inQ <- liftEffect (Queue.bounded k)
  outQ <- liftEffect (Queue.bounded defaultBuffer)
  remainingRef <- liftEffect (Ref.new k)
  failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
  traverse_ (\_ -> fork (mapRIOParWorker f inQ outQ remainingRef failureRef))
    (Array.range 1 k)
  _ <- fork (mapRIOParDispatch inQ upstream failureRef)
  unStream (consumer outQ failureRef)

-- | The dispatcher side of `mapRIOPar`: drains upstream under
-- | `attemptCause` and offers each value onto the shared input queue.
-- | On completion or failure, shuts the input queue down so every
-- | worker observes end-of-input and exits.
mapRIOParDispatch
  :: forall r e a
   . Queue a
  -> Stream r e a
  -> Ref.Ref (Maybe (Cause e))
  -> RIO r () Unit
mapRIOParDispatch inQ upstream failureRef = do
  outcome <- attemptCause (drain upstream)
  case outcome of
    Right _ -> Queue.shutdown inQ
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      Queue.shutdown inQ
  where
  drain :: Stream r e a -> RIO r e Unit
  drain s = do
    step <- unStream s
    case step of
      Done -> pure unit
      Yield a rest -> do
        _ <- Queue.offer inQ a
        drain rest

-- | One `mapRIOPar` worker: pulls from the shared input queue, runs
-- | `f` under `attemptCause`, and offers each result onto the shared
-- | output queue. On `f` failure, records the cause and shuts the
-- | output queue down so the consumer wakes up; on input-queue
-- | shutdown, decrements the remaining-workers counter and (when
-- | last) shuts the output queue down for end-of-stream.
mapRIOParWorker
  :: forall r e a b
   . (a -> RIO r e b)
  -> Queue a
  -> Queue b
  -> Ref.Ref Int
  -> Ref.Ref (Maybe (Cause e))
  -> RIO r () Unit
mapRIOParWorker f inQ outQ remainingRef failureRef = go
  where
  go = do
    ma <- Queue.take inQ
    case ma of
      Nothing -> do
        decremented <- liftEffect (Ref.modify (_ - 1) remainingRef)
        when (decremented <= 0) (Queue.shutdown outQ)
      Just a -> do
        outcome <- attemptCause (f a)
        case outcome of
          Right b -> do
            _ <- Queue.offer outQ b
            go
          Left cause -> do
            liftEffect
              ( Ref.modify_
                  ( case _ of
                      Nothing -> Just cause
                      existing -> existing
                  )
                  failureRef
              )
            Queue.shutdown outQ

-- | Pull one element from each of two upstreams in parallel and
-- | pair them with `Tuple`. The output stream ends as soon as
-- | either upstream ends; any element already pulled from the
-- | longer side is discarded.
-- |
-- | The two pulls run under `Concurrency.zipPar`, so a typed
-- | failure on either side cancels the sibling and surfaces on the
-- | parent's row.
zipPar
  :: forall r e a b
   . Stream r e a
  -> Stream r e b
  -> Stream r e (Tuple a b)
zipPar sa sb = Stream do
  Tuple stepA stepB <- Concurrency.zipPar (unStream sa) (unStream sb)
  case stepA, stepB of
    Yield a restA, Yield b restB ->
      pure (Yield (Tuple a b) (zipPar restA restB))
    _, _ -> pure Done

-- | Pair every new element from either upstream with the most
-- | recent value from the other side. The output starts producing
-- | once both sides have yielded at least once and continues until
-- | both sides have ended.
-- |
-- | Both upstreams are drained on their own fibers; the latest
-- | value from each side lives in a `TVar`, and each new yield
-- | atomically updates its own `TVar` and reads the other's. When
-- | both are populated the combined value lands on a shared
-- | bounded output queue.
-- |
-- | Failure model matches `mergeAll`: the first observed typed
-- | failure or defect on either side shuts the output queue down
-- | and is propagated on the next pull.
zipLatest
  :: forall r e a b
   . Stream r e a
  -> Stream r e b
  -> Stream r e (Tuple a b)
zipLatest = zipLatestWith Tuple

-- | `zipLatest` with a user-supplied combiner.
zipLatestWith
  :: forall r e a b c
   . (a -> b -> c)
  -> Stream r e a
  -> Stream r e b
  -> Stream r e c
zipLatestWith f sa sb = Stream do
  output <- liftEffect (Queue.bounded defaultBuffer)
  latestA <- atomically (newTVar (Nothing :: Maybe a))
  latestB <- atomically (newTVar (Nothing :: Maybe b))
  remaining <- liftEffect (Ref.new 2)
  failureRef <- liftEffect (Ref.new (Nothing :: Maybe (Cause e)))
  _ <- fork
    (zipLatestSide sa latestA latestB f output remaining failureRef)
  _ <- fork
    ( zipLatestSide sb latestB latestA (flip f) output remaining
        failureRef
    )
  unStream (consumer output failureRef)

-- | The producer side of `partitionEither`: drains upstream under
-- | `attemptCause`, routes each element to one of the two queues,
-- | and shuts both queues down on completion or failure.
partitionEitherProducer
  :: forall r e l x
   . Queue l
  -> Queue x
  -> Ref.Ref (Maybe (Cause e))
  -> Stream r e (Either l x)
  -> RIO r () Unit
partitionEitherProducer leftQ rightQ failureRef upstream = do
  outcome <- attemptCause (drainTo upstream)
  case outcome of
    Right _ -> do
      Queue.shutdown leftQ
      Queue.shutdown rightQ
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      Queue.shutdown leftQ
      Queue.shutdown rightQ
  where
  drainTo :: Stream r e (Either l x) -> RIO r e Unit
  drainTo s = do
    step <- unStream s
    case step of
      Done -> pure unit
      Yield e rest -> do
        case e of
          Left l -> void (Queue.offer leftQ l)
          Right x -> void (Queue.offer rightQ x)
        drainTo rest

-- | The producer side of `partitioned`: drains upstream under
-- | `attemptCause`, routes each element to one of the two queues by
-- | the predicate, and shuts both queues down on completion or
-- | failure.
partitionedProducer
  :: forall r e a
   . (a -> Boolean)
  -> Queue a
  -> Queue a
  -> Ref.Ref (Maybe (Cause e))
  -> Stream r e a
  -> RIO r () Unit
partitionedProducer p yesQ noQ failureRef upstream = do
  outcome <- attemptCause (drainTo upstream)
  case outcome of
    Right _ -> do
      Queue.shutdown yesQ
      Queue.shutdown noQ
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      Queue.shutdown yesQ
      Queue.shutdown noQ
  where
  drainTo :: Stream r e a -> RIO r e Unit
  drainTo s = do
    step <- unStream s
    case step of
      Done -> pure unit
      Yield a rest -> do
        if p a then void (Queue.offer yesQ a)
        else void (Queue.offer noQ a)
        drainTo rest

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
          "RIO.Aff.Stream.Par: unexpected Parallel cause from a single producer"
      )
  Sequential _ _ -> mkRIO \_ ->
    throwError
      ( Aff.error
          "RIO.Aff.Stream.Par: unexpected Sequential cause from a single producer"
      )

-- | The dispatcher side of `mapPar`: pulls each upstream value and
-- | offers it to the worker queue at the current round-robin index.
-- | On upstream completion or failure, shuts every worker queue
-- | down so workers exit cleanly.
mapParDispatch
  :: forall r e a
   . Array (Queue a)
  -> Stream r e a
  -> Ref.Ref (Maybe (Cause e))
  -> RIO r () Unit
mapParDispatch reqs upstream failureRef = do
  outcome <- attemptCause (drain upstream 0)
  case outcome of
    Right _ -> traverse_ Queue.shutdown reqs
    Left cause -> do
      liftEffect
        ( Ref.modify_
            ( case _ of
                Nothing -> Just cause
                existing -> existing
            )
            failureRef
        )
      traverse_ Queue.shutdown reqs
  where
  k = Array.length reqs
  drain s i = do
    step <- unStream s
    case step of
      Done -> pure unit
      Yield a rest -> do
        case Array.index reqs (i `mod` k) of
          Just q -> void (Queue.offer q a)
          Nothing -> pure unit
        drain rest (i + 1)

-- | One `mapPar` worker fiber: pulls from its request queue, runs
-- | `f` under `attemptCause`, and offers the result on its response
-- | queue. On dispatcher shutdown (`Nothing` from the request
-- | queue), or on failure of `f`, shuts the response queue down so
-- | the collector sees end-of-slot.
mapParWorker
  :: forall r e a b
   . (a -> RIO r e b)
  -> Queue a
  -> Queue b
  -> Ref.Ref (Maybe (Cause e))
  -> RIO r () Unit
mapParWorker f req resp failureRef = go
  where
  go = do
    ma <- Queue.take req
    case ma of
      Nothing -> Queue.shutdown resp
      Just a -> do
        outcome <- attemptCause (f a)
        case outcome of
          Right b -> do
            _ <- Queue.offer resp b
            go
          Left cause -> do
            liftEffect
              ( Ref.modify_
                  ( case _ of
                      Nothing -> Just cause
                      existing -> existing
                  )
                  failureRef
              )
            Queue.shutdown resp

-- | The collector side of `mapPar`: reads worker response queues
-- | round-robin in the same order the dispatcher used, so the
-- | output preserves upstream order. A `Nothing` from the current
-- | slot ends the stream; if a captured cause is present, it is
-- | re-raised on the downstream channel.
mapParCollect
  :: forall r e b
   . Array (Queue b)
  -> Int
  -> Int
  -> Ref.Ref (Maybe (Cause e))
  -> Stream r e b
mapParCollect resps i k failureRef = Stream do
  case Array.index resps (i `mod` k) of
    Nothing -> pure Done
    Just q -> do
      mb <- Queue.take q
      case mb of
        Just b ->
          pure
            ( Yield b (mapParCollect resps (i + 1) k failureRef)
            )
        Nothing -> do
          mCause <- liftEffect (Ref.read failureRef)
          case mCause of
            Nothing -> pure Done
            Just cause -> propagateCause cause

-- | One producer fiber for `zipLatestWith`. Drains its own
-- | upstream, writes each value into `self` and atomically reads
-- | `other` to see whether a pair can be emitted. The `combine`
-- | argument is the application's combiner for the "this side"
-- | element first; the right-hand fiber passes `flip f` so both
-- | fibers can share the same body.
-- |
-- | When the upstream ends naturally, decrements the `remaining`
-- | counter and shuts the output queue down once both sides have
-- | ended. On failure, records the cause and shuts the output
-- | queue down immediately so the consumer surfaces it.
zipLatestSide
  :: forall r e a b c
   . Stream r e a
  -> TVar (Maybe a)
  -> TVar (Maybe b)
  -> (a -> b -> c)
  -> Queue c
  -> Ref.Ref Int
  -> Ref.Ref (Maybe (Cause e))
  -> RIO r () Unit
zipLatestSide source self other combine output remaining failureRef =
  do
    outcome <- attemptCause (drain source)
    case outcome of
      Right _ -> do
        r <- liftEffect (Ref.modify (_ - 1) remaining)
        when (r <= 0) (Queue.shutdown output)
      Left cause -> do
        liftEffect
          ( Ref.modify_
              ( case _ of
                  Nothing -> Just cause
                  existing -> existing
              )
              failureRef
          )
        Queue.shutdown output
  where
  drain stream = do
    step <- unStream stream
    case step of
      Done -> pure unit
      Yield a rest -> do
        mb <- atomically do
          writeTVar self (Just a)
          readTVar other
        case mb of
          Just b -> void (Queue.offer output (combine a b))
          Nothing -> pure unit
        drain rest
