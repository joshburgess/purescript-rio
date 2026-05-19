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
  , fromTQueue
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
  , throttle
  , debounce
  , catchAll
  , retry
  , broadcast
  , share
  , timeoutPerPull
  ) where

import Prelude hiding (map)

import Data.Array (index, range, snoc, uncons, zipWith)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect.Ref as Ref
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Hub (Hub)
import RIO.Fiber.Hub as Hub
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q
import RIO.Fiber.Schedule (Schedule)
import RIO.Fiber.Schedule as Sch
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TQueue (TQueue)
import RIO.Fiber.STM.TQueue as TQ
import Unsafe.Coerce (unsafeCoerce)

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

-- | A stream that pulls from the given transactional queue. Each
-- | pull commits a single `readTQueue`, retrying until an element is
-- | available. Infinite: callers bound it externally.
fromTQueue :: forall r e a. TQueue a -> Stream r e a
fromTQueue q = Stream do
  a <- STM.atomically (TQ.readTQueue q)
  pure (Yield a (fromTQueue q))

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

-- | Rate-limit emissions to at most one per `duration`. Bursts on
-- | the upstream are paced out; an upstream slower than `duration`
-- | passes through unchanged. Reads time from the active `Clock`
-- | service so tests can substitute a virtual clock.
throttle :: forall r e a. Milliseconds -> Stream r e a -> Stream r e a
throttle (Milliseconds duration) source = Stream do
  lastRef <- F.liftEffect (Ref.new (Nothing :: Maybe Milliseconds))
  case loop lastRef source of
    Stream pull -> pull
  where
  loop lastRef (Stream pull) = Stream do
    step <- pull
    case step of
      Done -> pure Done
      Yield a rest -> do
        last <- F.liftEffect (Ref.read lastRef)
        Milliseconds now <- Clock.currentEpoch
        case last of
          Nothing -> do
            F.liftEffect (Ref.write (Just (Milliseconds now)) lastRef)
            pure (Yield a (loop lastRef rest))
          Just (Milliseconds prev) -> do
            let elapsed = now - prev
            if elapsed >= duration then do
              F.liftEffect (Ref.write (Just (Milliseconds now)) lastRef)
              pure (Yield a (loop lastRef rest))
            else do
              F.sleep (Milliseconds (duration - elapsed))
              Milliseconds now2 <- Clock.currentEpoch
              F.liftEffect (Ref.write (Just (Milliseconds now2)) lastRef)
              pure (Yield a (loop lastRef rest))

data DebounceStep a
  = DBEmit a
  | DBRetry { seq :: Int, value :: a }
  | DBDone

-- | Emit an element only after `duration` has elapsed without a new
-- | element arriving. Coalesces bursts into a single trailing-edge
-- | emission. When the source ends with a pending value, that value
-- | is dropped if it has not yet stabilized.
debounce :: forall r e a. Milliseconds -> Stream r e a -> Stream r e a
debounce duration source = Stream do
  seqVar <- F.liftEffect (STM.newTVar 0)
  latest <- F.liftEffect (STM.newTVar (Nothing :: Maybe a))
  doneVar <- F.liftEffect (STM.newTVar false)
  _ <- F.fork do
    forEach
      ( \a -> STM.atomically do
          STM.modifyTVar seqVar (_ + 1)
          STM.writeTVar latest (Just a)
      )
      source
    STM.atomically (STM.writeTVar doneVar true)
  case loop seqVar latest doneVar of
    Stream pull -> pull
  where
  loop seqVar latest doneVar = Stream do
    res <- STM.atomically do
      m <- STM.readTVar latest
      done <- STM.readTVar doneVar
      case m of
        Just a -> do
          s <- STM.readTVar seqVar
          pure (Just { seq: s, value: a })
        Nothing ->
          if done then pure Nothing
          else STM.retry
    case res of
      Nothing -> pure Done
      Just snap -> stabilize seqVar latest doneVar snap

  stabilize seqVar latest doneVar snap = do
    F.sleep duration
    result <- STM.atomically do
      s <- STM.readTVar seqVar
      m <- STM.readTVar latest
      done <- STM.readTVar doneVar
      if s == snap.seq then do
        STM.writeTVar latest Nothing
        pure (DBEmit snap.value)
      else case m of
        Just v' -> pure (DBRetry { seq: s, value: v' })
        Nothing ->
          if done then pure DBDone
          else STM.retry
    case result of
      DBEmit v -> pure (Yield v (loop seqVar latest doneVar))
      DBRetry snap' -> stabilize seqVar latest doneVar snap'
      DBDone -> pure Done

-- | Handle typed failures from the producer's pull. The handler
-- | receives the failure and produces a replacement stream to
-- | continue with. Elements emitted before the failure are preserved;
-- | every emit after the handler fires comes from the handler's
-- | stream.
catchAll
  :: forall r e e' a
   . (Variant e -> Stream r e' a)
  -> Stream r e a
  -> Stream r e' a
catchAll handler (Stream pull) = Stream do
  result <- F.causeOf pull
  case result of
    Right Done -> pure Done
    Right (Yield a rest) -> pure (Yield a (catchAll handler rest))
    Left cause -> case Array.head (Cause.failures cause) of
      Just v -> case handler v of
        Stream pull' -> pull'
      Nothing -> F.failCause (unsafeCoerceCause cause)
  where
  unsafeCoerceCause :: forall x y. Cause x -> Cause y
  unsafeCoerceCause = unsafeCoerce

-- | Retry the source stream from the beginning on typed failure,
-- | consulting `schedule` between attempts. Elements yielded before
-- | the failure are not replayed; if the schedule halts, the last
-- | failure is re-raised.
retry
  :: forall r e a b
   . Schedule (Variant e) b
  -> Stream r e a
  -> Stream r e a
retry schedule source = Stream (pullWith schedule source)
  where
  pullWith sched s@(Stream p) = do
    result <- F.causeOf p
    case result of
      Right Done -> pure Done
      Right (Yield a rest) -> pure (Yield a (Stream (pullWith schedule rest)))
      Left cause -> case Array.head (Cause.failures cause) of
        Nothing -> F.failCause (unsafeCoerceCauseR cause)
        Just v -> do
          d <- F.liftEffect (stepSchedule sched v)
          case d of
            Sch.Halt _ -> F.fail v
            Sch.Step _ delay next -> do
              Clock.sleep delay
              pullWith next s

  stepSchedule (Sch.Schedule step) v = step v

  unsafeCoerceCauseR :: forall x y. Cause x -> Cause y
  unsafeCoerceCauseR = unsafeCoerce

-- | Split a single producer into `n` independent consumer streams.
-- | The source is pumped into a `Hub` from a forked fiber; each
-- | returned stream is a separate subscriber with its own buffer of
-- | size `capacity`. End-of-stream from the source closes every
-- | output stream once the buffered elements drain.
-- |
-- | `publish` on the underlying hub uses default backpressure: if a
-- | consumer falls behind by more than `capacity` items, the
-- | publisher's forked fiber suspends until that consumer catches
-- | up. This protects the slow consumer from losing elements at the
-- | cost of slowing the publisher (and therefore every sibling).
broadcast
  :: forall r e a
   . Int
  -> Int
  -> Stream r e a
  -> RIO r e (Array (Stream r e a))
broadcast n capacity source = do
  hub <- F.liftEffect (Hub.make capacity :: _ (Hub (Maybe a)))
  subs <- traverse (\_ -> Hub.subscribe hub) (range 1 (max 1 n))
  _ <- F.fork do
    forEach (\a -> Hub.publish hub (Just a)) source
    Hub.publish hub Nothing
  pure (subscriptionStream <$> subs)
  where
  subscriptionStream sub = Stream do
    m <- Hub.take sub
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (subscriptionStream sub))

-- | Run the source in the background and hand back an action that
-- | yields a fresh subscriber stream every time it's invoked. Each
-- | new subscriber observes elements published from its
-- | subscription point forward (cold subscribers see only the future,
-- | not the history). Use `share` to fan a hot publisher out to a
-- | varying set of consumers; use `broadcast` when the consumer
-- | count is known up front.
share
  :: forall r e a
   . Int
  -> Stream r e a
  -> RIO r e (RIO r e (Stream r e a))
share capacity source = do
  hub <- F.liftEffect (Hub.make capacity :: _ (Hub (Maybe a)))
  _ <- F.fork do
    forEach (\a -> Hub.publish hub (Just a)) source
    Hub.publish hub Nothing
  pure (subscribe hub)
  where
  subscribe hub = do
    sub <- Hub.subscribe hub
    pure (loop sub)

  loop sub = Stream do
    m <- Hub.take sub
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (loop sub))

-- | Wrap every pull from the source in a per-pull timeout. If the
-- | pull does not produce a step within `duration`, the consumer
-- | sees `Yield Nothing` and the stream continues with the same
-- | source (the slow pull is interrupted). A successful pull yields
-- | `Yield (Just a)`. End-of-stream propagates as a final `Done`.
timeoutPerPull
  :: forall r e a
   . Milliseconds
  -> Stream r e a
  -> Stream r e (Maybe a)
timeoutPerPull duration (Stream pull) = Stream do
  result <- F.timeout duration pull
  case result of
    Nothing -> pure (Yield Nothing (timeoutPerPull duration (Stream pull)))
    Just Done -> pure Done
    Just (Yield a rest) -> pure (Yield (Just a) (timeoutPerPull duration rest))
