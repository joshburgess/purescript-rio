-- | Pull-based asynchronous streams.
-- |
-- | A `Stream r e a` is built around a single primitive: pull the
-- | next step. Each pull runs in `RIO r e` and either yields the
-- | head plus the continuation, or signals end-of-stream. This keeps
-- | the implementation small while supporting both bounded and
-- | unbounded producers, backpressure (each pull is explicit), and
-- | failure (the pull RIO can raise typed errors or defects).
-- |
-- | The surface covers construction (`empty`, `emit`, `fromArray`,
-- | `repeatRIO`, `fromQueue`, `fromTQueue`), the usual transforms
-- | (`map`, `filter`, `take`, `mapRIO`, `mapPar`, `throttle`,
-- | `timeoutPerPull`), concurrency operators (`merge`, `zipPar`,
-- | `broadcast`, `share`), and terminations (`run`, `runCollect`,
-- | `fold`, `forEach`).
module RIO.Fiber.Stream
  ( Stream(..)
  , Step(..)
  , Emit(..)
  , async
  , empty
  , emit
  , cons
  , append
  , fromArray
  , fromRIO
  , repeatRIO
  , fromQueue
  , fromTQueue
  , fromHub
  , tick
  , range
  , iterate
  , iterateRIO
  , iterateM
  , unfold
  , unfoldRIO
  , paginate
  , paginateRIO
  , forever
  , haltWhen
  , interruptWhen
  , map
  , mapM
  , mapRIO
  , filter
  , filterM
  , collectSome
  , take
  , takeWhile
  , takeUntil
  , drop
  , dropWhile
  , dropUntil
  , tap
  , tapError
  , fold
  , runFoldM
  , forEach
  , head
  , last
  , find
  , run
  , runCollect
  , runSink
  , via
  , buffer
  , bufferDropping
  , bufferSliding
  , concat
  , concatAll
  , merge
  , mergeAll
  , sliding
  , mapPar
  , mapRIOPar
  , scan
  , scanM
  , scanRIO
  , mapAccum
  , intersperse
  , flatMap
  , chunked
  , unchunked
  , mapChunks
  , groupBy
  , zip
  , zipWith
  , zipWithIndex
  , zipPar
  , zipLatest
  , zipLatestWith
  , interleave
  , partitionEither
  , toQueue
  , toHub
  , throttle
  , debounce
  , catchAll
  , retry
  , broadcast
  , share
  , partitioned
  , distinct
  , distinctBy
  , timeoutPerPull
  , acquireReleaseStream
  , peel
  , transduce
  , aggregate
  , aggregateWithin
  , groupedWithin
  , changes
  ) where

import Prelude hiding (map)

import Data.Array (index, snoc, uncons)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Ref as Ref
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Deferred (Deferred)
import RIO.Fiber.Deferred as Deferred
import RIO.Fiber.Hub (Hub)
import RIO.Fiber.Hub as Hub
import RIO.Fiber.Mailbox (Mailbox)
import RIO.Fiber.Mailbox as Mailbox
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q
import RIO.Fiber.Schedule (Schedule)
import RIO.Fiber.Schedule as Sch
import RIO.Fiber.Scope (Scope)
import RIO.Fiber.Scope as Scope
import RIO.Fiber.Pipe (Pipe(..))
import RIO.Fiber.Sink (Sink(..), SinkLoop)
import RIO.Fiber.Sink as Sink
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TDeferred (TDeferred)
import RIO.Fiber.STM.TDeferred as TDeferred
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

-- | A push-side event delivered to the emit callback handed out by
-- | `async`. Each call delivers exactly one of:
-- |
-- |   * `EmitValue a`     - emit a single element
-- |   * `EmitEnd`         - end the stream cleanly (subsequent pulls return `Done`)
-- |   * `EmitFailure v`   - terminate the stream with a typed failure
-- |   * `EmitDefect e`    - terminate the stream with a defect
-- |
-- | Once any terminal event is delivered (`EmitEnd`, `EmitFailure`,
-- | `EmitDefect`), later emits are ignored: the stream has already
-- | committed to a single outcome.
data Emit e a
  = EmitValue a
  | EmitEnd
  | EmitFailure (Variant e)
  | EmitDefect Error

-- | Build a stream from an external push-style producer.
-- |
-- | `register` runs once (in `Effect`) at the first pull and receives
-- | an emit callback that the producer uses to push `Emit` events. It
-- | returns a best-effort cleanup `Effect` that is registered as a
-- | finalizer on the supplied scope; the cleanup fires when the scope
-- | closes, regardless of how the stream terminated (`EmitEnd`,
-- | failure, defect, or downstream halt).
-- |
-- | The internal buffer is unbounded: emits never block and never drop
-- | elements. If you need to bound memory under a slow consumer, layer
-- | a bounded `Queue` between the producer and the stream or apply
-- | `throttle` / `debounce` downstream.
-- |
-- | Typical use: bridge a callback-style source like a WebSocket or
-- | DOM event handler into a `Stream`:
-- |
-- | ```purescript
-- | Scope.scoped \scope -> do
-- |   let
-- |     events :: Stream r e Message
-- |     events = S.async scope \emit -> do
-- |       socket <- openSocket "..."
-- |       onMessage socket (\msg -> emit (EmitValue msg))
-- |       onClose socket (\_ -> emit EmitEnd)
-- |       pure (closeSocket socket)
-- |   S.runCollect (S.take 100 events)
-- | ```
async
  :: forall r e a
   . Scope
  -> ((Emit e a -> Effect Unit) -> Effect (Effect Unit))
  -> Stream r e a
async scope register = Stream do
  state <- F.liftEffect (Ref.new (initialAsyncState :: AsyncState e a))
  cleanup <- F.liftEffect (register (asyncEmit state))
  Scope.addFinalizer scope cleanup
  case asyncLoop state of
    Stream pull -> pull

type AsyncState e a =
  { buffer :: Array (Emit e a)
  , waiter :: Maybe (Emit e a -> Effect Unit)
  , halted :: Boolean
  , terminal :: Maybe (Emit e a)
  }

initialAsyncState :: forall e a. AsyncState e a
initialAsyncState =
  { buffer: []
  , waiter: Nothing
  , halted: false
  , terminal: Nothing
  }

asyncEmit
  :: forall e a
   . Ref.Ref (AsyncState e a)
  -> Emit e a
  -> Effect Unit
asyncEmit ref event = do
  st <- Ref.read ref
  if st.halted then pure unit
  else case st.waiter of
    Just k -> do
      Ref.write
        (st { waiter = Nothing, halted = isTerminal event })
        ref
      k event
    Nothing -> case event of
      EmitValue _ ->
        Ref.write (st { buffer = snoc st.buffer event }) ref
      _ ->
        -- Terminal event with no current waiter: stash it as `terminal`
        -- so the next pull drains the buffer first, then surfaces the
        -- terminal cause without admitting later emits.
        Ref.write
          (st { halted = true, terminal = Just event }) ref

isTerminal :: forall e a. Emit e a -> Boolean
isTerminal = case _ of
  EmitValue _ -> false
  _ -> true

asyncLoop :: forall r e a. Ref.Ref (AsyncState e a) -> Stream r e a
asyncLoop ref = Stream do
  event <- F.async \cb -> do
    st <- Ref.read ref
    case uncons st.buffer of
      Just { head, tail } -> do
        Ref.write (st { buffer = tail }) ref
        cb (Right head)
        pure (pure unit)
      Nothing -> case st.terminal of
        Just term -> do
          -- Consume the latched terminal; subsequent pulls return Done.
          Ref.write (st { terminal = Nothing }) ref
          cb (Right term)
          pure (pure unit)
        Nothing
          | st.halted -> do
              cb (Right EmitEnd)
              pure (pure unit)
          | otherwise -> do
              Ref.write (st { waiter = Just (\e -> cb (Right e)) }) ref
              pure (Ref.modify_ (_ { waiter = Nothing }) ref)
  case event of
    EmitValue a -> pure (Yield a (asyncLoop ref))
    EmitEnd -> pure Done
    EmitFailure v -> F.fail v
    EmitDefect e -> F.die e

-- | The empty stream. The first pull immediately signals `Done`.
empty :: forall r e a. Stream r e a
empty = Stream (pure Done)

-- | A stream that yields exactly one element, then halts.
emit :: forall r e a. a -> Stream r e a
emit a = Stream (pure (Yield a empty))

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
-- | with `concat s (emit a)`.
append :: forall r e a. Stream r e a -> a -> Stream r e a
append (Stream pull) a = Stream do
  step <- pull
  case step of
    Done -> pure (Yield a empty)
    Yield x rest -> pure (Yield x (append rest a))

-- | A stream that yields each element of the array in order, then halts.
fromArray :: forall r e a. Array a -> Stream r e a
fromArray xs = Stream
  ( pure case uncons xs of
      Nothing -> Done
      Just { head, tail } -> Yield head (fromArray tail)
  )

-- | A stream that runs `action` once on the first pull, yields the
-- | result, then halts. The lifted action's failure / defect /
-- | interrupt surfaces as the stream's pull outcome.
fromRIO :: forall r e a. RIO r e a -> Stream r e a
fromRIO action = Stream do
  a <- action
  pure (Yield a empty)

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

-- | A stream that subscribes to `hub` for the lifetime of `scope`
-- | and pulls every message published from that point on. The
-- | subscription is unregistered automatically when the scope
-- | closes, even if the stream is dropped without exhausting it
-- | (e.g. via an upstream `take`).
-- |
-- | Cold subscription: messages published before the first pull
-- | are missed.
fromHub :: forall r e a. Scope -> Hub a -> Stream r e a
fromHub scope hub = acquireReleaseStream scope
  (Hub.subscribe hub)
  Hub.unsubscribe
  drain
  where
  drain :: Hub.Subscription a -> Stream r e a
  drain sub = Stream do
    a <- Hub.take sub
    pure (Yield a (drain sub))

-- | A stream that yields `unit` once every `dur`, sleeping through
-- | the gap. Infinite: bound with `take` or interrupt the consumer.
-- | The first element is emitted after the first sleep, not at time
-- | zero, matching the effect-ts convention.
tick :: forall r e. Milliseconds -> Stream r e Unit
tick dur = Stream do
  Clock.sleep dur
  pure (Yield unit (tick dur))

-- | A stream of ascending integers from `start` (inclusive) to `end`
-- | (exclusive). When `end <= start` the stream is empty.
range :: forall r e. Int -> Int -> Stream r e Int
range start end
  | start >= end = empty
  | otherwise = Stream do
      s <- pure start
      pure (Yield s (range (s + 1) end))

-- | Iterate a pure function starting from `seed`, yielding every
-- | intermediate value (including `seed`). Infinite: bound with
-- | `take` or `haltWhen`.
iterate :: forall r e a. a -> (a -> a) -> Stream r e a
iterate seed step = Stream do
  s <- pure seed
  pure (Yield s (iterate (step s) step))

-- | Effectful sibling of `iterate`: yield `seed`, then `f seed`,
-- | then `f (f seed)`, ... where `f` runs in `RIO`. The step runs
-- | once per element pulled. Infinite: bound externally.
iterateRIO :: forall r e a. a -> (a -> RIO r e a) -> Stream r e a
iterateRIO seed f = Stream do
  next <- f seed
  pure (Yield seed (iterateRIO next f))

-- | Alias for `iterateRIO`, matching rio-aff's `iterateM` name.
iterateM :: forall r e a. a -> (a -> RIO r e a) -> Stream r e a
iterateM = iterateRIO

-- | Build a stream by unfolding a seed with a pure step. The
-- | generator returns `Nothing` to halt, or `Just (Tuple a s')` to
-- | yield `a` and continue from `s'`.
unfold
  :: forall r e s a
   . s
  -> (s -> Maybe (Tuple a s))
  -> Stream r e a
unfold seed step = Stream do
  pure (go (step seed))
  where
  go = case _ of
    Nothing -> Done
    Just (Tuple a s') -> Yield a (unfold s' step)

-- | Build a stream by unfolding a seed with an effectful step. The
-- | generator runs in `RIO`, so it may observe services or fail. Use
-- | `unfold` when the step is pure.
unfoldRIO
  :: forall r e s a
   . s
  -> (s -> RIO r e (Maybe (Tuple a s)))
  -> Stream r e a
unfoldRIO seed step = Stream do
  m <- step seed
  case m of
    Nothing -> pure Done
    Just (Tuple a s') -> pure (Yield a (unfoldRIO s' step))

-- | Page through a resource. Starting from `seed`, each call to the
-- | step function returns a value to emit plus the next seed (or
-- | `Nothing` to stop). Equivalent to `unfold` for a step that always
-- | emits, then optionally terminates.
-- |
-- | Common shape for paginated APIs: the step performs the request
-- | for the current page, emits the page (or its contents flattened
-- | downstream), and returns the cursor for the next page or
-- | `Nothing` when there are no more.
paginate
  :: forall r e s a
   . s
  -> (s -> Tuple a (Maybe s))
  -> Stream r e a
paginate seed step = Stream do
  let Tuple a next = step seed
  pure
    ( Yield a case next of
        Nothing -> empty
        Just s' -> paginate s' step
    )

-- | Effectful variant of `paginate`. The step runs in `RIO` so it can
-- | perform requests, observe services, or fail.
paginateRIO
  :: forall r e s a
   . s
  -> (s -> RIO r e (Tuple a (Maybe s)))
  -> Stream r e a
paginateRIO seed step = Stream do
  Tuple a next <- step seed
  pure
    ( Yield a case next of
        Nothing -> empty
        Just s' -> paginateRIO s' step
    )

-- | Halt the upstream as soon as `signal` completes (with any
-- | outcome). The signal is started once in a forked fiber; each
-- | subsequent pull races the upstream against the shared completion
-- | flag. The first upstream element after subscription is forwarded;
-- | the moment `signal` resolves the stream emits `Done` without
-- | pulling further.
-- |
-- | Typical use: `haltWhen (Deferred.await stopSignal) stream` lets
-- | external code request graceful shutdown of an infinite stream.
haltWhen :: forall r e a x. RIO r e x -> Stream r e a -> Stream r e a
haltWhen signal source = Stream do
  done <- F.liftEffect (TDeferred.make :: _ (TDeferred Unit))
  _ <- F.fork do
    _ <- signal
    _ <- STM.atomically (TDeferred.complete done unit)
    pure unit
  case go done source of
    Stream pull -> pull
  where
  go done (Stream pull) = Stream do
    already <- STM.atomically (TDeferred.poll done)
    case already of
      Just _ -> pure Done
      Nothing -> do
        res <- F.race
          (Right <$> pull)
          (Left <$> STM.atomically (TDeferred.await done))
        case res of
          Left _ -> pure Done
          Right Done -> pure Done
          Right (Yield a rest) -> pure (Yield a (go done rest))

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
interruptWhen
  :: forall r e a
   . Deferred e Unit
  -> Stream r e a
  -> Stream r e a
interruptWhen sentinel s = Stream do
  poll <- Deferred.poll sentinel
  case poll of
    Just (Left v) -> F.fail v
    Just (Right _) -> pure Done
    Nothing -> F.race
      (Deferred.await sentinel *> pure Done)
      ( case s of
          Stream pull -> do
            step <- pull
            case step of
              Done -> pure Done
              Yield a rest ->
                pure (Yield a (interruptWhen sentinel rest))
      )

-- | Transform every element with `f`. Lazy: the function runs as
-- | elements are pulled.
map :: forall r e a b. (a -> b) -> Stream r e a -> Stream r e b
map f (Stream pull) = Stream do
  s <- pull
  pure case s of
    Done -> Done
    Yield a rest -> Yield (f a) (map f rest)

-- | Transform every element with an effectful action. Sequential:
-- | each element is awaited before the next is pulled. For
-- | concurrent mapping, use `mapPar` (order-preserving) or
-- | `mapRIOPar` (unordered).
mapRIO :: forall r e a b. (a -> RIO r e b) -> Stream r e a -> Stream r e b
mapRIO f (Stream pull) = Stream do
  s <- pull
  case s of
    Done -> pure Done
    Yield a rest -> do
      b <- f a
      pure (Yield b (mapRIO f rest))

-- | Alias for `mapRIO` matching rio-aff's `mapM` naming. Provided
-- | so code ported from rio-aff snippets reads the same; reach for
-- | `mapRIO` in new code.
mapM :: forall r e a b. (a -> RIO r e b) -> Stream r e a -> Stream r e b
mapM = mapRIO

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

-- | Filter with an effectful predicate. The predicate runs in the
-- | same `RIO r e` as the stream's pull effect, so it can read
-- | services, hit refs, or itself fail with a typed error (which
-- | aborts the stream).
filterM
  :: forall r e a
   . (a -> RIO r e Boolean)
  -> Stream r e a
  -> Stream r e a
filterM p (Stream pull) = Stream (loop pull)
  where
  loop next = do
    s <- next
    case s of
      Done -> pure Done
      Yield a (Stream rest) -> do
        keep <- p a
        if keep then pure (Yield a (filterM p (Stream rest)))
        else loop rest

-- | Filter + map in one pass: keep the elements where the function
-- | returns `Just`, replacing them with the inner value. Elements
-- | producing `Nothing` are dropped silently.
collectSome
  :: forall r e a b
   . (a -> Maybe b)
  -> Stream r e a
  -> Stream r e b
collectSome f (Stream pull) = Stream (loop pull)
  where
  loop next = do
    s <- next
    case s of
      Done -> pure Done
      Yield a (Stream rest) -> case f a of
        Just b -> pure (Yield b (collectSome f (Stream rest)))
        Nothing -> loop rest

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

-- | Yield elements while `p` holds, then stop. The first element
-- | failing the predicate is NOT included. If the stream ends
-- | before any element fails the predicate, every element is
-- | forwarded.
takeWhile :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
takeWhile p = go
  where
  go (Stream pull) = Stream do
    step <- pull
    case step of
      Done -> pure Done
      Yield a rest
        | p a -> pure (Yield a (go rest))
        | otherwise -> pure Done

-- | Yield elements until `p` matches an element, then stop. The
-- | matching element IS included in the output. If the stream ends
-- | before any element matches, every element is forwarded.
takeUntil :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
takeUntil p = go
  where
  go (Stream pull) = Stream do
    step <- pull
    case step of
      Done -> pure Done
      Yield a rest
        | p a -> pure (Yield a empty)
        | otherwise -> pure (Yield a (go rest))

-- | Drop the first `n` elements, then yield the rest unchanged.
-- | Non-positive `n` is a no-op.
drop :: forall r e a. Int -> Stream r e a -> Stream r e a
drop n s
  | n <= 0 = s
  | otherwise = case s of
      Stream pull -> Stream do
        step <- pull
        case step of
          Done -> pure Done
          Yield _ rest -> case drop (n - 1) rest of
            Stream pull' -> pull'

-- | Drop elements until `p` matches an element, then yield the rest
-- | INCLUDING that matching element. If no element ever matches, the
-- | resulting stream is empty.
dropUntil :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
dropUntil p = go
  where
  go (Stream pull) = Stream do
    step <- pull
    case step of
      Done -> pure Done
      Yield a rest
        | p a -> pure (Yield a rest)
        | otherwise -> case go rest of
            Stream pull' -> pull'

-- | Drop elements while `p` holds, then yield the rest unchanged
-- | (including the first element for which `p` returns `false`).
dropWhile :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
dropWhile p = go
  where
  go (Stream pull) = Stream do
    step <- pull
    case step of
      Done -> pure Done
      Yield a rest
        | p a -> case go rest of
            Stream pull' -> pull'
        | otherwise -> pure (Yield a rest)

-- | Run a side-effect for every element, threading the value
-- | unchanged downstream. If the tap fails (typed or defect), the
-- | failure propagates to the consumer.
tap :: forall r e a. (a -> RIO r e Unit) -> Stream r e a -> Stream r e a
tap f = mapRIO \a -> do
  f a
  pure a

-- | Run a side-effect on every typed failure raised while pulling
-- | the stream, then re-raise it. The stream's error row is
-- | unchanged.
tapError
  :: forall r e a
   . (Variant e -> RIO r e Unit)
  -> Stream r e a
  -> Stream r e a
tapError onErr = go
  where
  go (Stream pull) = Stream
    ( F.catchAll (\v -> onErr v *> F.fail v)
        do
          step <- pull
          pure case step of
            Done -> Done
            Yield a rest -> Yield a (go rest)
    )

-- | Reduce the stream with `step`, starting from `seed`. Pulls
-- | until the stream signals `Done`.
fold :: forall r e a b. (b -> a -> b) -> b -> Stream r e a -> RIO r e b
fold step seed (Stream pull) = do
  s <- pull
  case s of
    Done -> pure seed
    Yield a rest -> fold step (step seed a) rest

-- | Effectful left fold: drain the stream with an `RIO` step
-- | function. Matches rio-aff's `runFoldM`.
runFoldM
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> RIO r e b
runFoldM seed step (Stream pull) = do
  s <- pull
  case s of
    Done -> pure seed
    Yield a rest -> do
      seed' <- step seed a
      runFoldM seed' step rest

-- | Pull the stream's first element, then stop. Returns `Nothing`
-- | for an empty stream. Short-circuits: the tail is never evaluated.
head :: forall r e a. Stream r e a -> RIO r e (Maybe a)
head (Stream pull) = do
  s <- pull
  case s of
    Done -> pure Nothing
    Yield a _ -> pure (Just a)

-- | Drain the stream and return the last element, or `Nothing` if
-- | the stream is empty. Runs the whole stream; do not call on an
-- | infinite stream.
last :: forall r e a. Stream r e a -> RIO r e (Maybe a)
last = go Nothing
  where
  go acc (Stream pull) = do
    s <- pull
    case s of
      Done -> pure acc
      Yield a rest -> go (Just a) rest

-- | Pull elements until one matches the predicate, return that
-- | element, and discard the rest. Returns `Nothing` if the stream
-- | ends before a match is found. Short-circuits on first match.
find
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> RIO r e (Maybe a)
find p (Stream pull) = do
  s <- pull
  case s of
    Done -> pure Nothing
    Yield a rest ->
      if p a then pure (Just a)
      else find p rest

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

-- | Drive a `Sink` with this stream. The sink may terminate early
-- | (via `Just o` from `step`), in which case the stream is not
-- | pulled further. If the stream ends first, the sink's `done`
-- | synthesises the final result from its accumulated state.
runSink :: forall r e i o. Stream r e i -> Sink r e i o -> RIO r e o
runSink stream0 (Sink mkLoop) = do
  loop <- mkLoop
  let
    go (Stream pull) = do
      s <- pull
      case s of
        Done -> loop.done
        Yield i rest -> do
          mOut <- loop.step i
          case mOut of
            Just o -> pure o
            Nothing -> go rest
  go stream0

-- | Splice a `Pipe` transducer between this stream and its consumer:
-- | each upstream element is fed to the pipe, and the pipe's emissions
-- | become the new stream. The pipe's `onDone` runs once upstream
-- | signals end-of-stream, producing any trailing emissions (e.g. the
-- | final partial chunk).
via :: forall r e i o. Stream r e i -> Pipe r e i o -> Stream r e o
via upstream0 (Pipe mkLoop) = Stream do
  loop <- mkLoop
  bufRef <- F.liftEffect (Ref.new ([] :: Array o))
  upRef <- F.liftEffect (Ref.new (Just upstream0))
  finishedRef <- F.liftEffect (Ref.new false)
  let
    drainOne :: RIO r e (Step r e o)
    drainOne = do
      buf <- F.liftEffect (Ref.read bufRef)
      case Array.uncons buf of
        Just { head, tail } -> do
          F.liftEffect (Ref.write tail bufRef)
          pure (Yield head (Stream drainOne))
        Nothing -> do
          done <- F.liftEffect (Ref.read finishedRef)
          if done then pure Done
          else do
            mUp <- F.liftEffect (Ref.read upRef)
            case mUp of
              Nothing -> pure Done
              Just (Stream pull) -> do
                s <- pull
                case s of
                  Done -> do
                    tailEmit <- loop.onDone
                    F.liftEffect (Ref.write tailEmit bufRef)
                    F.liftEffect (Ref.write Nothing upRef)
                    F.liftEffect (Ref.write true finishedRef)
                    drainOne
                  Yield i rest -> do
                    F.liftEffect (Ref.write (Just rest) upRef)
                    step <- loop.onInput i
                    F.liftEffect (Ref.write step.emit bufRef)
                    if not step.more then do
                      tailEmit <- loop.onDone
                      F.liftEffect
                        (Ref.modify_ (\xs -> xs <> tailEmit) bufRef)
                      F.liftEffect (Ref.write Nothing upRef)
                      F.liftEffect (Ref.write true finishedRef)
                      pure unit
                    else
                      pure unit
                    drainOne
  drainOne

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

-- | Like `buffer`, but the buffer drops *new* elements when full
-- | rather than back-pressuring the producer. Use when the consumer
-- | is the bottleneck and dropping is preferable to stalling the
-- | producer (telemetry pipelines, UI event streams).
bufferDropping :: forall r e a. Int -> Stream r e a -> Stream r e a
bufferDropping n source = Stream do
  q <- F.liftEffect (Q.dropping (max 1 n) :: _ (Q.Queue (Maybe a)))
  _ <- F.fork do
    forEach (\a -> void (Q.tryOffer q (Just a))) source
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

-- | Like `buffer`, but the buffer evicts the *oldest* element to
-- | make room for a new one when full. Use when the freshest values
-- | matter most and an old backlog should be discarded under
-- | pressure (live dashboards, sensor readouts).
bufferSliding :: forall r e a. Int -> Stream r e a -> Stream r e a
bufferSliding n source = Stream do
  q <- F.liftEffect (Q.sliding (max 1 n) :: _ (Q.Queue (Maybe a)))
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

-- | Sequential concatenation: emit every element of `sl`, then
-- | every element of `sr`. The right-hand stream is not touched
-- | until the left exhausts. Contrast with `merge`, which interleaves.
concat :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
concat (Stream pull) sr = Stream do
  step <- pull
  case step of
    Done -> case sr of Stream p -> p
    Yield a rest -> pure (Yield a (concat rest sr))

-- | Concatenate an array of streams sequentially: yield every
-- | element of the first stream, then the second, and so on. An
-- | empty input array yields the empty stream.
concatAll :: forall r e a. Array (Stream r e a) -> Stream r e a
concatAll sources = case uncons sources of
  Nothing -> empty
  Just { head, tail } -> concat head (concatAll tail)

-- | Repeat a stream forever. After the inner stream ends the same
-- | stream is drained again.
-- |
-- | Idempotent on an already-infinite stream. On an empty stream
-- | this produces an empty stream (the recursion never yields a
-- | value), not a busy loop.
forever :: forall r e a. Stream r e a -> Stream r e a
forever s = Stream
  ( case s of
      Stream pull -> do
        step <- pull
        case step of
          Done -> pure Done
          Yield a rest -> pure (Yield a (concat rest (forever s)))
  )

-- | Deterministically interleave two streams: left, right, left,
-- | right, alternating. When one side ends, the rest of the other
-- | side is forwarded unchanged. Pulls are sequential, so this
-- | composes well with backpressure-sensitive sources.
interleave :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
interleave sa sb = Stream do
  case sa of
    Stream pullA -> do
      step <- pullA
      case step of
        Done -> case sb of Stream pullB -> pullB
        Yield a restA -> pure (Yield a (interleave sb restA))

-- | Non-deterministically interleave two streams. Both producers
-- | run concurrently into a shared buffer; the consumer sees an
-- | arbitrary interleaving. The result terminates when both
-- | upstreams have ended.
merge :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
merge sl sr = Stream do
  mb <- F.liftEffect (Mailbox.make 16 2 :: _ (Mailbox a))
  let
    pump source = do
      forEach (Mailbox.offer mb) source
      Mailbox.done mb
  _ <- F.fork (pump sl)
  _ <- F.fork (pump sr)
  case drainMailbox mb of
    Stream pull -> pull
  where
  drainMailbox :: Mailbox a -> Stream r e a
  drainMailbox mb = Stream do
    m <- Mailbox.take mb
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (drainMailbox mb))

-- | Non-deterministically interleave an arbitrary number of
-- | streams. All sources run concurrently into a shared buffer; the
-- | consumer sees an arbitrary interleaving. The result terminates
-- | when every source has ended.
-- |
-- | An empty `sources` array produces the empty stream.
mergeAll :: forall r e a. Array (Stream r e a) -> Stream r e a
mergeAll sources = Stream do
  let n = Array.length sources
  if n == 0 then pure Done
  else do
    mb <- F.liftEffect (Mailbox.make 16 n :: _ (Mailbox a))
    let
      pump source = do
        forEach (Mailbox.offer mb) source
        Mailbox.done mb
    traverse_ (\s -> F.fork (pump s)) sources
    case drainMailbox mb of
      Stream pull -> pull
  where
  drainMailbox :: Mailbox a -> Stream r e a
  drainMailbox mb = Stream do
    m <- Mailbox.take mb
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (drainMailbox mb))

-- | Emit overlapping (or strided) windows of the upstream as
-- | arrays. `chunkSize` is the size of each emitted window;
-- | `step` is the distance the window advances between emissions
-- | (so `step = 1` yields fully-overlapping windows, `step =
-- | chunkSize` yields disjoint chunks).
-- |
-- | Both are clamped to at least 1. When upstream ends with a
-- | partially-filled buffer the residual window is emitted once
-- | (it may be shorter than `chunkSize`).
sliding
  :: forall r e a
   . { chunkSize :: Int, step :: Int }
  -> Stream r e a
  -> Stream r e (Array a)
sliding { chunkSize, step } source =
  let
    cs = max 1 chunkSize
    st = max 1 step
  in
    loop cs st [] source
  where
  loop cs st buf (Stream pull) = Stream do
    if Array.length buf >= cs then
      pure (Yield buf (loop cs st (Array.drop st buf) (Stream pull)))
    else do
      r <- pull
      case r of
        Done ->
          if Array.null buf then pure Done
          else pure (Yield buf empty)
        Yield a rest ->
          case loop cs st (snoc buf a) rest of
            Stream pull' -> pull'

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
  reqs <- F.liftEffect (traverse (\_ -> Q.make 1) (Array.range 0 (n - 1)) :: _ (Array (Q.Queue (Maybe a))))
  resps <- F.liftEffect (traverse (\_ -> Q.make 1) (Array.range 0 (n - 1)) :: _ (Array (Q.Queue (Maybe b))))
  let pairs = Array.zipWith Tuple reqs resps
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

-- | Parallel `mapRIO` that does NOT preserve input order. Each
-- | worker shoves its completed result onto a shared output queue
-- | as soon as it's ready, so a fast element can overtake a slow one
-- | ahead of it. Use this when the downstream consumer is order-
-- | insensitive (folding into a `Set`, sending to a sink keyed by
-- | the element, etc.).
-- |
-- | A `concurrency` of 0 or 1 collapses to sequential mapping (no
-- | reordering happens because there's no parallelism).
mapRIOPar
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
mapRIOPar concurrency f source = Stream do
  let n = max 1 concurrency
  inQ <- F.liftEffect (Q.make n :: _ (Q.Queue (Maybe a)))
  outMb <- F.liftEffect (Mailbox.make n n :: _ (Mailbox b))
  let
    worker = do
      m <- Q.take inQ
      case m of
        Nothing -> Mailbox.done outMb
        Just a -> do
          b <- f a
          Mailbox.offer outMb b
          worker
  _ <- F.forkAll (Array.replicate n worker)
  _ <- F.fork do
    forEach (\a -> Q.offer inQ (Just a)) source
    _ <- F.forEach (\_ -> Q.offer inQ Nothing) (Array.replicate n unit)
    pure unit
  case drainUnordered outMb of
    Stream pull -> pull
  where
  drainUnordered mb = Stream do
    m <- Mailbox.take mb
    case m of
      Nothing -> pure Done
      Just b -> pure (Yield b (drainUnordered mb))

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

-- | Effectful running fold: emit `seed`, then for every element `a`
-- | emit `step prev a` where the step runs in `RIO`.
scanRIO
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
scanRIO seed step source = Stream (pure (Yield seed (go seed source)))
  where
  go acc (Stream pull) = Stream do
    s <- pull
    case s of
      Done -> pure Done
      Yield a rest -> do
        next <- step acc a
        pure (Yield next (go next rest))

-- | Alias for `scanRIO`, matching rio-aff's `scanM` name.
scanM
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
scanM = scanRIO

-- | Stateful map: thread `s` through the stream and emit one `b` per
-- | input. The result has the same length as the input.
mapAccum
  :: forall r e s a b
   . (s -> a -> Tuple s b)
  -> s
  -> Stream r e a
  -> Stream r e b
mapAccum step seed source = Stream (go seed source)
  where
  go acc (Stream pull) = do
    s <- pull
    case s of
      Done -> pure Done
      Yield a rest ->
        let Tuple acc' b = step acc a
        in pure (Yield b (Stream (go acc' rest)))

-- | Insert `sep` between every pair of emitted elements. The first
-- | element passes through unchanged; thereafter every original
-- | element is preceded by `sep`.
intersperse :: forall r e a. a -> Stream r e a -> Stream r e a
intersperse sep (Stream pull) = Stream do
  s <- pull
  case s of
    Done -> pure Done
    Yield a rest -> pure (Yield a (go rest))
  where
  go (Stream nextPull) = Stream do
    s <- nextPull
    case s of
      Done -> pure Done
      Yield a rest -> pure (Yield sep (Stream (pure (Yield a (go rest)))))

-- | Run `f` on every element and concatenate the resulting sub-streams.
-- | Each sub-stream is fully drained before the next input is pulled.
flatMap
  :: forall r e a b
   . (a -> Stream r e b)
  -> Stream r e a
  -> Stream r e b
flatMap f (Stream pull) = Stream do
  s <- pull
  case s of
    Done -> pure Done
    Yield a rest -> case appendStream (f a) (flatMap f rest) of
      Stream nextPull -> nextPull
  where
  appendStream :: Stream r e b -> Stream r e b -> Stream r e b
  appendStream (Stream p1) after = Stream do
    s <- p1
    case s of
      Done -> case after of Stream p2 -> p2
      Yield b rest -> pure (Yield b (appendStream rest after))

-- | Group consecutive elements into fixed-size chunks. The final
-- | chunk is whatever remains when the upstream ends and may be
-- | shorter than `size`. `size <= 0` collapses to one chunk per
-- | element so downstream gets the same shape.
chunked :: forall r e a. Int -> Stream r e a -> Stream r e (Array a)
chunked n source = Stream (go [] source)
  where
  cap = max 1 n
  go acc (Stream pull) = do
    s <- pull
    case s of
      Done ->
        if Array.null acc then pure Done
        else pure (Yield acc empty)
      Yield a rest ->
        let acc' = snoc acc a
        in
          if Array.length acc' >= cap then
            pure (Yield acc' (Stream (go [] rest)))
          else go acc' rest

-- | Flatten a chunked stream back into individual elements.
unchunked :: forall r e a. Stream r e (Array a) -> Stream r e a
unchunked = flatMap fromArray

-- | Apply a pure function to each chunk. Useful for batched
-- | transforms that work better on arrays than element-at-a-time
-- | (e.g. SIMD-friendly arithmetic, batched encoding).
mapChunks
  :: forall r e a b
   . (Array a -> Array b)
  -> Stream r e (Array a)
  -> Stream r e (Array b)
mapChunks = map

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

-- | Pair elements from two streams positionally, pulling each side
-- | sequentially: pull the left, then pull the right, emit `(a, b)`.
-- | The output ends as soon as either side ends. Unlike `zipPar`,
-- | the pulls do not race, so a slow left blocks a fast right and
-- | vice versa.
zip :: forall r e a b. Stream r e a -> Stream r e b -> Stream r e (Tuple a b)
zip = zipWith Tuple

-- | Like `zip`, but combine paired elements with `f` rather than
-- | tupling them.
zipWith
  :: forall r e a b c
   . (a -> b -> c)
  -> Stream r e a
  -> Stream r e b
  -> Stream r e c
zipWith f (Stream pullA) (Stream pullB) = Stream do
  sa <- pullA
  case sa of
    Done -> pure Done
    Yield a restA -> do
      sb <- pullB
      case sb of
        Done -> pure Done
        Yield b restB -> pure (Yield (f a b) (zipWith f restA restB))

-- | Pair each element with its 0-based position in the stream.
zipWithIndex
  :: forall r e a
   . Stream r e a
  -> Stream r e (Tuple Int a)
zipWithIndex = go 0
  where
  go i (Stream pull) = Stream do
    step <- pull
    case step of
      Done -> pure Done
      Yield a rest -> pure (Yield (Tuple i a) (go (i + 1) rest))

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

-- | Emit a new tuple whenever either source emits, pairing the new
-- | value with the most recently seen value from the other side.
-- |
-- | No tuple is emitted until both sides have emitted at least once.
-- | When only one side has ever emitted and the other ends without
-- | emitting, the output ends without yielding. Otherwise the output
-- | ends when both sides have ended.
-- |
-- | This is useful for combining independently-updating signals
-- | (e.g. "always show the latest of A and B side by side") and is
-- | the analogue of effect-ts's `Stream.zipLatest`.
zipLatest
  :: forall r e a b
   . Stream r e a
  -> Stream r e b
  -> Stream r e (Tuple a b)
zipLatest = zipLatestWith Tuple

-- | Like `zipLatest`, but combine the latest values with `f` instead
-- | of pairing them.
zipLatestWith
  :: forall r e a b c
   . (a -> b -> c)
  -> Stream r e a
  -> Stream r e b
  -> Stream r e c
zipLatestWith f sa sb = Stream do
  latestA <- F.liftEffect (STM.newTVar (Nothing :: Maybe a))
  latestB <- F.liftEffect (STM.newTVar (Nothing :: Maybe b))
  genVar <- F.liftEffect (STM.newTVar 0)
  doneA <- F.liftEffect (STM.newTVar false)
  doneB <- F.liftEffect (STM.newTVar false)
  let
    pumpA = do
      forEach
        ( \a -> STM.atomically do
            STM.writeTVar latestA (Just a)
            STM.modifyTVar genVar (_ + 1)
        )
        sa
      STM.atomically (STM.writeTVar doneA true)
    pumpB = do
      forEach
        ( \b -> STM.atomically do
            STM.writeTVar latestB (Just b)
            STM.modifyTVar genVar (_ + 1)
        )
        sb
      STM.atomically (STM.writeTVar doneB true)
  _ <- F.fork pumpA
  _ <- F.fork pumpB
  case loop latestA latestB genVar doneA doneB 0 of
    Stream pull -> pull
  where
  loop la lb gv da db lastSeen = Stream do
    res <- STM.atomically do
      g <- STM.readTVar gv
      aDone <- STM.readTVar da
      bDone <- STM.readTVar db
      if g /= lastSeen then do
        ma <- STM.readTVar la
        mb <- STM.readTVar lb
        case ma, mb of
          Just a, Just b -> pure (Just { gen: g, value: f a b })
          Nothing, _ ->
            if aDone then pure Nothing
            else STM.retry
          _, Nothing ->
            if bDone then pure Nothing
            else STM.retry
      else if aDone && bDone then pure Nothing
      else STM.retry
    case res of
      Nothing -> pure Done
      Just s -> pure (Yield s.value (loop la lb gv da db s.gen))

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
  subs <- traverse (\_ -> Hub.subscribe hub) (Array.range 1 (max 1 n))
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

-- | Split the source stream into two sub-streams by a predicate.
-- | Elements matching `p` flow to `yes`; the rest flow to `no`. The
-- | source is consumed once by a forked pump that routes each element
-- | into the appropriate output queue and propagates end-of-stream to
-- | both sides on completion.
-- |
-- | Each output queue is bounded (capacity 16). A slow consumer on
-- | one side backpressures the pump, which in turn pauses the other
-- | side; consume both streams in parallel to keep things flowing.
partitioned
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> RIO r e { yes :: Stream r e a, no :: Stream r e a }
partitioned p source = do
  yesQ <- F.liftEffect (Q.make 16 :: _ (Q.Queue (Maybe a)))
  noQ <- F.liftEffect (Q.make 16 :: _ (Q.Queue (Maybe a)))
  _ <- F.fork do
    forEach
      ( \a ->
          if p a then Q.offer yesQ (Just a)
          else Q.offer noQ (Just a)
      )
      source
    Q.offer yesQ Nothing
    Q.offer noQ Nothing
  pure { yes: drainQ yesQ, no: drainQ noQ }
  where
  drainQ :: Queue (Maybe a) -> Stream r e a
  drainQ q = Stream do
    m <- Q.take q
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (drainQ q))

-- | Split a stream of `Either l r` into two streams: `lefts` carries
-- | every `Left l`, `rights` carries every `Right r`. The source is
-- | consumed once by a forked pump that routes each element into the
-- | appropriate output queue and propagates end-of-stream to both
-- | sides on completion.
-- |
-- | Each output queue is bounded (capacity 16). A slow consumer on
-- | one side backpressures the pump, which in turn pauses the other;
-- | consume both streams in parallel to keep things flowing.
partitionEither
  :: forall r e l x
   . Stream r e (Either l x)
  -> RIO r e { lefts :: Stream r e l, rights :: Stream r e x }
partitionEither source = do
  leftQ <- F.liftEffect (Q.make 16 :: _ (Q.Queue (Maybe l)))
  rightQ <- F.liftEffect (Q.make 16 :: _ (Q.Queue (Maybe x)))
  _ <- F.fork do
    forEach
      ( case _ of
          Left l -> Q.offer leftQ (Just l)
          Right x -> Q.offer rightQ (Just x)
      )
      source
    Q.offer leftQ Nothing
    Q.offer rightQ Nothing
  pure { lefts: drainQ leftQ, rights: drainQ rightQ }
  where
  drainQ :: forall a. Queue (Maybe a) -> Stream r e a
  drainQ q = Stream do
    m <- Q.take q
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (drainQ q))

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

-- | Acquire a resource into a `Scope` on the first pull, then pull
-- | from the use-stream built from that resource. The release is
-- | registered with the supplied scope and runs at scope close,
-- | regardless of how the consumer terminates: normal `Done`,
-- | mid-stream typed failure, defect, interrupt, or an early stop
-- | by an upstream `take` (the scope simply closes when the
-- | enclosing `Scope.scoped` body exits).
-- |
-- | Usage pattern:
-- |
-- |     Scope.scoped \scope -> do
-- |       let s = S.acquireReleaseStream scope openFile closeFile linesOf
-- |       S.runCollect (S.take 100 s)
acquireReleaseStream
  :: forall r e a b
   . Scope
  -> RIO r e a
  -> (a -> RIO r e Unit)
  -> (a -> Stream r e b)
  -> Stream r e b
acquireReleaseStream scope acquire release use = Stream do
  resource <- Scope.acquireRelease scope acquire release
  case use resource of
    Stream pull -> pull

-- | Run a `Sink` over a prefix of the stream and hand back the
-- | sink's result paired with the remaining stream. The sink decides
-- | the prefix length: it stops as soon as its `step` returns
-- | `Just o`, leaving the unread tail of the upstream as the
-- | continuation. If the stream ends before the sink terminates, the
-- | sink's `done` synthesises the result and the returned stream is
-- | empty.
-- |
-- | The complement to `runSink`: where `runSink` drains the stream
-- | for one final answer, `peel` extracts a prefix and lets the
-- | caller keep going with the rest. Compose with `transduce` for
-- | "apply this sink over and over, emit each result".
peel
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> RIO r e (Tuple b (Stream r e a))
peel (Sink mkLoop) stream0 = do
  loop <- mkLoop
  go stream0 loop
  where
  go (Stream pull) loop = do
    s <- pull
    case s of
      Done -> do
        b <- loop.done
        pure (Tuple b empty)
      Yield a rest -> do
        mOut <- loop.step a
        case mOut of
          Just b -> pure (Tuple b rest)
          Nothing -> go rest loop

-- | Apply a `Sink` as a stateful transducer: run the sink over the
-- | stream and emit each sink result, then start a fresh sink from
-- | the next element. When the upstream signals end-of-stream while
-- | a sink is mid-run, the sink's `done` synthesises a final flush
-- | emission. When upstream ends before any element has been seen,
-- | no flush is emitted.
-- |
-- | Pairs well with `takeN`-style sinks ("emit every 100 elements
-- | as a batch") and `foldUntil` ("emit when a predicate triggers").
transduce
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> Stream r e b
transduce (Sink mkLoop) source = Stream do
  loopRef <- F.liftEffect (Ref.new (Nothing :: Maybe (SinkLoop r e a b)))
  upRef <- F.liftEffect (Ref.new (Just source))
  let
    getLoop = do
      mLoop <- F.liftEffect (Ref.read loopRef)
      case mLoop of
        Just l -> pure l
        Nothing -> do
          l <- mkLoop
          F.liftEffect (Ref.write (Just l) loopRef)
          pure l

    pull = do
      mUp <- F.liftEffect (Ref.read upRef)
      case mUp of
        Nothing -> pure Done
        Just (Stream up) -> do
          s <- up
          case s of
            Done -> do
              F.liftEffect (Ref.write Nothing upRef)
              mLoop <- F.liftEffect (Ref.read loopRef)
              case mLoop of
                Just l -> do
                  b <- l.done
                  F.liftEffect (Ref.write Nothing loopRef)
                  pure (Yield b (Stream (pure Done)))
                Nothing -> pure Done
            Yield a rest -> do
              F.liftEffect (Ref.write (Just rest) upRef)
              loop <- getLoop
              mOut <- loop.step a
              case mOut of
                Just b -> do
                  F.liftEffect (Ref.write Nothing loopRef)
                  pure (Yield b (Stream pull))
                Nothing -> pull
  pull

-- | Sink-based aggregation: synonym for `transduce` provided for
-- | effect-ts familiarity. Run the sink across the stream, emitting
-- | a `b` each time the sink completes and starting a fresh sink
-- | from the next element. When upstream ends mid-aggregation, the
-- | sink's `done` synthesises a final flush.
aggregate
  :: forall r e a b
   . Sink r e a b
  -> Stream r e a
  -> Stream r e b
aggregate = transduce

-- | Aggregate elements with a `Sink`, but emit at least every
-- | `duration` even when the sink has not yet completed. Useful for
-- | "batch up to N elements or every T milliseconds, whichever comes
-- | first" patterns: pair `aggregateWithin` with `Sink.takeN n` and
-- | downstream sees batches of at most `n`, with no element waiting
-- | longer than `duration`.
-- |
-- | A forced flush calls the sink's `done` to synthesise an `o` from
-- | whatever has accumulated, then starts a fresh sink for the next
-- | window. Empty windows (no elements arrived before timeout) do
-- | not produce an emission; the window simply restarts.
-- |
-- | The producer pulls upstream into a small buffer concurrently;
-- | the consumer races each buffered take against a timer derived
-- | from `Clock.currentEpoch`, so virtual-clock tests can drive the
-- | flush deterministically.
aggregateWithin
  :: forall r e a b
   . Milliseconds
  -> Sink r e a b
  -> Stream r e a
  -> Stream r e b
aggregateWithin (Milliseconds dur) (Sink mkLoop) source = Stream do
  q <- F.liftEffect (Q.make 16 :: _ (Q.Queue (Maybe a)))
  _ <- F.fork do
    forEach (\a -> Q.offer q (Just a)) source
    Q.offer q Nothing
  case freshWindow q of
    Stream pull -> pull
  where
  freshWindow :: Queue (Maybe a) -> Stream r e b
  freshWindow q = Stream do
    loop <- mkLoop
    Milliseconds now <- Clock.currentEpoch
    runWindow q loop now false

  runWindow
    :: Queue (Maybe a)
    -> SinkLoop r e a b
    -> Number
    -> Boolean
    -> RIO r e (Step r e b)
  runWindow q loop windowStart hasData = do
    Milliseconds now <- Clock.currentEpoch
    let remaining = max 0.0 (dur - (now - windowStart))
    m <- F.race
      (Just <$> Q.take q)
      (F.sleep (Milliseconds remaining) *> pure Nothing)
    case m of
      Nothing ->
        if hasData then do
          b <- loop.done
          pure (Yield b (freshWindow q))
        else
          runWindow q loop now false
      Just Nothing ->
        if hasData then do
          b <- loop.done
          pure (Yield b empty)
        else pure Done
      Just (Just a) -> do
        mOut <- loop.step a
        case mOut of
          Just b -> pure (Yield b (freshWindow q))
          Nothing -> runWindow q loop windowStart true

-- | Group elements into chunks of up to `maxSize`. A chunk flushes
-- | as soon as it fills, *or* when `duration` elapses since the
-- | chunk window opened (whichever comes first). A trailing partial
-- | chunk is flushed on upstream termination.
-- |
-- | Sugar over `aggregateWithin duration (Sink.takeN maxSize)`.
groupedWithin
  :: forall r e a
   . Int
  -> Milliseconds
  -> Stream r e a
  -> Stream r e (Array a)
groupedWithin maxSize duration source
  | maxSize <= 0 = empty
  | otherwise =
      aggregateWithin duration (Sink.takeN maxSize) source

-- | Filter out consecutive duplicate elements. The first element of
-- | each run is kept; subsequent equal neighbours are dropped. Useful
-- | for collapsing repeated states (sensor readings, UI events).
changes :: forall r e a. Eq a => Stream r e a -> Stream r e a
changes source = Stream (start source)
  where
  start (Stream pull) = do
    s <- pull
    case s of
      Done -> pure Done
      Yield a rest -> pure (Yield a (go a rest))

  go prev (Stream pull) = Stream (loop prev pull)

  loop prev pull = do
    s <- pull
    case s of
      Done -> pure Done
      Yield a (Stream rest)
        | a == prev -> loop prev rest
        | otherwise -> pure (Yield a (go a (Stream rest)))

-- | Alias for `changes` matching rio-aff's `distinct` naming. Drop
-- | consecutive duplicate elements, keeping the first of each run.
-- | Provided for symmetry with rio-aff, where consumers may already
-- | reach for that name.
distinct :: forall r e a. Eq a => Stream r e a -> Stream r e a
distinct = changes

-- | Filter out consecutive elements whose extracted key is equal to
-- | the previous one. A keyed variant of `changes`: useful when the
-- | element itself lacks `Eq`, or when "duplicate" means "same id"
-- | rather than "same payload".
distinctBy
  :: forall r e a k
   . Eq k
  => (a -> k)
  -> Stream r e a
  -> Stream r e a
distinctBy key source = Stream (start source)
  where
  start (Stream pull) = do
    s <- pull
    case s of
      Done -> pure Done
      Yield a rest -> pure (Yield a (go (key a) rest))

  go prevKey (Stream pull) = Stream (loop prevKey pull)

  loop prevKey pull = do
    s <- pull
    case s of
      Done -> pure Done
      Yield a (Stream rest)
        | key a == prevKey -> loop prevKey rest
        | otherwise -> pure (Yield a (go (key a) (Stream rest)))

-- | Drain every element of the source stream into the given queue.
-- | Returns when the stream ends; the caller decides what (if
-- | anything) to write to mark end-of-stream. Useful for bridging a
-- | stream into a backpressure-aware consumer of `Queue`.
toQueue :: forall r e a. Queue a -> Stream r e a -> RIO r e Unit
toQueue q = forEach (Q.offer q)

-- | Drain every element of the source stream into the given hub.
-- | Returns when the stream ends. Each element is broadcast to every
-- | current subscriber.
toHub :: forall r e a. Hub a -> Stream r e a -> RIO r e Unit
toHub h = forEach (Hub.publish h)
