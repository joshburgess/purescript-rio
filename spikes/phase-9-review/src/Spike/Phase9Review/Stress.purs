-- | Phase 9 stress scenarios for the recently-added modules.
-- |
-- | Each scenario targets one module and asserts a single
-- | load-bearing invariant after random scheduling and random
-- | termination modes (success, typed failure, fiber kill where
-- | applicable). Modules under test:
-- |
-- |   * `RIO.Logger`: annotation stack is restored after every
-- |     `withFields` block, including blocks that exit by typed
-- |     failure or are torn down while a forked child is active.
-- |   * `RIO.Local`: `locally`'s value-restore is honoured on
-- |     every termination path of the wrapped action.
-- |   * `RIO.STM.TQueue`: producer/consumer correctness under
-- |     contention.
-- |   * `RIO.STM.THub`: all four back-pressure strategies
-- |     (Unbounded fan-out, Bounded back-pressure, Sliding drop-
-- |     oldest, Dropping drop-new + boolean return).
-- |   * `RIO.STM.TSemaphore`: `withTSemaphore` returns permits on
-- |     every termination path including mid-hold fiber kills.
module Spike.Phase9Review.Stress
  ( ScenarioResult
  , loggerScenario
  , localScenario
  , queueScenario
  , hubScenario
  , hubBoundedScenario
  , hubSlidingScenario
  , hubDroppingScenario
  , semaphoreScenario
  ) where

import Prelude hiding (join)

import Data.Array (length, range, snoc) as Array
import Data.Either (Either(..))
import Data.Foldable (all, for_, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Core
  ( RIO
  , catchAll
  , fail
  , fork
  , interrupt
  , join
  , parTraverse
  , provideAll
  , runRIO
  , unsafeRunRIO
  )
import RIO.Local (Local, locally, newLocalEffect)
import RIO.Local as Local
import RIO.Logger (Logger, logInfo, withFields)
import RIO.STM (atomically)
import RIO.STM.THub
  ( THub
  , newBoundedTHub
  , newDroppingTHub
  , newSlidingTHub
  , newUnboundedTHub
  , publishTHub
  , subscribeTHub
  , takeSubscription
  , tryTakeSubscription
  , unsubscribeTHub
  )
import RIO.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import RIO.STM.TSemaphore
  ( TSemaphore
  , availableTSemaphore
  , newTSemaphore
  , withTSemaphore
  )
import RIO.Test.Logger (newRecordingLogger)

-- | Outcome of a single iteration: did the invariant hold, with
-- | an `Int` payload describing the magnitude of the failure on
-- | the unhappy path (e.g. residual annotation count, drift in
-- | the cell's final value, delta between expected and observed
-- | totals). `0` on the happy path.
type ScenarioResult =
  { ok :: Boolean
  , detail :: Int
  }

okResult :: ScenarioResult
okResult = { ok: true, detail: 0 }

failResult :: Int -> ScenarioResult
failResult d = { ok: false, detail: d }

-- | Run a random nested tree of `withFields` blocks. At each
-- | level we attach a fresh `(k, v)` pair, log a message, and
-- | randomly either recurse, fail with a typed error, or fork
-- | the recursion and join it before returning. After the whole
-- | program returns, the logger's annotation set must be empty
-- | (every `withFields` restored its predecessor on the way out).
-- |
-- | Random parameters: `depth` (max recursion depth, 1..8),
-- | `failPct` (probability of a typed failure at each level,
-- | 0..50), `forkPct` (probability of forking instead of
-- | recursing inline, 0..50).
loggerScenario
  :: { depth :: Int, failPct :: Int, forkPct :: Int }
  -> Aff ScenarioResult
loggerScenario opts = do
  rec <- newRecordingLogger
  let
    program :: Int -> RIO (logger :: Logger) (boom :: Int) Unit
    program n =
      if n <= 0 then logInfo "leaf"
      else withFields [ Tuple ("k" <> show n) (show n) ] do
        logInfo ("level " <> show n)
        f <- liftEffect (randomInt 0 99)
        fk <- liftEffect (randomInt 0 99)
        if f < opts.failPct then fail (Proxy :: Proxy "boom") n
        else if fk < opts.forkPct then do
          fib <- fork (program (n - 1))
          _ <- join fib
          pure unit
        else program (n - 1)

  _ <- attempt
    (runRIO (provideAll { logger: rec.logger } (program opts.depth)))
  remaining <- liftEffect rec.logger.getAnnotations
  pure
    ( if Array.length remaining == 0 then okResult
      else failResult (Array.length remaining)
    )

-- | Nest `locally` overrides on a single `Local Int`, randomly
-- | failing or forking at each level. After everything returns,
-- | the cell must hold its initial value (every `locally`
-- | restored its snapshot, including through interrupts).
-- |
-- | Random parameters: `depth` (1..8), `failPct` (0..50),
-- | `forkPct` (0..50), `killPct` (0..50). When a fork happens
-- | and `killPct` fires, the parent kills the child mid-flight
-- | to exercise the `finally`-backed restore on the interrupt
-- | path.
localScenario
  :: { depth :: Int
     , failPct :: Int
     , forkPct :: Int
     , killPct :: Int
     }
  -> Aff ScenarioResult
localScenario opts = do
  initial <- liftEffect (randomInt 1000 1999)
  cell <- liftEffect (newLocalEffect initial)
  let
    nested :: Local Int -> Int -> RIO () (boom :: Int) Unit
    nested fl n =
      if n <= 0 then pure unit
      else locally fl (initial + n) do
        f <- liftEffect (randomInt 0 99)
        fk <- liftEffect (randomInt 0 99)
        kk <- liftEffect (randomInt 0 99)
        if f < opts.failPct then fail (Proxy :: Proxy "boom") n
        else if fk < opts.forkPct then do
          fib <- fork (nested fl (n - 1))
          if kk < opts.killPct then interrupt fib
          else do
            _ <- join fib
            pure unit
        else nested fl (n - 1)

  _ <- attempt (unsafeRunRIO (nested cell opts.depth) {})
  finalE <- unsafeRunRIO (Local.get cell :: RIO () () Int) {}
  let
    final = case finalE of
      Right v -> v
      Left _ -> initial - 1 -- can't happen; row is empty
  pure
    ( if final == initial then okResult
      else failResult (final - initial)
    )

-- | `producers` fibers each enqueue `perProducer` integers into
-- | a shared `TQueue`; `consumers` fibers each dequeue until
-- | their per-fiber quota is filled. The invariant: total
-- | enqueued count and sum equal total dequeued count and sum.
-- |
-- | Random parameters: `producers` (1..4), `consumers` (1..4),
-- | `perProducer` (4..16). Producer quotas are divided across
-- | the consumers as evenly as possible; the last consumer
-- | picks up the remainder so the total budget matches.
queueScenario
  :: { producers :: Int, consumers :: Int, perProducer :: Int }
  -> Aff ScenarioResult
queueScenario opts = do
  let
    total = opts.producers * opts.perProducer
    base = total / opts.consumers
    extra = total - (base * opts.consumers)
    consumerQuotas =
      map (\i -> if i == opts.consumers - 1 then base + extra else base)
        (Array.range 0 (opts.consumers - 1))
    expectedSum =
      sum
        ( map
            ( \p -> sum
                (map (\i -> p * 1000 + i) (Array.range 1 opts.perProducer))
            )
            (Array.range 1 opts.producers)
        )
  sumRef <- liftEffect (Ref.new 0)
  countRef <- liftEffect (Ref.new 0)

  let
    producer :: TQueue Int -> Int -> RIO () () Unit
    producer q p =
      for_ (Array.range 1 opts.perProducer) \i ->
        atomically (writeTQueue q (p * 1000 + i))

    consumer :: TQueue Int -> Int -> RIO () () Unit
    consumer q quota =
      for_ (Array.range 1 quota) \_ -> do
        v <- atomically (readTQueue q)
        liftEffect (Ref.modify_ (_ + v) sumRef)
        liftEffect (Ref.modify_ (_ + 1) countRef)

    program :: RIO () () Unit
    program = do
      q <- atomically newTQueue
      consumerFibers <- traverse (\quota -> fork (consumer q quota))
        consumerQuotas
      _ <- parTraverse (producer q) (Array.range 1 opts.producers)
      _ <- traverse join consumerFibers
      pure unit

  _ <- attempt (unsafeRunRIO program {})
  observedSum <- liftEffect (Ref.read sumRef)
  observedCount <- liftEffect (Ref.read countRef)
  pure
    ( if observedSum == expectedSum && observedCount == total then okResult
      else failResult (observedCount - total)
    )

-- | `subscribers` consumers subscribe to an Unbounded THub, then
-- | a single publisher pushes `publishCount` values. Every
-- | subscriber must dequeue exactly the published count and
-- | their sums must match the publisher's sum.
-- |
-- | Random parameters: `subscribers` (1..5), `publishCount`
-- | (4..20). Subscribers consume in parallel via forked fibers
-- | joined at the end.
hubScenario
  :: { subscribers :: Int, publishCount :: Int }
  -> Aff ScenarioResult
hubScenario opts = do
  resultRef <-
    liftEffect (Ref.new ([] :: Array { count :: Int, total :: Int }))

  let
    expectedSum = sum (Array.range 1 opts.publishCount)

    runOneSubscriber :: THub Int -> RIO () () Unit
    runOneSubscriber hub = do
      sub <- atomically (subscribeTHub hub)
      let
        loop acc =
          if acc.count >= opts.publishCount then pure acc
          else do
            v <- atomically (takeSubscription sub)
            loop { count: acc.count + 1, total: acc.total + v }
      result <- loop { count: 0, total: 0 }
      atomically (unsubscribeTHub sub)
      liftEffect (Ref.modify_ (\xs -> Array.snoc xs result) resultRef)

    program :: RIO () () Unit
    program = do
      hub :: THub Int <- atomically newUnboundedTHub
      consumerFibers <- traverse
        (\_ -> fork (runOneSubscriber hub))
        (Array.range 1 opts.subscribers)
      liftAff (delay (Milliseconds 1.0))
      for_ (Array.range 1 opts.publishCount) \v ->
        atomically (publishTHub hub v)
      _ <- traverse join consumerFibers
      pure unit

  _ <- attempt (unsafeRunRIO program {})
  results <- liftEffect (Ref.read resultRef)
  let
    countOk = Array.length results == opts.subscribers
    bodyOk =
      all
        ( \r ->
            r.count == opts.publishCount && r.total == expectedSum
        )
        results
  pure
    ( if countOk && bodyOk then okResult
      else failResult (opts.subscribers - Array.length results)
    )

-- | Bounded THub back-pressure. A single subscriber with buffer
-- | size `buffer` reads `publishCount` values from a forked
-- | publisher. `publishCount` is chosen well above `buffer` so
-- | the publisher's `publishTHub` must retry every time the
-- | buffer fills and the consumer must drain to unblock it. The
-- | invariant: the consumer receives exactly `publishCount`
-- | values and their sum matches `1 + 2 + ... + publishCount`.
-- |
-- | Random parameters: `buffer` (2..6), `publishCount`
-- | (`buffer + 4`..`buffer * 4`).
hubBoundedScenario
  :: { buffer :: Int, publishCount :: Int }
  -> Aff ScenarioResult
hubBoundedScenario opts = do
  countRef <- liftEffect (Ref.new 0)
  sumRef <- liftEffect (Ref.new 0)
  let
    expectedSum = sum (Array.range 1 opts.publishCount)

    program :: RIO () () Unit
    program = do
      hub :: THub Int <- atomically (newBoundedTHub opts.buffer)
      sub <- atomically (subscribeTHub hub)
      publisher <- fork do
        for_ (Array.range 1 opts.publishCount) \v ->
          atomically (publishTHub hub v)
      let
        drain n =
          if n <= 0 then pure unit
          else do
            v <- atomically (takeSubscription sub)
            liftEffect (Ref.modify_ (_ + 1) countRef)
            liftEffect (Ref.modify_ (_ + v) sumRef)
            drain (n - 1)
      drain opts.publishCount
      _ <- join publisher
      atomically (unsubscribeTHub sub)

  _ <- attempt (unsafeRunRIO program {})
  obsCount <- liftEffect (Ref.read countRef)
  obsSum <- liftEffect (Ref.read sumRef)
  pure
    ( if obsCount == opts.publishCount && obsSum == expectedSum then okResult
      else failResult (obsCount - opts.publishCount)
    )

-- | Sliding THub overflow. A single subscriber with buffer size
-- | `buffer` does not consume while the publisher pushes
-- | `publishCount > buffer` values 1..K. After publishing
-- | completes, the subscriber drains all available values via
-- | `tryTakeSubscription`. The invariant: drained values are
-- | exactly the last `buffer` values published, in publish
-- | order (Sliding drops the oldest entry on overflow).
-- |
-- | Random parameters: `buffer` (2..6), `publishCount`
-- | (`buffer + 2`..`buffer * 3`).
hubSlidingScenario
  :: { buffer :: Int, publishCount :: Int }
  -> Aff ScenarioResult
hubSlidingScenario opts = do
  drainedRef <- liftEffect (Ref.new ([] :: Array Int))
  let
    expected =
      Array.range (opts.publishCount - opts.buffer + 1) opts.publishCount

    program :: RIO () () Unit
    program = do
      hub :: THub Int <- atomically (newSlidingTHub opts.buffer)
      sub <- atomically (subscribeTHub hub)
      for_ (Array.range 1 opts.publishCount) \v ->
        atomically (publishTHub hub v)
      let
        drain = do
          m <- atomically (tryTakeSubscription sub)
          case m of
            Nothing -> pure unit
            Just v -> do
              liftEffect (Ref.modify_ (\xs -> Array.snoc xs v) drainedRef)
              drain
      drain
      atomically (unsubscribeTHub sub)

  _ <- attempt (unsafeRunRIO program {})
  drained <- liftEffect (Ref.read drainedRef)
  pure
    ( if drained == expected then okResult
      else failResult (Array.length drained - Array.length expected)
    )

-- | Dropping THub overflow. A single subscriber with buffer
-- | size `buffer` does not consume while the publisher pushes
-- | `publishCount > buffer` values. Each publish records its
-- | boolean return. After publishing completes, the subscriber
-- | drains all available values. The invariant: the first
-- | `buffer` publishes returned `true`, the rest returned
-- | `false`; the drained values are exactly the first `buffer`
-- | values published, in order (Dropping drops the new value
-- | on overflow, the survivors are the early arrivals).
-- |
-- | Random parameters: `buffer` (2..6), `publishCount`
-- | (`buffer + 2`..`buffer * 3`).
hubDroppingScenario
  :: { buffer :: Int, publishCount :: Int }
  -> Aff ScenarioResult
hubDroppingScenario opts = do
  returnsRef <- liftEffect (Ref.new ([] :: Array Boolean))
  drainedRef <- liftEffect (Ref.new ([] :: Array Int))
  let
    expectedReturns =
      map (\i -> i <= opts.buffer) (Array.range 1 opts.publishCount)
    expectedDrained = Array.range 1 opts.buffer

    program :: RIO () () Unit
    program = do
      hub :: THub Int <- atomically (newDroppingTHub opts.buffer)
      sub <- atomically (subscribeTHub hub)
      for_ (Array.range 1 opts.publishCount) \v -> do
        r <- atomically (publishTHub hub v)
        liftEffect (Ref.modify_ (\xs -> Array.snoc xs r) returnsRef)
      let
        drain = do
          m <- atomically (tryTakeSubscription sub)
          case m of
            Nothing -> pure unit
            Just v -> do
              liftEffect (Ref.modify_ (\xs -> Array.snoc xs v) drainedRef)
              drain
      drain
      atomically (unsubscribeTHub sub)

  _ <- attempt (unsafeRunRIO program {})
  returns <- liftEffect (Ref.read returnsRef)
  drained <- liftEffect (Ref.read drainedRef)
  pure
    ( if returns == expectedReturns && drained == expectedDrained then
        okResult
      else failResult (Array.length drained - opts.buffer)
    )

-- | TSemaphore permit return on every termination path. Allocate
-- | a semaphore with `permits` permits; fork `workers` fibers
-- | each holding one permit via `withTSemaphore` while doing a
-- | small randomised body. Each body randomly succeeds, fails
-- | with a typed error, or is killed mid-hold by the harness.
-- | After every worker has settled (joined or interrupted), the
-- | semaphore's available count must equal the original `permits`.
-- |
-- | Random parameters: `permits` (2..5), `workers` (3..12),
-- | `failPct` (0..40), `killPct` (0..40), `holdMs` (1..6).
semaphoreScenario
  :: { permits :: Int
     , workers :: Int
     , failPct :: Int
     , killPct :: Int
     , holdMs :: Int
     }
  -> Aff ScenarioResult
semaphoreScenario opts = do
  let
    worker :: TSemaphore -> RIO () (boom :: Unit) Unit
    worker sem = withTSemaphore sem do
      f <- liftEffect (randomInt 0 99)
      liftAff (delay (Milliseconds (toNumber opts.holdMs)))
      if f < opts.failPct then fail (Proxy :: Proxy "boom") unit
      else pure unit

    program :: RIO () () Int
    program = do
      sem <- atomically (newTSemaphore opts.permits)
      fibers <- traverse (\_ -> fork (worker sem))
        (Array.range 1 opts.workers)
      _ <- traverse
        ( \fib -> do
            kk <- liftEffect (randomInt 0 99)
            if kk < opts.killPct then interrupt fib
            else catchAll (\_ -> pure unit) (void (join fib))
        )
        fibers
      atomically (availableTSemaphore sem)

  e <- attempt (unsafeRunRIO program {})
  case e of
    Right (Right available) ->
      pure
        ( if available == opts.permits then okResult
          else failResult (opts.permits - available)
        )
    _ ->
      pure (failResult (-1))
