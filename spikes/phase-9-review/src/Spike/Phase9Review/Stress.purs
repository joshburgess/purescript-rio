-- | Phase 9 (v0.3) review stress scenarios.
-- |
-- | Each scenario targets one v0.3 module and asserts a single
-- | load-bearing invariant after random scheduling and random
-- | termination modes (success, typed failure, fiber kill where
-- | applicable). The four modules under test are:
-- |
-- |   * `RIO.Logger`: annotation stack is restored after every
-- |     `withFields` block, including blocks that exit by typed
-- |     failure or are torn down while a forked child is active.
-- |   * `RIO.Local`: `locally`'s value-restore is honoured on
-- |     every termination path of the wrapped action.
-- |   * `RIO.STM.TQueue`: producer/consumer correctness under
-- |     contention. Sum of dequeued values equals sum of enqueued
-- |     values; total count matches.
-- |   * `RIO.STM.THub` (Unbounded strategy): every subscribed
-- |     consumer receives every value published after it
-- |     subscribed; sums match across all subscribers.
module Spike.Phase9Review.Stress
  ( ScenarioResult
  , loggerScenario
  , localScenario
  , queueScenario
  , hubScenario
  ) where

import Prelude hiding (join)

import Data.Array (length, range, snoc) as Array
import Data.Either (Either(..))
import Data.Foldable (all, for_, sum)
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
  , newUnboundedTHub
  , publishTHub
  , subscribeTHub
  , takeSubscription
  , unsubscribeTHub
  )
import RIO.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
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
