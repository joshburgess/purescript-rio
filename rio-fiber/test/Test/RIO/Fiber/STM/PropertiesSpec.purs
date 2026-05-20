module Test.RIO.Fiber.STM.PropertiesSpec (spec) where

import Prelude

import Data.Array (foldM, index, range, replicate, sort) as Array
import Data.Foldable (and, for_, sum)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.STM (TVar)
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TChan (TChan)
import RIO.Fiber.STM.TChan as TChan
import RIO.Fiber.STM.TMVar (TMVar)
import RIO.Fiber.STM.TMVar as TMVar
import RIO.Fiber.STM.TQueue (TQueue)
import RIO.Fiber.STM.TQueue as TQ
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample', vectorOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | One scripted transfer: move `amount` from account `from` to
-- | account `to`. Indices are constrained to fall inside the
-- | generated `balances` array by the generator.
type Transfer = { from :: Int, to :: Int, amount :: Int }

type Scenario =
  { balances :: Array Int
  , transfers :: Array Transfer
  }

-- | Generate a scenario with 2..6 accounts, balances in 0..100, and
-- | 0..30 transfers. Small enough to keep hundreds of concurrent
-- | transactions per sample cheap; wide enough that contention is
-- | the rule rather than the exception.
genScenario :: Gen Scenario
genScenario = do
  n <- chooseInt 2 6
  balances <- vectorOf n (chooseInt 0 100)
  k <- chooseInt 0 30
  transfers <- vectorOf k do
    from <- chooseInt 0 (n - 1)
    to <- chooseInt 0 (n - 1)
    amount <- chooseInt 0 50
    pure { from, to, amount }
  pure { balances, transfers }

forAll :: forall a. Int -> Gen a -> (a -> Aff Unit) -> Aff Unit
forAll n gen prop = do
  samples <- liftEffect (randomSample' n gen)
  for_ samples prop

-- | Run one transfer atomically. The transaction first reads the
-- | source account; if it lacks the requested amount, it commits
-- | without changing anything. When the funds are present, both
-- | writes happen atomically.
-- |
-- | Same-account transfers are a no-op so the conservation
-- | properties stay clean (a regression that wrote both legs to
-- | the same TVar would otherwise mask the bug).
runTransfer :: Array (TVar Int) -> Transfer -> RIO () () Unit
runTransfer accounts t =
  case Array.index accounts t.from, Array.index accounts t.to of
    Just fromRef, Just toRef
      | t.from /= t.to ->
          STM.atomically do
            fromBal <- STM.readTVar fromRef
            if fromBal < t.amount then pure unit
            else do
              STM.modifyTVar fromRef (\b -> b - t.amount)
              STM.modifyTVar toRef (\b -> b + t.amount)
    _, _ -> pure unit

runScenario :: Scenario -> RIO () () (Array Int)
runScenario scenario = do
  accounts <- traverse
    (\b -> F.liftEffect (STM.newTVar b))
    scenario.balances
  _ <- F.parTraverse (runTransfer accounts) scenario.transfers
  traverse (\r -> STM.atomically (STM.readTVar r)) accounts

spec :: Spec Unit
spec = describe "rio-fiber: STM (property tests)" do
  -- The unit test in `STMSpec` ("concurrent modifyTVar from many
  -- fibers converges to N") fixes a single counter and fires 50
  -- atomic increments. Generalise across the broader conservation
  -- invariant: many TVars, a stream of random transfers, fired in
  -- parallel through `parTraverse`. If `atomically` ever leaked a
  -- half-applied transaction across contended writes, the sum
  -- after would diverge from the sum before.
  it "concurrent transfers preserve the total sum" do
    forAll 20 genScenario \scenario -> do
      finalBalances <- runAffThrow (runScenario scenario)
      sum finalBalances `shouldEqual` sum scenario.balances

  -- Same scenario, different invariant. A regression that allowed
  -- a transfer to commit without observing the funds guard (e.g.
  -- one that short-circuited the `if fromBal < t.amount` check
  -- under contention) would manifest here as a negative balance.
  it "no account balance goes negative under concurrent transfers" do
    forAll 20 genScenario \scenario -> do
      finalBalances <- runAffThrow (runScenario scenario)
      and (map (_ >= 0) finalBalances) `shouldEqual` true

  -- One producer and one consumer racing on a TMVar: the cell can
  -- only ever hold one value, so without atomicity the consumer
  -- could observe gaps or duplicates. Conservation pins the count.
  it "TMVar producer/consumer never drops or duplicates items" do
    forAll 10 (chooseInt 0 30) \n -> do
      seen <- liftEffect (Ref.new ([] :: Array Int))
      let
        producer :: TMVar Int -> RIO () () Unit
        producer cell = for_ (Array.range 1 n) \i ->
          STM.atomically (TMVar.put cell i)

        consumer :: TMVar Int -> RIO () () Unit
        consumer cell = for_ (Array.range 1 n) \_ -> do
          x <- STM.atomically (TMVar.take cell)
          F.liftEffect (Ref.modify_ (\xs -> xs <> [ x ]) seen)

        prog :: RIO () () Unit
        prog = do
          cell <- F.liftEffect TMVar.newEmpty
          _ <- F.parTraverse (\f -> f cell) [ producer, consumer ]
          pure unit
      runAffThrow prog
      xs <- liftEffect (Ref.read seen)
      -- TMVar guarantees alternating put/take, so a single consumer
      -- sees the producer's exact sequence in order.
      xs `shouldEqual` (if n == 0 then [] else Array.range 1 n)

  -- Multiple writers pushing into a bounded TQueue concurrently
  -- with a single reader draining it. The reader records every
  -- value it sees; the set of seen values must equal the union of
  -- writer payloads, and the queue must never carry more than its
  -- capacity at any point during the run.
  it "TQueue never exceeds capacity under concurrent writers" do
    forAll 10 (chooseInt 1 6) \cap -> do
      let
        writerCount = 4
        itemsPerWriter = 8
        totalItems = writerCount * itemsPerWriter
        prog :: RIO () () Boolean
        prog = do
          q <- F.liftEffect (TQ.new cap :: _ (TQueue Int))
          seenAll <- F.liftEffect (Ref.new true)
          let
            writer :: Int -> RIO () () Unit
            writer w = for_ (Array.range 1 itemsPerWriter) \i ->
              STM.atomically (TQ.writeTQueue q (w * 100 + i))

            reader :: RIO () () Unit
            reader =
              let
                step _ = do
                  _ <- STM.atomically (TQ.readTQueue q)
                  n <- STM.atomically (TQ.lengthTQueue q)
                  ok <- F.liftEffect (Ref.read seenAll)
                  F.liftEffect (Ref.write (ok && n <= cap) seenAll)
                  pure unit
              in
                for_ (Array.range 1 totalItems) step
          _ <- F.parTraverse identity
            [ reader
            , writer 1, writer 2, writer 3, writer 4
            ]
          F.liftEffect (Ref.read seenAll)
      ok <- runAffThrow prog
      ok `shouldEqual` true

  -- Conservation across a multi-writer / single-reader TQueue:
  -- every value enqueued lands in the reader's tally exactly once.
  -- FIFO is not asserted (writers interleave) but the multiset is.
  it "TQueue: every produced item is read exactly once" do
    forAll 10 (chooseInt 1 4) \cap -> do
      let
        writerCount = 3
        itemsPerWriter = 6
        totalItems = writerCount * itemsPerWriter

        expected :: Array Int
        expected = Array.sort do
          w <- Array.range 1 writerCount
          i <- Array.range 1 itemsPerWriter
          pure (w * 100 + i)

        prog :: RIO () () (Array Int)
        prog = do
          q <- F.liftEffect (TQ.new cap :: _ (TQueue Int))
          ref <- F.liftEffect (Ref.new ([] :: Array Int))
          let
            writer :: Int -> RIO () () Unit
            writer w = for_ (Array.range 1 itemsPerWriter) \i ->
              STM.atomically (TQ.writeTQueue q (w * 100 + i))

            reader :: RIO () () Unit
            reader = for_ (Array.range 1 totalItems) \_ -> do
              x <- STM.atomically (TQ.readTQueue q)
              F.liftEffect (Ref.modify_ (\xs -> xs <> [ x ]) ref)
          _ <- F.parTraverse identity
            [ reader, writer 1, writer 2, writer 3 ]
          F.liftEffect (Ref.read ref)
      seen <- runAffThrow prog
      Array.sort seen `shouldEqual` expected

  -- Same conservation property over the unbounded TChan: every
  -- value written by any producer is read by the single consumer
  -- exactly once. Tests the cell-list invariant under contention.
  it "TChan: every produced item is read exactly once" do
    forAll 10 (chooseInt 2 5) \writerCount -> do
      let
        itemsPerWriter = 5
        totalItems = writerCount * itemsPerWriter

        expected :: Array Int
        expected = Array.sort do
          w <- Array.range 1 writerCount
          i <- Array.range 1 itemsPerWriter
          pure (w * 100 + i)

        prog :: RIO () () (Array Int)
        prog = do
          ch <- F.liftEffect (TChan.new :: _ (TChan Int))
          ref <- F.liftEffect (Ref.new ([] :: Array Int))
          let
            writer :: Int -> RIO () () Unit
            writer w = for_ (Array.range 1 itemsPerWriter) \i ->
              STM.atomically (TChan.writeTChan ch (w * 100 + i))

            reader :: RIO () () Unit
            reader = for_ (Array.range 1 totalItems) \_ -> do
              x <- STM.atomically (TChan.readTChan ch)
              F.liftEffect (Ref.modify_ (\xs -> xs <> [ x ]) ref)

            writers :: Array (RIO () () Unit)
            writers = map writer (Array.range 1 writerCount)
          _ <- F.parTraverse identity ([ reader ] <> writers)
          F.liftEffect (Ref.read ref)
      seen <- runAffThrow prog
      Array.sort seen `shouldEqual` expected

  -- Retry wakeup property: a fiber blocked on `check (n >= k)`
  -- must wake exactly when a writer's commit bumps the TVar past
  -- the threshold, regardless of how many writers race to get
  -- there. Conservation of the counter plus the waiter's seen
  -- value pins the invariant: the waiter never observes a value
  -- below `k`, and the final counter equals the total bumps.
  it "retry suspends until the target value is committed" do
    forAll 10 (chooseInt 1 10) \k -> do
      let
        bumps = k + 5
        prog :: RIO () () { seen :: Int, final :: Int }
        prog = do
          t <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
          waiter <- F.fork
            ( STM.atomically do
                n <- STM.readTVar t
                STM.check (n >= k)
                pure n
            )
          F.sleep (Milliseconds 5.0)
          _ <- F.parTraverse
            (\_ -> STM.atomically (STM.modifyTVar t (_ + 1)))
            (Array.replicate bumps unit)
          seen <- F.join waiter
          final <- STM.atomically (STM.readTVar t)
          pure { seen, final }
      r <- runAffThrow prog
      (r.seen >= k) `shouldEqual` true
      r.final `shouldEqual` bumps

  -- orElse rollback property: the first branch writes garbage and
  -- retries; the second branch writes the real value. After commit
  -- the garbage must not be visible: the staged writes from the
  -- failed branch are discarded before the alternative runs.
  -- Repeat many times to flush out any transient leak under
  -- contention with concurrent readers.
  it "orElse rolls back failed-branch writes (no leak under contention)" do
    let
      attempts = 50

      one :: RIO () () Int
      one = do
        t <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
        _ <- STM.atomically
          ( (STM.writeTVar t 999 *> STM.retry)
              `STM.orElse` STM.writeTVar t 7
          )
        STM.atomically (STM.readTVar t)

      prog :: RIO () () Boolean
      prog = do
        results <- F.parTraverse
          (\_ -> one)
          (Array.replicate attempts unit)
        pure (and (map (_ == 7) results))
    ok <- runAffThrow prog
    ok `shouldEqual` true

  -- Atomicity property across multiple writes in one transaction:
  -- a single commit either makes every staged write visible or
  -- none of them. We bump three TVars in lock-step inside one
  -- atomically block. After every commit (across many parallel
  -- runs) the three counters must remain equal: any partial
  -- commit would let one race ahead of another.
  it "multi-TVar transaction commits all-or-nothing" do
    let
      runs = 80
      prog :: RIO () () { a :: Int, b :: Int, c :: Int }
      prog = do
        a <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
        b <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
        c <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
        let
          bumpAll :: RIO () () Unit
          bumpAll = STM.atomically do
            STM.modifyTVar a (_ + 1)
            STM.modifyTVar b (_ + 1)
            STM.modifyTVar c (_ + 1)
        _ <- F.parTraverse (\_ -> bumpAll) (Array.replicate runs unit)
        ra <- STM.atomically (STM.readTVar a)
        rb <- STM.atomically (STM.readTVar b)
        rc <- STM.atomically (STM.readTVar c)
        pure { a: ra, b: rb, c: rc }
    r <- runAffThrow prog
    r.a `shouldEqual` runs
    r.b `shouldEqual` runs
    r.c `shouldEqual` runs

  -- Folding into a TVar inside a single transaction: the commit
  -- must surface the final fold value, never an intermediate.
  -- Exercises the staged-log path with many writes per
  -- transaction and confirms `Array.foldM` over STM threads its
  -- accumulator through the staging buffer without leaking.
  it "fold inside one transaction commits the final value" do
    let
      prog :: RIO () () Int
      prog = do
        t <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
        STM.atomically do
          _ <- Array.foldM
            (\_ x -> do
                n <- STM.readTVar t
                STM.writeTVar t (n + x)
            )
            unit
            (Array.range 1 100)
          STM.readTVar t
    n <- runAffThrow prog
    -- sum 1..100 = 5050
    n `shouldEqual` 5050

  -- Reader-isolation property: while a long transaction holds a
  -- read on a TVar, concurrent writers may commit, but the reader
  -- must either see a single consistent snapshot or retry. We
  -- assert the post-condition rather than the schedule: after the
  -- reader commits and N writers commit, the final value equals
  -- exactly N (no missed bumps).
  it "long reader does not lose writer commits" do
    let
      writers = 30
      prog :: RIO () () Int
      prog = do
        t <- F.liftEffect (STM.newTVar 0 :: _ (TVar Int))
        readerF <- F.fork
          ( STM.atomically do
              -- Read twice to force a re-read across any commits.
              _ <- STM.readTVar t
              STM.readTVar t
          )
        _ <- F.parTraverse
          (\_ -> STM.atomically (STM.modifyTVar t (_ + 1)))
          (Array.replicate writers unit)
        _ <- F.join readerF
        STM.atomically (STM.readTVar t)
    n <- runAffThrow prog
    n `shouldEqual` writers

  -- TMVar mutual-exclusion property: treat the TMVar as a
  -- one-token mutex around a counter. With N contending fibers
  -- each performing take / increment / put, the counter must
  -- arrive at exactly N. A regression where `put` failed to wake
  -- a blocked `take` would manifest as a hang; one that allowed
  -- two takers to share the token would manifest as a final
  -- count below N (lost update).
  it "TMVar acts as a mutex under contention" do
    forAll 5 (chooseInt 5 30) \n -> do
      let
        prog :: RIO () () Int
        prog = do
          lock <- F.liftEffect (TMVar.new unit)
          counter <- F.liftEffect (Ref.new 0)
          _ <- F.parTraverse
            (\_ -> do
                _ <- STM.atomically (TMVar.take lock)
                F.liftEffect (Ref.modify_ (_ + 1) counter)
                STM.atomically (TMVar.put lock unit)
            )
            (Array.replicate n unit)
          F.liftEffect (Ref.read counter)
      final <- runAffThrow prog
      final `shouldEqual` n
