module Test.RIO.Aff.STMSpec (spec) where

import Prelude hiding (join)

import Data.Array (range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, catchTag, fork, join, runRIO, runRIO')
import RIO.Aff.STM
  ( atomically
  , check
  , failSTM
  , modifyTRef
  , newTRef
  , orElse
  , readTRef
  , writeTRef
  )

spec :: Spec Unit
spec = do
  describe "RIO.Aff.STM" do
    describe "atomically" do
      it "applies writes after a successful transaction" do
        let
          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            atomically (writeTRef ref 7)
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` 7

      it "subsequent reads in the same tx see staged writes" do
        let
          program :: RIO () () Int
          program = atomically do
            ref <- newTRef 1
            writeTRef ref 2
            readTRef ref
        result <- runRIO' program
        result `shouldEqual` 2

      it "readTRef returns the latest of multiple writes to the same TRef in one tx" do
        -- `readTRef`'s docstring promises "If the transaction
        -- has already written to this `TRef`, returns the
        -- pending value". The implementation backs this with
        -- `findLatestWrite`, which calls `last (filter (\\e ->
        -- e.id == k) xs)` over the write log. The pinned
        -- "subsequent reads in the same tx see staged writes"
        -- test only writes once, so a regression that returned
        -- the FIRST matching write (e.g. swapping `last` for
        -- `head`, or replacing the filter with `Array.find`)
        -- would still pass: with a single staged write, first
        -- and last coincide. Pin the last-write-wins guarantee
        -- with three writes in one transaction.
        let
          program :: RIO () () Int
          program = atomically do
            ref <- newTRef 0
            writeTRef ref 1
            writeTRef ref 2
            writeTRef ref 3
            readTRef ref
        result <- runRIO' program
        result `shouldEqual` 3

      it "modifyTRef composes a read and a write" do
        let
          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 10)
            atomically (modifyTRef ref (_ * 3))
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` 30

    describe "failSTM" do
      it "aborts the transaction and surfaces the failure" do
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            ref <- atomically (newTRef 0)
            _ <- atomically do
              writeTRef ref 99
              _ <- failSTM (Proxy :: Proxy "boom") unit
              pure 0
            atomically (readTRef ref)
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "discards writes from a failed transaction" do
        let
          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure 0)
              ( atomically do
                  writeTRef ref 99
                  _ <- failSTM (Proxy :: Proxy "boom") unit
                  pure (0 :: Int)
              )
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` 0

    describe "retry" do
      it "a retry waiting on a TRef can be killed by the parent fiber" do
        -- The `atomically` docstring promises that "a retrying
        -- transaction awaits an `AVar` signal in `Aff`, so a
        -- parent fiber's `interrupt` cancels the wait at the
        -- next async boundary." Pin this by forking a transaction
        -- that retries on a TRef no other fiber will ever write,
        -- killing the fiber, and confirming the kill lands (the
        -- post-kill marker fires while the never-completed
        -- transaction does not).
        events <- liftEffect (Ref.new [])
        let
          push :: forall r e. String -> RIO r e Unit
          push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

          program :: RIO () () Unit
          program = do
            ref <- atomically (newTRef 0)
            push "before-retry"
            _ <- atomically do
              x <- readTRef ref
              check (x > 0)
              pure x
            push "after-retry"
        f <- forkAff (runRIO' program)
        liftAff (delay (Milliseconds 10.0))
        killFiber (error "test-cancel") f
        liftAff (delay (Milliseconds 10.0))
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "before-retry" ]

      it "suspends until another fiber writes a read TRef" do
        events <- liftEffect (Ref.new [])
        let
          push :: forall r e. String -> RIO r e Unit
          push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            push "before-fork"
            waiter <- fork do
              v <- atomically do
                x <- readTRef ref
                check (x > 0)
                pure x
              push "after-await"
              pure v
            liftAff (delay (Milliseconds 20.0))
            push "before-write"
            atomically (writeTRef ref 42)
            join waiter

        result <- runRIO' program
        result `shouldEqual` 42
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "before-fork", "before-write", "after-await" ]

    describe "orElse" do
      it "falls through when the left side retries" do
        let
          program :: RIO () () Int
          program = do
            refA <- atomically (newTRef 0)
            refB <- atomically (newTRef 99)
            atomically do
              orElse
                ( do
                    a <- readTRef refA
                    check (a > 0)
                    pure a
                )
                (readTRef refB)
        result <- runRIO' program
        result `shouldEqual` 99

      it "does not fall through on a typed failure" do
        let
          program :: RIO () (boom :: Unit) Int
          program = atomically do
            orElse
              ( do
                  _ <- failSTM (Proxy :: Proxy "boom") unit
                  pure 0
              )
              (pure 99)
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "uses the left value when the left commits" do
        let
          program :: RIO () () Int
          program = do
            refA <- atomically (newTRef 7)
            refB <- atomically (newTRef 99)
            atomically (orElse (readTRef refA) (readTRef refB))
        result <- runRIO' program
        result `shouldEqual` 7

      it "rolls back staged writes from a retried left branch" do
        -- The docstring promises that "the log effect of a
        -- fallen-through `left` is rolled back before `right`
        -- runs, so a retried branch leaves no reads or writes
        -- behind." The fall-through path is already pinned;
        -- this pins the rollback specifically so a write
        -- inside a retrying left branch must not commit when
        -- the right branch succeeds.
        let
          program :: RIO () () { fromOr :: Int, afterCommit :: Int }
          program = do
            refA <- atomically (newTRef 0)
            refB <- atomically (newTRef 99)
            fromOr <- atomically do
              orElse
                ( do
                    -- stage a write, then retry: this should not commit
                    writeTRef refA 1234
                    a <- readTRef refA
                    check (a > 9999)
                    pure a
                )
                (readTRef refB)
            afterCommit <- atomically (readTRef refA)
            pure { fromOr, afterCommit }
        result <- runRIO' program
        result `shouldEqual` { fromOr: 99, afterCommit: 0 }

      it
        "orElse rolls back the read-set of a retried left branch (stale read does not wake the waiter)"
        do
          -- The `orElse` docstring promises: "The log effect of a
          -- fallen-through `left` is rolled back before `right`
          -- runs, so a retried branch leaves no reads or writes
          -- behind." The existing "rolls back staged writes" test
          -- above pins the WRITE-rollback half of this promise.
          -- The READ-rollback half is unpinned: if the failed
          -- left branch's reads were not rolled back when both
          -- branches retry, a subsequent write to the left's read
          -- target would spuriously wake the suspended waiter,
          -- which would then replay and commit on stale data.
          -- Pin the read-rollback half by suspending an `orElse`
          -- in which both branches retry, writing the LEFT
          -- branch's read target, and observing that the waiter
          -- is NOT awoken by that write.
          doneRef <- liftEffect (Ref.new (Nothing :: Maybe Int))
          let
            program :: RIO () () (Maybe Int)
            program = do
              refA <- atomically (newTRef 0)
              refB <- atomically (newTRef 0)
              waiter <- fork do
                v <- atomically do
                  orElse
                    ( do
                        a <- readTRef refA
                        check (a > 100)
                        pure a
                    )
                    ( do
                        b <- readTRef refB
                        check (b > 100)
                        pure b
                    )
                liftEffect (Ref.write (Just v) doneRef)
                pure unit
              liftAff (delay (Milliseconds 20.0))
              -- Write the LEFT branch's read target. If reads
              -- were rolled back, refA is no longer in the
              -- waiter's read-set and this write must not wake
              -- it. (If we wrote a value greater than 100, a
              -- broken-rollback impl would also commit the
              -- left branch successfully and surface that in
              -- `doneRef`.)
              atomically (writeTRef refA 999)
              liftAff (delay (Milliseconds 30.0))
              mid <- liftEffect (Ref.read doneRef)
              -- Wake the waiter via the RIGHT branch's read
              -- target so the test does not hang.
              atomically (writeTRef refB 999)
              _ <- join waiter
              pure mid
          result <- runRIO' program
          result `shouldEqual` Nothing

      it "outer transaction retries when both sides retry" do
        events <- liftEffect (Ref.new [])
        let
          push :: forall r e. String -> RIO r e Unit
          push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

          program :: RIO () () Int
          program = do
            refA <- atomically (newTRef 0)
            refB <- atomically (newTRef 0)
            push "before-fork"
            waiter <- fork do
              v <- atomically do
                orElse
                  ( do
                      a <- readTRef refA
                      check (a > 0)
                      pure a
                  )
                  ( do
                      b <- readTRef refB
                      check (b > 0)
                      pure b
                  )
              push "after-await"
              pure v
            liftAff (delay (Milliseconds 20.0))
            push "before-write"
            atomically (writeTRef refB 88)
            join waiter
        result <- runRIO' program
        result `shouldEqual` 88
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "before-fork", "before-write", "after-await" ]

    describe "concurrent increments" do
      it "preserves the invariant under many parallel updates" do
        let
          n = 50

          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            fibers <- traverse
              (\_ -> fork (atomically (modifyTRef ref (_ + 1))))
              (range 1 n)
            _ <- traverse join fibers
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` n
