module Test.RIO.Aff.STM.PropertiesSpec (spec) where

import Prelude

import Data.Array (index) as Array
import Data.Foldable (and, for_, sum)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample', vectorOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Concurrency (parTraverse)
import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (TRef, atomically, modifyTRef, newTRef, readTRef)

-- | One scripted transfer: move `amount` from account `from` to
-- | account `to`. Indices are constrained to fall inside the
-- | generated `balances` array by the generator.
type Transfer = { from :: Int, to :: Int, amount :: Int }

-- | A scenario carries both the initial account balances and the
-- | sequence of transfers to apply. The generator picks the
-- | balance count first, then constrains every transfer's
-- | `from`/`to` to that count so no out-of-range index escapes
-- | into the test body.
type Scenario =
  { balances :: Array Int
  , transfers :: Array Transfer
  }

-- | Generate a scenario with `2..6` accounts, balances in
-- | `0..100`, and `0..30` transfers each carrying an amount in
-- | `0..50`. Small enough that hundreds of concurrent
-- | transactions per sample stay cheap; wide enough that
-- | contention is the rule rather than the exception.
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

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

-- | Run one transfer atomically. The transaction first reads the
-- | source account; if it lacks the requested amount, it commits
-- | without changing anything. When the funds are present, both
-- | writes happen atomically: a concurrent reader inside another
-- | transaction can never see the source debited without the
-- | destination credited.
-- |
-- | Same-account transfers (`from == to`) are a no-op so the
-- | conservation properties below stay clean (a self-transfer
-- | would still preserve the sum, but a regression that wrote
-- | both legs to the same `TRef` could mask the bug).
runTransfer
  :: Array (TRef Int) -> Transfer -> RIO () () Unit
runTransfer accounts t = case Array.index accounts t.from, Array.index accounts t.to of
  Just fromRef, Just toRef
    | t.from /= t.to ->
        atomically do
          fromBal <- readTRef fromRef
          if fromBal < t.amount then pure unit
          else do
            modifyTRef fromRef (\b -> b - t.amount)
            modifyTRef toRef (\b -> b + t.amount)
  _, _ -> pure unit

-- | The shared setup: allocate one `TRef` per balance, fire every
-- | scripted transfer in parallel through `parTraverse`, then
-- | read the final balances back out. Both properties share
-- | this body and differ only in how they inspect the result.
runScenario :: Scenario -> RIO () () (Array Int)
runScenario scenario = do
  accounts <- traverse
    (\b -> atomically (newTRef b))
    scenario.balances
  _ <- parTraverse (runTransfer accounts) scenario.transfers
  traverse (\r -> atomically (readTRef r)) accounts

spec :: Spec Unit
spec = describe "RIO.Aff.STM (property tests)" do
  -- The unit pin in `STMSpec` ("concurrent increments preserve
  -- the invariant") fixes a single `TRef` counter and fires 50
  -- atomic increments at it. Generalise across the broader
  -- conservation invariant: many `TRef`s, a stream of random
  -- transfers, fired in parallel through `parTraverse`. If
  -- `atomically` ever leaked a half-applied transaction across
  -- contended writes (committing one leg without the other),
  -- the sum after would diverge from the sum before. The unit
  -- pin would still pass (its workload only touches a single
  -- `TRef`); this property would fail.
  it "concurrent transfers preserve the total sum" do
    forAll genScenario \scenario -> do
      finalBalances <- runRIO' (runScenario scenario)
      sum finalBalances `shouldEqual` sum scenario.balances

  -- Same setup, but pin the non-negative-balance invariant.
  -- A regression that allowed a transfer to commit without
  -- first observing the funds requirement (e.g. one that
  -- short-circuited the `if fromBal < t.amount` guard under
  -- contention) would manifest here as an account whose
  -- balance went below zero.
  it "no account balance goes negative under concurrent transfers" do
    forAll genScenario \scenario -> do
      finalBalances <- runRIO' (runScenario scenario)
      and (map (_ >= 0) finalBalances) `shouldEqual` true
