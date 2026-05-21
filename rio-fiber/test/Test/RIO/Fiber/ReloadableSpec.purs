module Test.RIO.Fiber.ReloadableSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Reloadable as Reloadable
import RIO.Fiber.Schedule as Sch
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "rio-fiber: Reloadable" do
  it "seeds the initial value at make time" do
    acquires <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect
        (Ref.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      prog :: F.RIO () () { acquires :: Int, value :: Int }
      prog = Scope.scoped \scope -> do
        slot <- Reloadable.make scope (Sch.recurs 0) acquire
        v <- Reloadable.get slot
        a <- F.liftEffect (Ref.read acquires)
        pure { acquires: a, value: v }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.value `shouldEqual` 1
        rec.acquires `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "scheduled re-acquires update the slot" do
    acquires <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect
        (Ref.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      prog :: F.RIO () () { initial :: Int, later :: Int, calls :: Int }
      prog = Scope.scoped \scope -> do
        slot <- Reloadable.make scope (Sch.spaced (Milliseconds 5.0)) acquire
        initial <- Reloadable.get slot
        F.sleep (Milliseconds 40.0)
        later <- Reloadable.get slot
        calls <- F.liftEffect (Ref.read acquires)
        pure { initial, later, calls }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.initial `shouldEqual` 1
        if rec.later > rec.initial then pure unit
        else fail ("expected later > initial, got " <> show rec)
        if rec.calls >= 2 then pure unit
        else fail ("expected at least 2 acquire calls, got " <> show rec.calls)
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "manual reload re-runs acquire immediately" do
    acquires <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect
        (Ref.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      prog :: F.RIO () () { before :: Int, after :: Int, calls :: Int }
      prog = Scope.scoped \scope -> do
        -- recurs 0 emits Halt immediately, so no background ticks fire
        slot <- Reloadable.make scope (Sch.recurs 0) acquire
        before <- Reloadable.get slot
        Reloadable.reload slot
        after <- Reloadable.get slot
        calls <- F.liftEffect (Ref.read acquires)
        pure { before, after, calls }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.before `shouldEqual` 1
        rec.after `shouldEqual` 2
        rec.calls `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "scheduled-loop failures are swallowed and the slot keeps its prior value" do
    acquires <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () Boom Int
      acquire = do
        n <- F.liftEffect
          (Ref.modify' (\k -> { state: k + 1, value: k + 1 }) acquires)
        if n == 1 then pure 100
        else F.fail (Variant.inj (Proxy :: Proxy "boom") ("tick " <> show n))

      prog :: F.RIO () Boom { value :: Int, calls :: Int }
      prog = Scope.scoped \scope -> do
        slot <- Reloadable.make scope (Sch.spaced (Milliseconds 5.0)) acquire
        F.sleep (Milliseconds 40.0)
        v <- Reloadable.get slot
        calls <- F.liftEffect (Ref.read acquires)
        pure { value: v, calls }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.value `shouldEqual` 100
        if rec.calls >= 2 then pure unit
        else fail ("expected multiple acquire attempts, got " <> show rec.calls)
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "manual reload re-raises acquire failures (unlike the scheduled loop)" do
    calls <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () Boom Int
      acquire = do
        n <- F.liftEffect
          (Ref.modify' (\k -> { state: k + 1, value: k + 1 }) calls)
        if n == 1 then pure 7
        else F.fail (Variant.inj (Proxy :: Proxy "boom") "later")

      prog :: F.RIO () Boom { initial :: Int, reloadFailed :: Boolean, valueAfter :: Int }
      prog = Scope.scoped \scope -> do
        slot <- Reloadable.make scope (Sch.recurs 0) acquire
        initial <- Reloadable.get slot
        cause <- F.causeOf (Reloadable.reload slot)
        let reloadFailed = case cause of
              Left _ -> true
              Right _ -> false
        valueAfter <- Reloadable.get slot
        pure { initial, reloadFailed, valueAfter }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.initial `shouldEqual` 7
        rec.reloadFailed `shouldEqual` true
        rec.valueAfter `shouldEqual` 7
      Fail v -> Variant.match { boom: \s -> fail ("got Fail boom: " <> s) } v
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
