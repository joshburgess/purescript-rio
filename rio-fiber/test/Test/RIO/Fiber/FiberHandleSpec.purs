module Test.RIO.Fiber.FiberHandleSpec (spec) where

import Prelude

import Data.Maybe (isJust, isNothing)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.FiberHandle as FH
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: FiberHandle" do
  it "stores and exposes the running fiber" do
    let
      prog :: F.RIO () () Boolean
      prog = Scope.scoped \scope -> do
        h <- FH.make scope
        _ <- FH.run h (F.sleep (Milliseconds 50.0) *> pure 42)
        mf <- FH.get h
        pure (isJust mf)
    out <- runAff prog {}
    case out of
      Success b -> b `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "second run interrupts the first" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall e. String -> F.RIO () e Unit
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      prog :: F.RIO () () Unit
      prog = Scope.scoped \scope -> do
        h <- FH.make scope
        _ <- FH.run h
          (F.ensuring (record "first-out") (F.sleep (Milliseconds 200.0)))
        F.sleep (Milliseconds 10.0)
        f2 <- FH.run h
          (F.ensuring (record "second-out") (record "second-in"))
        _ <- F.join f2
        pure unit
    out <- runAff prog {}
    case out of
      Success _ -> do
        events <- liftEffect (Ref.read log)
        events `shouldEqual` [ "first-out", "second-in", "second-out" ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "slot auto-clears after the fiber finishes" do
    let
      prog :: F.RIO () () Boolean
      prog = Scope.scoped \scope -> do
        h <- FH.make scope
        f <- FH.run h (pure 1)
        _ <- F.join f
        F.sleep (Milliseconds 5.0)
        mf <- FH.get h
        pure (isNothing mf)
    out <- runAff prog {}
    case out of
      Success b -> b `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "scope close interrupts the current occupant" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall e. String -> F.RIO () e Unit
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      prog :: F.RIO () () Unit
      prog = do
        Scope.scoped \scope -> do
          h <- FH.make scope
          _ <- FH.run h
            ( F.ensuring (record "interrupted")
                (F.sleep (Milliseconds 200.0))
            )
          F.sleep (Milliseconds 10.0)
        -- scope is closed here; the occupant has been interrupted
        -- but its finalizer needs a tick to run.
        F.sleep (Milliseconds 20.0)
    out <- runAff prog {}
    case out of
      Success _ -> do
        events <- liftEffect (Ref.read log)
        events `shouldEqual` [ "interrupted" ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "clear interrupts the current occupant and reports true" do
    let
      prog :: F.RIO () () { didInterrupt :: Boolean, after :: Boolean }
      prog = Scope.scoped \scope -> do
        h <- FH.make scope
        _ <- FH.run h (F.sleep (Milliseconds 500.0))
        F.sleep (Milliseconds 5.0)
        didInterrupt <- FH.clear h
        again <- FH.clear h
        pure { didInterrupt, after: again }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.didInterrupt `shouldEqual` true
        r.after `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
