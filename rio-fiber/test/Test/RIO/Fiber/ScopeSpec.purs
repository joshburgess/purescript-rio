module Test.RIO.Fiber.ScopeSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Scope" do
  it "scoped returns the body's result" do
    let
      prog :: F.RIO () () Int
      prog = Scope.scoped \_ -> pure 42
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "acquireRelease registers a finalizer that fires on scope close" do
    ref <- liftEffect (Ref.new 0)
    let
      prog :: F.RIO () () Int
      prog = Scope.scoped \s -> do
        n <- Scope.acquireRelease s (pure 7) (\_ -> F.liftEffect (Ref.write 99 ref))
        pure n
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)
    -- give the fire-and-forget finalizer a tick to run
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    seen <- liftEffect (Ref.read ref)
    seen `shouldEqual` 99

  it "fires finalizers in LIFO order" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: String -> F.RIO () () Unit
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      prog :: F.RIO () () Unit
      prog = Scope.scoped \s -> do
        _ <- Scope.acquireRelease s (pure 1) (\_ -> record "first")
        _ <- Scope.acquireRelease s (pure 2) (\_ -> record "second")
        _ <- Scope.acquireRelease s (pure 3) (\_ -> record "third")
        pure unit
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "third", "second", "first" ]

  it "closes the scope when the body raises a typed failure" do
    ref <- liftEffect (Ref.new false)
    let
      prog :: F.RIO () (boom :: String) Unit
      prog = Scope.scoped \s -> do
        _ <- Scope.acquireRelease s (pure unit)
          (\_ -> F.liftEffect (Ref.write true ref))
        F.fail (Variant.inj (Proxy :: _ "boom") "nope")
    out <- runAff prog {}
    case out of
      Fail _ -> pure unit
      other -> fail ("expected Fail, got " <> describeOutcome other)
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    fired <- liftEffect (Ref.read ref)
    fired `shouldEqual` true

  it "closes the scope when the body is interrupted" do
    ref <- liftEffect (Ref.new false)
    let
      action :: F.RIO () () Unit
      action = Scope.scoped \s -> do
        _ <- Scope.acquireRelease s (pure unit)
          (\_ -> F.liftEffect (Ref.write true ref))
        F.sleep (Milliseconds 100.0)

      prog :: F.RIO () () Unit
      prog = do
        fib <- F.fork action
        F.sleep (Milliseconds 10.0)
        F.interrupt fib
        _ <- F.join fib
        pure unit
    _out <- runAff prog {}
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    fired <- liftEffect (Ref.read ref)
    fired `shouldEqual` true

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
