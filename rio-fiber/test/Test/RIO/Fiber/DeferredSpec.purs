module Test.RIO.Fiber.DeferredSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Deferred as D
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Deferred" do
  it "await resumes with the value succeed() set" do
    d <- liftEffect (D.make :: _ (D.Deferred () Int))
    let
      prog :: F.RIO () () Int
      prog = do
        fib <- F.fork (D.await d)
        F.sleep (Milliseconds 5.0)
        _ <- D.succeed d 99
        F.join fib
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "await fires synchronously when the deferred is already done" do
    d <- liftEffect (D.make :: _ (D.Deferred () Int))
    _ <- runAff (D.succeed d 7 :: F.RIO () () Boolean) {}
    let
      prog :: F.RIO () () Int
      prog = D.await d
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "await surfaces a fail() as a typed failure" do
    d <- liftEffect (D.make :: _ (D.Deferred (boom :: String) Int))
    let
      prog :: F.RIO () (boom :: String) Int
      prog = do
        fib <- F.fork (D.await d)
        F.sleep (Milliseconds 5.0)
        _ <- D.fail d (Variant.inj (Proxy :: _ "boom") "x")
        F.join fib
    out <- runAff prog {}
    case out of
      Fail v -> (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
        `shouldEqual` "x"
      other -> fail ("expected Fail, got " <> describeOutcome other)

  it "second completion returns false" do
    d <- liftEffect (D.make :: _ (D.Deferred () Int))
    let
      prog :: F.RIO () () { first :: Boolean, second :: Boolean }
      prog = do
        a <- D.succeed d 1
        b <- D.succeed d 2
        pure { first: a, second: b }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.first `shouldEqual` true
        r.second `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "poll returns Nothing then Just after completion" do
    d <- liftEffect (D.make :: _ (D.Deferred () Int))
    let
      prog :: F.RIO () () { before :: Maybe (Either _ Int), after :: Maybe (Either _ Int) }
      prog = do
        before <- D.poll d
        _ <- D.succeed d 42
        after <- D.poll d
        pure { before, after }
    out <- runAff prog {}
    case out of
      Success r -> do
        case r.before of
          Nothing -> pure unit
          _ -> fail "expected Nothing before"
        case r.after of
          Just (Right 42) -> pure unit
          _ -> fail "expected Just (Right 42) after"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "interrupting an awaiting fiber unsubscribes cleanly" do
    d <- liftEffect (D.make :: _ (D.Deferred () Int))
    let
      prog :: F.RIO () () Unit
      prog = do
        fib <- F.fork (D.await d :: F.RIO () () Int)
        F.sleep (Milliseconds 5.0)
        F.interrupt fib
        -- now complete after interrupt; should be a no-op for the
        -- consumer, and not crash the runtime
        _ <- D.succeed d 100
        pure unit
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
