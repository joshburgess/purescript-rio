module Test.RIO.Fiber.PromiseSpec (spec) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import RIO.Fiber.Aff (runAff)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Promise (Promise, fromPromise, fromPromiseEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

foreign import _resolved :: forall a. a -> Effect (Promise a)
foreign import _rejected :: forall a. String -> Effect (Promise a)
foreign import _throwing :: forall a. String -> Effect (Promise a)

spec :: Spec Unit
spec = describe "rio-fiber: Promise bridge" do
  describe "fromPromise" do
    it "delivers a resolved Promise on the success channel" do
      p <- liftEffect (_resolved 42)
      out <- runAff (fromPromise p :: F.RIO () () Int) {}
      case out of
        Success n -> n `shouldEqual` 42
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "reifies a rejected Promise as a Die defect" do
      p <- liftEffect (_rejected "kaboom" :: Effect (Promise Int))
      out <- runAff (fromPromise p :: F.RIO () () Int) {}
      case out of
        Die e -> message e `shouldEqual` "kaboom"
        other -> fail ("expected Die, got " <> describeOutcome other)

  describe "fromPromiseEffect" do
    it "runs the constructor each time the program is invoked" do
      let
        prog :: F.RIO () () Int
        prog = fromPromiseEffect (_resolved 7)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 7
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "reflects a synchronous throw from the constructor as a Die defect" do
      let
        prog :: F.RIO () () Int
        prog = fromPromiseEffect (_throwing "sync-boom")
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "sync-boom"
        other -> fail ("expected Die, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
