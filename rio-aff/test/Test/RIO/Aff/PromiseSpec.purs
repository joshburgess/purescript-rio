module Test.RIO.Aff.PromiseSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Promise (Promise, fromPromise, fromPromiseEffect)

foreign import _resolved :: forall a. a -> Effect (Promise a)
foreign import _rejected :: forall a. String -> Effect (Promise a)
foreign import _throwing :: forall a. String -> Effect (Promise a)

spec :: Spec Unit
spec = describe "RIO.Aff.Promise" do
  describe "fromPromise" do
    it "delivers a resolved Promise on the success channel" do
      p <- liftEffect (_resolved 42)
      n <- runRIO' (fromPromise p :: RIO () () Int)
      n `shouldEqual` 42

    it "reifies a rejected Promise as a defect" do
      p <- liftEffect (_rejected "kaboom" :: Effect (Promise Int))
      out <- attempt (runRIO' (fromPromise p :: RIO () () Int))
      case out of
        Left err -> message err `shouldEqual` "kaboom"
        Right _ -> fail "expected rejection to surface as a defect"

  describe "fromPromiseEffect" do
    it "runs the constructor each time the program is invoked" do
      let
        prog :: RIO () () Int
        prog = fromPromiseEffect (_resolved 7)
      n <- runRIO' prog
      n `shouldEqual` 7

    it "reflects a synchronous throw from the constructor as a defect" do
      let
        prog :: RIO () () Int
        prog = fromPromiseEffect (_throwing "sync-boom")
      out <- attempt (runRIO' prog)
      case out of
        Left err -> message err `shouldEqual` "sync-boom"
        Right _ -> fail "expected synchronous throw to surface as a defect"
