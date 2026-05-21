module Test.RIO.Fiber.Stream.AsyncIterableSpec (spec) where

import Prelude

import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Scope as Scope
import RIO.Fiber.Stream (Stream)
import RIO.Fiber.Stream as S
import RIO.Fiber.Stream.AsyncIterable (AsyncIterable)
import RIO.Fiber.Stream.AsyncIterable as AI
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

foreign import _asyncIterableOf :: forall a. Array a -> Effect (AsyncIterable a)
foreign import _asyncIterableRejecting
  :: forall a. Array a -> String -> Effect (AsyncIterable a)

type Boom = (boom :: String)

absurdErr :: forall e. Variant e -> Error
absurdErr _ = error "unreachable: stream produced no typed failure"

spec :: Spec Unit
spec = describe "rio-fiber: Stream AsyncIterable bridge" do
  describe "fromAsyncIterable" do
    it "collects every value from the iterable" do
      iter <- liftEffect (_asyncIterableOf [ 1, 2, 3 ])
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (AI.fromAsyncIterable iter)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "ends the stream on a clean exhaustion" do
      iter <- liftEffect (_asyncIterableOf ([] :: Array Int))
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (AI.fromAsyncIterable iter)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "reifies a Promise rejection as a Die defect" do
      iter <- liftEffect (_asyncIterableRejecting [ 1, 2 ] "kaboom")
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runCollect (AI.fromAsyncIterable iter)
      out <- runAff prog {}
      case out of
        Die err -> message err `shouldEqual` "kaboom"
        other -> fail ("expected Die, got " <> describeOutcome other)

  describe "toAsyncIterable (round-trip via fromAsyncIterable)" do
    it "delivers every emitted value to the JS-facing iterable" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          iter <- AI.toAsyncIterable scope absurdErr
            (S.fromArray [ 1, 2, 3, 4, 5 ])
          S.runCollect (AI.fromAsyncIterable iter)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "ends the iterable when the source stream halts" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Scope.scoped \scope -> do
          iter <- AI.toAsyncIterable scope absurdErr
            (S.fromArray ([] :: Array Int))
          S.runCollect (AI.fromAsyncIterable iter)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "projects a typed failure through errToJs as a JS rejection" do
      let
        failingStream :: Stream () Boom Int
        failingStream =
          S.flatMap
            ( \n ->
                if n == 2 then
                  S.fromRIO
                    (F.fail (Variant.inj (Proxy :: Proxy "boom") "nope"))
                else S.emit n
            )
            (S.fromArray [ 1, 2, 3 ])

        proj :: Variant Boom -> Error
        proj v = Variant.match { boom: \s -> error ("projected:" <> s) } v

        prog :: F.RIO () Boom (Array Int)
        prog = Scope.scoped \scope -> do
          iter <- AI.toAsyncIterable scope proj failingStream
          S.runCollect (AI.fromAsyncIterable iter)

      out <- runAff prog {}
      case out of
        Die err -> message err `shouldEqual` "projected:nope"
        other -> fail ("expected Die, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
