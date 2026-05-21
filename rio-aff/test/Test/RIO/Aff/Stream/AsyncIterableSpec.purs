module Test.RIO.Aff.Stream.AsyncIterableSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (attempt, error, message)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, fail, runRIO, runRIO')
import RIO.Aff.Resource (scoped)
import RIO.Aff.Stream (Stream)
import RIO.Aff.Stream as S
import RIO.Aff.Stream.AsyncIterable (AsyncIterable)
import RIO.Aff.Stream.AsyncIterable as AI

foreign import _asyncIterableOf :: forall a. Array a -> Effect (AsyncIterable a)
foreign import _asyncIterableRejecting
  :: forall a. Array a -> String -> Effect (AsyncIterable a)

type Boom = (boom :: String)

absurdErr :: forall e. Variant e -> Error
absurdErr _ = error "unreachable: stream produced no typed failure"

spec :: Spec Unit
spec = describe "RIO.Aff.Stream.AsyncIterable" do
  describe "fromAsyncIterable" do
    it "collects every value from the iterable" do
      iter <- liftEffect (_asyncIterableOf [ 1, 2, 3 ])
      let
        prog :: RIO () () (Array Int)
        prog = S.runCollect (AI.fromAsyncIterable iter)
      result <- runRIO prog
      result `shouldEqual` (Right [ 1, 2, 3 ] :: Either _ (Array Int))

    it "ends the stream on a clean exhaustion" do
      iter <- liftEffect (_asyncIterableOf ([] :: Array Int))
      let
        prog :: RIO () () (Array Int)
        prog = S.runCollect (AI.fromAsyncIterable iter)
      result <- runRIO prog
      result `shouldEqual` (Right ([] :: Array Int) :: Either _ (Array Int))

    it "reifies a Promise rejection as a defect" do
      iter <- liftEffect (_asyncIterableRejecting [ 1, 2 ] "kaboom")
      let
        prog :: RIO () () (Array Int)
        prog = S.runCollect (AI.fromAsyncIterable iter)
      r <- attempt (runRIO' prog)
      case r of
        Left err -> message err `shouldEqual` "kaboom"
        Right _ -> Spec.fail "expected rejection to surface as a defect"

  describe "toAsyncIterable (round-trip via fromAsyncIterable)" do
    it "delivers every emitted value to the JS-facing iterable" do
      let
        prog :: RIO () () (Array Int)
        prog = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          iter <- AI.toAsyncIterable scope absurdErr
            (S.fromArray [ 1, 2, 3, 4, 5 ])
          S.runCollect (AI.fromAsyncIterable iter)
      result <- runRIO prog
      result `shouldEqual`
        (Right [ 1, 2, 3, 4, 5 ] :: Either _ (Array Int))

    it "ends the iterable when the source stream halts" do
      let
        prog :: RIO () () (Array Int)
        prog = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          iter <- AI.toAsyncIterable scope absurdErr
            (S.fromArray ([] :: Array Int))
          S.runCollect (AI.fromAsyncIterable iter)
      result <- runRIO prog
      result `shouldEqual` (Right ([] :: Array Int) :: Either _ (Array Int))

    it "projects a typed failure through errToJs as a JS rejection" do
      let
        failingStream :: forall r. Stream r Boom Int
        failingStream =
          S.flatMap
            (S.fromArray [ 1, 2, 3 ])
            ( \n ->
                if n == 2 then
                  S.Stream (fail (Proxy :: Proxy "boom") "nope")
                else S.single n
            )

        proj :: Variant Boom -> Error
        proj v =
          Variant.match { boom: \s -> error ("projected:" <> s) } v

        prog :: RIO () Boom (Array Int)
        prog = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          iter <- AI.toAsyncIterable scope proj failingStream
          S.runCollect (AI.fromAsyncIterable iter)

      -- The typed failure is projected to an Error on the JS side
      -- and surfaces as a Promise rejection, which the
      -- `fromAsyncIterable` consumer reifies as an Aff defect. The
      -- typed-error row therefore stays clean; the defect is what
      -- escapes the `runRIO`. We catch it with `attempt` on the
      -- underlying `Aff`.
      r <- attempt (runRIO prog)
      case r of
        Left err -> message err `shouldEqual` "projected:nope"
        Right _ -> Spec.fail "expected typed failure to surface as a defect"
