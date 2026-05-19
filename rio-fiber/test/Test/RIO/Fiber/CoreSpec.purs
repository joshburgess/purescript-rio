module Test.RIO.Fiber.CoreSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core as F
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Core" do
  it "pure returns its argument" do
    res <- liftEffect (F.runRIO' (pure 42 :: F.RIO () () Int))
    res `shouldEqual` 42

  it "map composes through bind" do
    let
      prog :: F.RIO () () Int
      prog = map (_ + 1) (pure 41)
    res <- liftEffect (F.runRIO' prog)
    res `shouldEqual` 42

  it "bind threads results in order" do
    let
      prog :: F.RIO () () Int
      prog = do
        a <- pure 10
        b <- pure 20
        pure (a + b)
    res <- liftEffect (F.runRIO' prog)
    res `shouldEqual` 30

  it "liftEffect runs synchronous effects" do
    ref <- liftEffect (Ref.new 0)
    let
      prog :: F.RIO () () Int
      prog = do
        _ <- F.liftEffect (Ref.write 7 ref)
        F.liftEffect (Ref.read ref)
    res <- liftEffect (F.runRIO' prog)
    res `shouldEqual` 7

  it "fail surfaces a typed failure on Left" do
    let
      prog :: F.RIO () (boom :: String) Int
      prog = F.fail (Variant.inj (Proxy :: _ "boom") "kaboom")
    res <- liftEffect (F.runRIO prog)
    case res of
      Right _ -> fail "expected typed failure, got success"
      Left v ->
        (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
          `shouldEqual` "kaboom"

  it "catchAll recovers from a typed failure" do
    let
      raised :: F.RIO () (boom :: String) Int
      raised = F.fail (Variant.inj (Proxy :: _ "boom") "nope")

      recovered :: F.RIO () () Int
      recovered = F.catchAll
        ( \v ->
            (Variant.case_ # Variant.on (Proxy :: _ "boom") (\_ -> pure 99)) v
        )
        raised
    res <- liftEffect (F.runRIO' recovered)
    res `shouldEqual` 99

  it "ask returns the environment record" do
    let
      prog :: F.RIO (greet :: String) () String
      prog = F.asks _.greet
    res <- liftEffect (F.runFiber prog { greet: "hello" })
    case res of
      Right s -> s `shouldEqual` "hello"
      Left _ -> fail "expected success, got failure"
