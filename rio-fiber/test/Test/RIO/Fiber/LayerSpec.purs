module Test.RIO.Fiber.LayerSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Layer as Layer
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Layer" do
  it "fromValue + provide makes the record available" do
    let
      makeGreeting :: Layer.Layer () () (greeting :: String)
      makeGreeting = Layer.fromValue { greeting: "hello" }

      prog :: F.RIO () () String
      prog = Layer.provide makeGreeting do
        env <- F.ask
        pure env.greeting
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` "hello"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chainLayer threads the first output into the second" do
    let
      step1 :: Layer.Layer () () (n :: Int)
      step1 = Layer.fromValue { n: 21 }

      step2 :: Layer.Layer () (n :: Int) (m :: Int)
      step2 = Layer.fromRIO do
        env <- F.ask
        pure { m: env.n * 2 }

      combined :: Layer.Layer () () (m :: Int)
      combined = Layer.chainLayer step1 step2

      prog :: F.RIO () () Int
      prog = Layer.provide combined do
        env <- F.ask
        pure env.m
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mergeLayers builds both records from the same input" do
    let
      a :: Layer.Layer () () (x :: Int)
      a = Layer.fromValue { x: 1 }

      b :: Layer.Layer () () (y :: String)
      b = Layer.fromValue { y: "hi" }

      merged :: Layer.Layer () () (x :: Int, y :: String)
      merged = Layer.mergeLayers a b

      prog :: F.RIO () () { x :: Int, y :: String }
      prog = Layer.provide merged F.ask
    out <- runAff prog {}
    case out of
      Success r -> do
        r.x `shouldEqual` 1
        r.y `shouldEqual` "hi"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "provideScoped runs the layer's finalizers on exit" do
    ref <- liftEffect (Ref.new false)
    let
      buildResource
        :: Scope.Scope -> F.RIO () () (Record (n :: Int))
      buildResource scope = do
        n <- Scope.acquireRelease scope
          (pure 7)
          (\_ -> F.liftEffect (Ref.write true ref))
        pure { n }

      prog :: F.RIO () () Int
      prog = Layer.provideScoped buildResource do
        env <- F.ask
        pure env.n
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)
    -- the scoped finalizer fires fire-and-forget; wait for it
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    fired <- liftEffect (Ref.read ref)
    fired `shouldEqual` true

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
