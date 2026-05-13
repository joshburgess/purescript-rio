module Test.RIO.ConcurrencySpec (spec) where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), attempt, delay, error, message)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core
  ( RIO
  , addFinalizer
  , ask
  , die
  , fail
  , fork
  , interrupt
  , join
  , runRIO
  , sandbox
  , scoped
  )

spec :: Spec Unit
spec = do
  describe "RIO.Concurrency (Phase 6.1)" do
    describe "fork / join round trip" do
      it "joins a successful fiber and surfaces its value" do
        result <- runRIO do
          fib <- fork (pure 42 :: RIO () () Int)
          join fib
        result `shouldEqual` (Right 42 :: Either _ Int)

      it "lets the parent do other work while the child runs" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Int
          child = do
            liftAff (delay (Milliseconds 5.0))
            liftAff (push "child-done")
            pure 7

          parent :: RIO () () Int
          parent = do
            fib <- fork child
            liftAff (push "parent-mid")
            join fib

        result <- runRIO parent
        result `shouldEqual` (Right 7 :: Either _ Int)
        order <- liftEffect (Ref.read events)
        -- The parent's "parent-mid" must be observed before the
        -- child's "child-done" because of the 5ms delay.
        order `shouldEqual` [ "parent-mid", "child-done" ]

    describe "join surfaces typed failures" do
      it "returns Left on the joiner's row" do
        let
          child :: RIO () (boom :: Unit) Int
          child = fail (Proxy :: Proxy "boom") unit

          parent :: RIO () (boom :: Unit) Int
          parent = do
            fib <- fork child
            join fib
        result <- runRIO parent
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "join surfaces defects via sandbox" do
      it "a die'd fiber surfaces as Aff exception at the join" do
        let
          child :: RIO () () Int
          child = die (error "kaboom")

          parent :: RIO () () (Either _ Int)
          parent = sandbox do
            fib <- fork child
            join fib
        result <- runRIO parent
        case result of
          Right (Left e) -> message e `shouldEqual` "kaboom"
          _ -> 1 `shouldEqual` 0

    describe "interrupt" do
      it "cancels an in-flight Aff.delay and resources are released" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Unit
          child = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "acquired")
            _ <- addFinalizer scope (push "released")
            liftAff (delay (Milliseconds 5000.0))
            liftAff (push "should-not-happen")

          parent :: RIO () () Unit
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 10.0))
            interrupt fib
            -- Join after interrupt: the kill exception propagates as
            -- a defect; sandbox it so the test keeps running.
            _ <- sandbox (join fib)
            pure unit

        result <- runRIO parent
        result `shouldEqual` (Right unit :: Either _ Unit)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquired", "released" ]

      it "is a no-op on an already-completed fiber" do
        let
          child :: RIO () () Int
          child = pure 99

          parent :: RIO () () Int
          parent = do
            fib <- fork child
            n <- join fib
            interrupt fib
            pure n
        result <- runRIO parent
        result `shouldEqual` (Right 99 :: Either _ Int)

    describe "join surfaces interrupt as a defect" do
      it "joining an interrupted fiber throws inside Aff" do
        let
          child :: RIO () () Int
          child = do
            liftAff (delay (Milliseconds 5000.0))
            pure 1

          parent :: RIO () () (Either _ Int)
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 5.0))
            interrupt fib
            sandbox (join fib)
        result <- runRIO parent
        case result of
          Right (Left _) -> pure unit
          _ -> 1 `shouldEqual` 0

    describe "joining twice returns the same result" do
      it "is safe to join an already-joined fiber" do
        let
          child :: RIO () () Int
          child = pure 11

          parent :: RIO () () Int
          parent = do
            fib <- fork child
            n1 <- join fib
            n2 <- join fib
            pure (n1 + n2)
        result <- runRIO parent
        result `shouldEqual` (Right 22 :: Either _ Int)

    describe "parent kill does not finalize child's pre-interrupt state" do
      it "interrupting between acquire and delay releases the resource" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          child :: RIO () () Unit
          child = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "child:acquired")
            _ <- addFinalizer scope (push "child:released")
            liftAff (delay (Milliseconds 500.0))

          parent :: RIO () () Unit
          parent = do
            fib <- fork child
            liftAff (delay (Milliseconds 10.0))
            interrupt fib
            _ <- sandbox (join fib)
            liftAff (push "parent:after-join")

        result <- runRIO parent
        result `shouldEqual` (Right unit :: Either _ Unit)
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "child:acquired", "child:released", "parent:after-join" ]
