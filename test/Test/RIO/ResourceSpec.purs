module Test.RIO.ResourceSpec (spec) where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), attempt, delay, error, joinFiber, killFiber)
import Effect.Aff (forkAff, throwError) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core
  ( RIO
  , acquireRelease
  , addFinalizer
  , ask
  , die
  , fail
  , runRIO
  , scoped
  )

spec :: Spec Unit
spec = do
  describe "RIO.Resource (Phase 4.1)" do
    describe "acquireRelease" do
      it "runs release after a successful use" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = acquireRelease
            (liftAff (push "acquire") *> pure 42)
            (\_ -> liftAff (push "release"))
            (\a -> liftAff (push "use") *> pure (a + 1))
        result <- runRIO program
        result `shouldEqual` Right 43
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "runs release after a typed failure in use" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Int
          program = acquireRelease
            (liftAff (push "acquire") *> pure 1)
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> fail (Proxy :: Proxy "boom") unit)
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "runs release after a defect in use" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = acquireRelease
            (liftAff (push "acquire") *> pure 1)
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> die (error "boom"))
        _ <- attempt (runRIO program)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "does NOT run release if acquisition itself fails (typed)" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (acqFail :: Unit) Int
          program = acquireRelease
            (fail (Proxy :: Proxy "acqFail") unit)
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> pure 0)
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` []

      it "runs release when the surrounding Aff fiber is killed mid-use" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = acquireRelease
            (liftAff (push "acquire"))
            (\_ -> liftAff (push "release"))
            ( \_ -> liftAff do
                push "use-start"
                delay (Milliseconds 1000.0)
                push "use-end" -- should not happen; fiber will be killed
            )
        fib <- Aff.forkAff (runRIO program)
        -- Give the fiber enough time to acquire and start its sleep.
        delay (Milliseconds 50.0)
        killFiber (error "test-kill") fib
        _ <- attempt (joinFiber fib)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use-start", "release" ]

  describe "RIO.Resource (Phase 4.2)" do
    describe "scoped" do
      it "runs all registered finalizers in LIFO order on success" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope (push "f1")
            _ <- addFinalizer scope (push "f2")
            _ <- addFinalizer scope (push "f3")
            liftAff (push "body")
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "body", "f3", "f2", "f1" ]

      it "runs finalizers in LIFO order on typed failure" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope (push "f1")
            _ <- addFinalizer scope (push "f2")
            liftAff (push "body")
            fail (Proxy :: Proxy "boom") unit
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "body", "f2", "f1" ]

      it "runs finalizers in LIFO order on defect" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope (push "f1")
            _ <- addFinalizer scope (push "f2")
            liftAff (push "body")
            die (error "boom")
        _ <- attempt (runRIO program)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "body", "f2", "f1" ]

      it "runs finalizers in LIFO order when the surrounding Aff fiber is killed" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope (push "f1")
            _ <- addFinalizer scope (push "f2")
            liftAff do
              push "body-start"
              delay (Milliseconds 1000.0)
              push "body-end"
        fib <- Aff.forkAff (runRIO program)
        delay (Milliseconds 50.0)
        killFiber (error "test-kill") fib
        _ <- attempt (joinFiber fib)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "body-start", "f2", "f1" ]

      it "does not let one finalizer's exception suppress the others" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope (push "f1")
            _ <- addFinalizer scope (Aff.throwError (error "f2-boom"))
            _ <- addFinalizer scope (push "f3")
            liftAff (push "body")
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "body", "f3", "f1" ]
