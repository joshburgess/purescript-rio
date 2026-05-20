module Test.RIO.Aff.ResourceSpec (spec) where

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

import RIO.Aff.Core
  ( RIO
  , acquireRelease
  , addFinalizer
  , ask
  , bracket
  , die
  , ensuring
  , fail
  , onInterrupt
  , runRIO
  , sandbox
  , scoped
  )

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Resource (Phase 4.1)" do
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

      it "does NOT run release if acquisition itself fails (defect)" do
        -- `acquireRelease`'s docstring promises: "If `acquire`
        -- itself fails (typed or defect), `release` is not
        -- called, because there is nothing to release. The
        -- typed failure or defect propagates unchanged." The
        -- typed-failure half is pinned above. The defect half
        -- is unpinned: a refactor that wrapped acquire in
        -- `Aff.attempt` and forwarded the caught Error to
        -- release would silently invoke release on an
        -- uninitialised handle, but every other test would
        -- still pass. Pin the defect half by raising a defect
        -- inside `acquire` and observing that neither release
        -- nor use ever runs.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = acquireRelease
            (die (error "acquire-boom"))
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> pure 0)
        outcome <- attempt (runRIO program)
        order <- liftEffect (Ref.read events)
        case outcome of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
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

  describe "RIO.Aff.Resource (Phase 4.2)" do
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

      it "first-to-run (LIFO) finalizer throwing does not suppress the others" do
        -- Docstring promise: "each finalizer is allowed to
        -- throw; its exception is caught and does not stop
        -- subsequent finalizers from running." The pinned
        -- "does not let one finalizer's exception suppress
        -- the others" test only puts the throwing finalizer
        -- in the MIDDLE LIFO position (f2-boom, with f3 first
        -- and f1 last). The implementation folds with
        -- `foldr (\fin acc -> attempt fin *> acc) (pure unit)
        -- fins`, which wraps every finalizer in `attempt`.
        -- A subtle refactor that wrapped only the inner
        -- finalizers (e.g. `\fin acc -> fin *> attempt acc`)
        -- would still pass the middle-position test, because
        -- `f2`'s `throwError` lives inside the outer `attempt`
        -- — but it would let the LIFO-first finalizer's
        -- exception propagate out of the bracket release and
        -- silently skip every other finalizer. Pin the
        -- LIFO-first throwing case explicitly.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = scoped do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope (push "f1")
            _ <- addFinalizer scope (push "f2")
            _ <- addFinalizer scope
              (Aff.throwError (error "f3-boom"))
            liftAff (push "body")
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "body", "f2", "f1" ]

    describe "ensuring" do
      it "runs the finalizer after a successful action" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = ensuring
            (liftAff (push "use") *> pure 42)
            (liftAff (push "fin"))
        result <- runRIO program
        result `shouldEqual` (Right 42 :: Either _ Int)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use", "fin" ]

      it "runs the finalizer after a typed failure" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Int
          program = ensuring
            (liftAff (push "use") *> fail (Proxy :: Proxy "boom") unit)
            (liftAff (push "fin"))
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use", "fin" ]

      it "runs the finalizer after a defect" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () (Either _ Int)
          program = sandbox
            ( ensuring
                (liftAff (push "use") *> die (error "kaboom"))
                (liftAff (push "fin"))
            )
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use", "fin" ]

      it "runs the finalizer when the surrounding Aff fiber is killed" do
        -- Docstring promise: `ensuring`'s finalizer runs "on every
        -- termination path (success, typed failure, defect, or
        -- external fiber kill)". The fork/kill path is the fourth
        -- of those and the only one not already pinned.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = ensuring
            ( liftAff do
                push "use-start"
                delay (Milliseconds 1000.0)
                push "use-end"
            )
            (liftAff (push "fin"))
        fib <- Aff.forkAff (runRIO program)
        delay (Milliseconds 50.0)
        killFiber (error "test-kill") fib
        _ <- attempt (joinFiber fib)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use-start", "fin" ]

    describe "onInterrupt" do
      -- `onInterrupt` is the cancellation-specific counterpart to
      -- `ensuring`: it fires only when the action is killed by an
      -- external interrupt, not on success, typed failure, or
      -- defect. Pin all four paths.
      it "does not run the finalizer after a successful action" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = onInterrupt
            (liftAff (push "use") *> pure 42)
            (liftAff (push "fin"))
        result <- runRIO program
        result `shouldEqual` (Right 42 :: Either _ Int)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use" ]

      it "does not run the finalizer after a typed failure" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Int
          program = onInterrupt
            (liftAff (push "use") *> fail (Proxy :: Proxy "boom") unit)
            (liftAff (push "fin"))
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use" ]

      it "does not run the finalizer after a defect" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () (Either _ Int)
          program = sandbox
            ( onInterrupt
                (liftAff (push "use") *> die (error "kaboom"))
                (liftAff (push "fin"))
            )
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use" ]

      it "runs the finalizer when the surrounding Aff fiber is killed" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = onInterrupt
            ( liftAff do
                push "use-start"
                delay (Milliseconds 1000.0)
                push "use-end"
            )
            (liftAff (push "fin"))
        fib <- Aff.forkAff (runRIO program)
        delay (Milliseconds 50.0)
        killFiber (error "test-kill") fib
        _ <- attempt (joinFiber fib)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "use-start", "fin" ]

    describe "bracket" do
      it "runs release after a successful use" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = bracket
            (liftAff (push "acquire") *> pure 7)
            (\_ -> liftAff (push "release"))
            (\a -> liftAff (push "use") *> pure (a * 6))
        result <- runRIO program
        result `shouldEqual` Right 42
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "runs release after a typed failure in use, and surfaces the use error" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Int
          program = bracket
            (liftAff (push "acquire") *> pure 1)
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> fail (Proxy :: Proxy "boom") unit)
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "swallows a typed failure raised by the release path" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Int
          program = bracket
            (liftAff (push "acquire") *> pure 11)
            ( \_ -> do
                liftAff (push "release")
                fail (Proxy :: Proxy "boom") unit
            )
            (\a -> liftAff (push "use") *> pure (a + 1))
        -- The use value survives even though release "failed"
        result <- runRIO program
        result `shouldEqual` Right 12
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "runs release on defect in use" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = bracket
            (liftAff (push "acquire") *> pure 1)
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> die (error "kaboom"))
        _ <- attempt (runRIO program)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use", "release" ]

      it "does NOT run release if acquisition fails" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (acqFail :: Unit) Int
          program = bracket
            (fail (Proxy :: Proxy "acqFail") unit)
            (\_ -> liftAff (push "release"))
            (\_ -> liftAff (push "use") *> pure 0)
        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` []

      it "runs release when the surrounding Aff fiber is killed" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Int
          program = bracket
            (liftAff (push "acquire") *> pure 1)
            (\_ -> liftAff (push "release"))
            ( \_ -> do
                liftAff (push "use-start")
                liftAff (delay (Milliseconds 1000.0))
                liftAff (push "use-end")
                pure 0
            )
        fib <- Aff.forkAff (runRIO program)
        delay (Milliseconds 50.0)
        killFiber (error "test-kill") fib
        _ <- attempt (joinFiber fib)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "use-start", "release" ]
