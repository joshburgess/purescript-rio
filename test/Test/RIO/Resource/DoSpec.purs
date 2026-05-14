module Test.RIO.Resource.DoSpec (spec) where

import Prelude

import Data.Array (snoc)
import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), attempt, delay, error, forkAff, joinFiber, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, runRIO, runRIO')
import RIO.Resource (acquireRelease)
import RIO.Resource.Do as Resource

spec :: Spec Unit
spec = do
  describe "RIO.Resource.Do" do
    describe "Resource.do" do
      it "matches nested acquireRelease event-for-event" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          flat :: RIO () () String
          flat = Resource.do
            h <- Resource.acquire (liftAff (push "open:h") *> Resource.pure "H")
              (\_ -> liftAff (push "close:h"))
            c <- Resource.acquire (liftAff (push "open:c") *> Resource.pure "C")
              (\_ -> liftAff (push "close:c"))
            _ <- Resource.liftRIO (liftAff (push ("use:" <> h <> "+" <> c)))
            Resource.pure (h <> "+" <> c)

          nested :: RIO () () String
          nested =
            acquireRelease (liftAff (push "open:h") *> pure "H") (\_ -> liftAff (push "close:h"))
              \h ->
                acquireRelease (liftAff (push "open:c") *> pure "C")
                  (\_ -> liftAff (push "close:c"))
                  \c -> do
                    liftAff (push ("use:" <> h <> "+" <> c))
                    pure (h <> "+" <> c)

        _ <- liftEffect (Ref.write [] events)
        flatResult <- runRIO flat
        flatEvents <- liftEffect (Ref.read events)

        _ <- liftEffect (Ref.write [] events)
        nestedResult <- runRIO nested
        nestedEvents <- liftEffect (Ref.read events)

        flatResult `shouldEqual` Right "H+C"
        nestedResult `shouldEqual` Right "H+C"
        flatEvents `shouldEqual`
          [ "open:h", "open:c", "use:H+C", "close:c", "close:h" ]
        flatEvents `shouldEqual` nestedEvents

      it "releases in LIFO order on typed failure in the body" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (boom :: Unit) Unit
          program = Resource.do
            _ <- Resource.acquire (liftAff (push "open:a") *> Resource.pure unit)
              (\_ -> liftAff (push "close:a"))
            _ <- Resource.acquire (liftAff (push "open:b") *> Resource.pure unit)
              (\_ -> liftAff (push "close:b"))
            fail (Proxy :: Proxy "boom") unit

        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "open:a", "open:b", "close:b", "close:a" ]

      it "releases in LIFO order on a defect in the body" do
        -- Module docstring promises that releases run on "every
        -- termination path (success, typed failure, defect, kill)"
        -- in LIFO order. The typed-failure case is pinned above;
        -- pin the defect path on the qualified-do surface so the
        -- whole contract is documented through Resource.do (not
        -- only through the lower-level acquireRelease).
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = Resource.do
            _ <- Resource.acquire (liftAff (push "open:a") *> Resource.pure unit)
              (\_ -> liftAff (push "close:a"))
            _ <- Resource.acquire (liftAff (push "open:b") *> Resource.pure unit)
              (\_ -> liftAff (push "close:b"))
            die (error "kaboom")

        _ <- attempt (runRIO' program)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "open:a", "open:b", "close:b", "close:a" ]

      it "releases in LIFO order when the surrounding Aff fiber is killed" do
        -- Pins the kill termination path through Resource.do. The
        -- delay parks the body long enough for the test to land a
        -- killFiber; both releases must still run, in LIFO order,
        -- before the fiber dies.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () () Unit
          program = Resource.do
            _ <- Resource.acquire (liftAff (push "open:a") *> Resource.pure unit)
              (\_ -> liftAff (push "close:a"))
            _ <- Resource.acquire (liftAff (push "open:b") *> Resource.pure unit)
              (\_ -> liftAff (push "close:b"))
            liftAff do
              push "body-start"
              delay (Milliseconds 1000.0)
              push "body-end"

        fib <- forkAff (runRIO' program)
        delay (Milliseconds 50.0)
        killFiber (error "test-kill") fib
        _ <- attempt (joinFiber fib)
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "open:a", "open:b", "body-start", "close:b", "close:a" ]

      it "releases acquired resources even if a later acquire fails" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          program :: RIO () (acqFail :: Unit) Unit
          program = Resource.do
            _ <- Resource.acquire (liftAff (push "open:a") *> Resource.pure unit)
              (\_ -> liftAff (push "close:a"))
            _ <- Resource.acquire
              (fail (Proxy :: Proxy "acqFail") unit)
              (\_ -> liftAff (push "close:b"))
            Resource.pure unit

        _ <- runRIO program
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "open:a", "close:a" ]
