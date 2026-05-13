module Test.RIO.ScheduleSpec (spec) where

import Prelude

import Data.Array (all, snoc) as Array
import Data.Either (Either(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Aff (delay, forkAff) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))

import RIO.Clock (Clock)
import RIO.Core (RIO, fail, provideAll, runRIO, runRIO')
import RIO.Schedule
  ( Schedule
  , Step(..)
  , exponential
  , intersect
  , jittered
  , recurs
  , repeat
  , retry
  , retryOrElse
  , spaced
  , step
  )
import RIO.Test.Clock (newTestClock)

spec :: Spec Unit
spec = do
  describe "RIO.Schedule" do
    describe "repeat" do
      it "recurs n runs the action n+1 times" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program = repeat (recurs 3) action

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        result `shouldEqual` 4
        count `shouldEqual` 4

      it "stops on the first typed failure" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            n <- liftEffect (Ref.modify (_ + 1) counter)
            if n >= 2 then fail (Proxy :: Proxy "boom") unit
            else pure n

          program :: RIO (clock :: Clock) (boom :: Unit) Int
          program = repeat (recurs 5) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        count `shouldEqual` 2

    describe "retry" do
      it "recovers after a transient failure" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            n <- liftEffect (Ref.modify (_ + 1) counter)
            if n < 3 then fail (Proxy :: Proxy "boom") unit
            else pure n

          program :: RIO (clock :: Clock) (boom :: Unit) Int
          program = retry (recurs 5) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        result `shouldEqual` (Right 3 :: Either _ Int)
        count `shouldEqual` 3

      it "surfaces the final failure once retries are exhausted" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            _ <- liftEffect (Ref.modify (_ + 1) counter)
            fail (Proxy :: Proxy "boom") unit

          program :: RIO (clock :: Clock) (boom :: Unit) Int
          program = retry (recurs 2) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        count `shouldEqual` 3

    describe "retryOrElse" do
      it "runs the fallback when retries are exhausted" do
        let
          action :: RIO () (boom :: Unit) Int
          action = fail (Proxy :: Proxy "boom") unit

          fallback :: Variant.Variant (boom :: Unit) -> RIO () () Int
          fallback _ = pure 99

          program :: RIO (clock :: Clock) () Int
          program = retryOrElse (recurs 1) action fallback

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        result `shouldEqual` 99

    describe "intersect" do
      it "stops as soon as either schedule stops" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program =
            repeat
              (intersect (recurs 2) (recurs 10))
              action

        tc <- newTestClock
        _ <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        count `shouldEqual` 3

    describe "exponential under the test clock" do
      it "drives one step per matching advance" do
        counter <- liftEffect (Ref.new 0)
        tc <- newTestClock
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program =
            repeat
              (intersect (recurs 3) (exponential (Milliseconds 100.0) 2.0))
              action

        _ <- Aff.forkAff
          (runRIO' (provideAll { clock: tc.clock } program))

        Aff.delay (Milliseconds 0.0)
        c0 <- liftEffect (Ref.read counter)
        c0 `shouldEqual` 1

        tc.advance (Milliseconds 100.0)
        Aff.delay (Milliseconds 0.0)
        c1 <- liftEffect (Ref.read counter)
        c1 `shouldEqual` 2

        tc.advance (Milliseconds 200.0)
        Aff.delay (Milliseconds 0.0)
        c2 <- liftEffect (Ref.read counter)
        c2 `shouldEqual` 3

        tc.advance (Milliseconds 400.0)
        Aff.delay (Milliseconds 0.0)
        c3 <- liftEffect (Ref.read counter)
        c3 `shouldEqual` 4

    describe "jittered" do
      it "keeps sampled delays inside the band [lo*base, hi*base]" do
        delays <- runRIO' (collectDelays 100 (jittered 0.8 1.2 (spaced (Milliseconds 100.0))))
        let
          inBand ms =
            let
              n = un Milliseconds ms
            in
              n >= 80.0 && n <= 120.0
        delays `shouldSatisfy` Array.all inBand

collectDelays
  :: forall o
   . Int
  -> Schedule () Unit o
  -> RIO () () (Array Milliseconds)
collectDelays n0 sched0 = go n0 sched0 []
  where
  go k s acc
    | k <= 0 = pure acc
    | otherwise = do
        out <- step s unit
        case out of
          Done -> pure acc
          Continue _ ms next -> go (k - 1) next (Array.snoc acc ms)
