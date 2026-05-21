module Test.RIO.Aff.Stream.TimedSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.Stream (Stream)
import RIO.Aff.Stream as Stream
import RIO.Aff.Stream.Timed (debounce, groupedWithin, throttle, timeoutPerPull)
import RIO.Aff.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Aff.Stream.Timed" do

  describe "throttle" do
    it "emits the first item immediately then sleeps between emits" do
      tc <- newTestClock
      received <- liftEffect (Ref.new [])
      let
        program :: RIO (clock :: Clock) () Unit
        program = Stream.runDrain
          ( Stream.mapM
              ( \i ->
                  liftEffect
                    (Ref.modify_ (\xs -> xs <> [ i ]) received) *>
                    pure unit
              )
              (throttle (Milliseconds 100.0) (Stream.fromArray [ 1, 2, 3 ]))
          )
      _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } program))
      -- first element arrives immediately
      liftAff (Aff.delay (Milliseconds 0.0))
      r1 <- liftEffect (Ref.read received)
      r1 `shouldEqual` [ 1 ]
      -- second arrives after one interval advance
      tc.advance (Milliseconds 100.0)
      liftAff (Aff.delay (Milliseconds 0.0))
      r2 <- liftEffect (Ref.read received)
      r2 `shouldEqual` [ 1, 2 ]
      -- third arrives after another interval advance
      tc.advance (Milliseconds 100.0)
      liftAff (Aff.delay (Milliseconds 0.0))
      r3 <- liftEffect (Ref.read received)
      r3 `shouldEqual` [ 1, 2, 3 ]

    it "passes through an empty stream as empty" do
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () (Array Int)
        program = Stream.runCollect
          (throttle (Milliseconds 100.0) Stream.empty)
      r <- runRIO' (provideAll { clock: tc.clock } program)
      r `shouldEqual` []

  describe "debounce" do
    it "emits a single trailing value after upstream silence" do
      -- Upstream yields one value then stays silent; the debounce
      -- timer fires after `interval` of quiet, and the buffered
      -- value is flushed.
      tc <- newTestClock
      received <- liftEffect (Ref.new [])
      let
        program :: RIO (clock :: Clock) () Unit
        program = Stream.runDrain
          ( Stream.mapM
              ( \i ->
                  liftEffect
                    (Ref.modify_ (\xs -> xs <> [ i ]) received) *>
                    pure unit
              )
              (debounce (Milliseconds 50.0) (Stream.fromArray [ 42 ]))
          )
      _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } program))
      -- Let the fork pull the single value and start its timer.
      liftAff (Aff.delay (Milliseconds 0.0))
      tc.advance (Milliseconds 50.0)
      liftAff (Aff.delay (Milliseconds 0.0))
      r <- liftEffect (Ref.read received)
      r `shouldEqual` [ 42 ]

  describe "groupedWithin" do
    it "flushes a partial group on upstream termination" do
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () (Array (Array Int))
        program = Stream.runCollect
          (groupedWithin 5 (Milliseconds 100.0) (Stream.fromArray [ 1, 2, 3 ]))
      r <- runRIO' (provideAll { clock: tc.clock } program)
      r `shouldEqual` [ [ 1, 2, 3 ] ]

    it "flushes once the chunk fills up to maxSize" do
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () (Array (Array Int))
        program = Stream.runCollect
          ( groupedWithin 2 (Milliseconds 1000.0)
              (Stream.fromArray [ 1, 2, 3, 4, 5 ])
          )
      r <- runRIO' (provideAll { clock: tc.clock } program)
      r `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]

    it "yields no groups for an empty upstream" do
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () (Array (Array Int))
        program = Stream.runCollect
          (groupedWithin 5 (Milliseconds 100.0) Stream.empty)
      r <- runRIO' (provideAll { clock: tc.clock } program)
      r `shouldEqual` []

  describe "timeoutPerPull" do
    it "wraps every successful pull as Just" do
      let
        program :: RIO () () (Array (Maybe Int))
        program = Stream.runCollect
          (timeoutPerPull (Milliseconds 50.0) (Stream.fromArray [ 1, 2, 3 ]))
      r <- runRIO' program
      r `shouldEqual` [ Just 1, Just 2, Just 3 ]

    it "emits Nothing then keeps pulling when a pull exceeds the deadline" do
      pulled <- liftEffect (Ref.new (0 :: Int))
      let
        -- A source that delays the first pull past the deadline,
        -- then yields one element on the second pull and stops.
        slowFirst :: Stream () () Int
        slowFirst = Stream.Stream do
          n <- liftEffect (Ref.modify (_ + 1) pulled)
          if n == 1 then do
            liftAff (Aff.delay (Milliseconds 50.0))
            pure (Stream.Yield 99 Stream.empty)
          else pure Stream.Done

        program :: RIO () () (Array (Maybe Int))
        program = Stream.runCollect
          (timeoutPerPull (Milliseconds 10.0) slowFirst)
      r <- runRIO' program
      -- The first pull stalls, races out, and the consumer sees a
      -- `Nothing`. The retry hits the same source's `n=2` arm, which
      -- terminates. So the visible output is [Nothing].
      r `shouldEqual` [ Nothing ]
