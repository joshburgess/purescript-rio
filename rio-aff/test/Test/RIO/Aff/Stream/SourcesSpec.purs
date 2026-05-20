module Test.RIO.Aff.Stream.SourcesSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, forkAff) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, provideAll, runRIO, runRIO')
import RIO.Aff.Hub as Hub
import RIO.Aff.Queue as Queue
import RIO.Aff.Stream
  ( Stream
  , fromHub
  , fromQueue
  , iterate
  , iterateM
  , range
  , repeat
  , runCollect
  , take
  , tick
  )
import RIO.Aff.Stream as Stream
import RIO.Aff.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Aff.Stream sources" do

  describe "range" do
    it "yields integers from start to end inclusive" do
      let
        program :: RIO () () (Array Int)
        program = runCollect (range 1 5)
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 3, 4, 5 ] :: Either _ _)

    it "is empty when end < start" do
      let
        program :: RIO () () (Array Int)
        program = runCollect (range 5 1)
      result <- runRIO program
      result `shouldEqual` (Right [] :: Either _ _)

    it "yields a single element when start == end" do
      let
        program :: RIO () () (Array Int)
        program = runCollect (range 3 3)
      result <- runRIO program
      result `shouldEqual` (Right [ 3 ] :: Either _ _)

  describe "iterate / iterateM" do
    it "iterate emits seed, f seed, f (f seed), ..." do
      let
        program :: RIO () () (Array Int)
        program = runCollect (take 5 (iterate 1 (_ * 2)))
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 4, 8, 16 ] :: Either _ _)

    it "iterateM runs the effectful step between yields" do
      counter <- liftEffect (Ref.new 0)
      let
        step :: Int -> RIO () () Int
        step n = do
          _ <- liftEffect (Ref.modify (_ + 1) counter)
          pure (n + 1)

        program :: RIO () () (Array Int)
        program = runCollect (take 4 (iterateM 10 step))
      result <- runRIO program
      result `shouldEqual` (Right [ 10, 11, 12, 13 ] :: Either _ _)
      -- step runs once per pulled element to compute the next seed.
      steps <- liftEffect (Ref.read counter)
      steps `shouldEqual` 4

  describe "repeat" do
    it "produces the same value forever (take 3 = three copies)" do
      let
        program :: RIO () () (Array String)
        program = runCollect (take 3 (repeat "x"))
      result <- runRIO program
      result `shouldEqual` (Right [ "x", "x", "x" ] :: Either _ _)

  describe "fromQueue" do
    it "yields each item offered and terminates on shutdown" do
      q <- liftEffect Queue.unbounded
      _ <- runRIO' do
        _ <- Queue.offer q 1
        _ <- Queue.offer q 2
        _ <- Queue.offer q 3
        Queue.shutdown q
      let
        program :: RIO () () (Array Int)
        program = runCollect (fromQueue q)
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 3 ] :: Either _ _)

    it "blocks the stream until items arrive, then continues" do
      q <- liftEffect Queue.unbounded
      -- arrange: a fork drips items in, eventually shuts down
      _ <- Aff.forkAff
        ( runRIO' do
            _ <- Queue.offer q 10
            _ <- Queue.offer q 20
            Queue.shutdown q
        )
      let
        program :: RIO () () (Array Int)
        program = runCollect (fromQueue q)
      result <- runRIO program
      result `shouldEqual` (Right [ 10, 20 ] :: Either _ _)

  describe "fromHub" do
    it "subscribes lazily and yields each subsequent publish" do
      h <- liftEffect Hub.make
      -- arrange: fork waits for the subscriber to land before
      -- publishing. `fromHub` subscribes lazily on the first pull,
      -- so the publishes have to happen *after* that. The fork
      -- yields once and then publishes; the main fiber subscribes
      -- and then awaits.
      _ <- Aff.forkAff
        ( do
            Aff.delay (Milliseconds 5.0)
            runRIO' do
              _ <- Hub.publish h 100
              _ <- Hub.publish h 200
              _ <- Hub.publish h 300
              pure unit
        )
      let
        program :: RIO () () (Array Int)
        program = runCollect (take 3 (fromHub h))
      result <- runRIO program
      result `shouldEqual` (Right [ 100, 200, 300 ] :: Either _ _)

  describe "tick" do
    it "emits a unit on the test clock at each advance" do
      tc <- newTestClock
      events <- liftEffect (Ref.new 0)
      let
        program :: RIO (clock :: Clock) () Unit
        program = Stream.runDrain
          ( Stream.mapM
              ( \_ -> liftEffect (Ref.modify_ (_ + 1) events) *> pure unit
              )
              (take 3 (tick (Milliseconds 50.0)))
          )
      _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } program))
      -- Let the fork park on its first sleep before advancing.
      liftAff (Aff.delay (Milliseconds 0.0))
      tc.advance (Milliseconds 50.0)
      liftAff (Aff.delay (Milliseconds 0.0))
      tc.advance (Milliseconds 50.0)
      liftAff (Aff.delay (Milliseconds 0.0))
      tc.advance (Milliseconds 50.0)
      liftAff (Aff.delay (Milliseconds 0.0))
      count <- liftEffect (Ref.read events)
      count `shouldEqual` 3
