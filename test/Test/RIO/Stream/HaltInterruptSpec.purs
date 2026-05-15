module Test.RIO.Stream.HaltInterruptSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Concurrency (fork)
import RIO.Core (RIO, runRIO, runRIO')
import RIO.Deferred (makeDeferred, succeedDeferred, failDeferred)
import RIO.Stream as Stream

type Errs = (stopped :: String)

mkStopped :: String -> Variant Errs
mkStopped = Variant.inj (Proxy :: Proxy "stopped")

spec :: Spec Unit
spec = describe "RIO.Stream (haltWhen / interruptWhen)" do

  describe "haltWhen" do
    it "passes every element through when the sentinel never fires" do
      result <- runRIO' do
        sentinel <- makeDeferred
        Stream.runCollect
          (Stream.haltWhen sentinel (Stream.fromArray [ 1, 2, 3 ]))
      result `shouldEqual` [ 1, 2, 3 ]

    it "halts at the next pull when the sentinel has already fired" do
      result <- runRIO' do
        sentinel <- makeDeferred
        _ <- succeedDeferred sentinel unit
        Stream.runCollect
          (Stream.haltWhen sentinel (Stream.fromArray [ 1, 2, 3 ]))
      result `shouldEqual` []

    it "raises the failure when the sentinel fails" do
      let
        program :: RIO () Errs (Array Int)
        program = do
          sentinel <- makeDeferred
          _ <- failDeferred sentinel (mkStopped "boom")
          Stream.runCollect
            (Stream.haltWhen sentinel (Stream.fromArray [ 1, 2, 3 ]))
      result <- runRIO program
      result `shouldEqual` (Left (mkStopped "boom") :: Either _ _)

    it "halts between pulls once the sentinel has been fired" do
      -- fire the sentinel after one element has been emitted; the
      -- next pull observes the halt and the stream ends.
      result <- runRIO' do
        sentinel <- makeDeferred
        let
          tapped :: Stream.Stream () () Int
          tapped = Stream.tap
            (\n -> if n == 1 then succeedDeferred sentinel unit *> pure unit else pure unit)
            (Stream.fromArray [ 1, 2, 3 ])
        Stream.runCollect (Stream.haltWhen sentinel tapped)
      result `shouldEqual` [ 1 ]

  describe "interruptWhen" do
    it "passes every element through when the sentinel never fires" do
      result <- runRIO' do
        sentinel <- makeDeferred
        Stream.runCollect
          (Stream.interruptWhen sentinel (Stream.fromArray [ 10, 20, 30 ]))
      result `shouldEqual` [ 10, 20, 30 ]

    it "halts immediately when the sentinel has already fired" do
      result <- runRIO' do
        sentinel <- makeDeferred
        _ <- succeedDeferred sentinel unit
        Stream.runCollect
          (Stream.interruptWhen sentinel (Stream.fromArray [ 10, 20, 30 ]))
      result `shouldEqual` []

    it "interrupts an in-flight pull when the sentinel fires" do
      -- The stream's pull sleeps for 500ms before yielding; the
      -- sentinel fires after 25ms. interruptWhen should win the
      -- race and the stream should end with nothing emitted.
      result <- runRIO' do
        sentinel <- makeDeferred
        let
          slow :: RIO () () Int
          slow = do
            liftAff (Aff.delay (Milliseconds 500.0))
            pure 42
          slowStream = Stream.repeatM slow
        _ <- fork do
          liftAff (Aff.delay (Milliseconds 25.0))
          _ <- succeedDeferred sentinel unit
          pure unit
        Stream.runCollect (Stream.interruptWhen sentinel slowStream)
      result `shouldEqual` []
