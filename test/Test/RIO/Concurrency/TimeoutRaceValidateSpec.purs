module Test.RIO.Concurrency.TimeoutRaceValidateSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Clock (Clock, sleep)
import RIO.Core
  ( RIO
  , fail
  , partition
  , provideAll
  , raceEither
  , runRIO
  , runRIO'
  , timeoutFail
  , validate
  )
import RIO.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Concurrency (timeoutFail / raceEither / validate / partition)" do

  describe "timeoutFail" do
    it "returns the value when the action beats the deadline" do
      let
        program :: RIO () (slow :: String) Int
        program = timeoutFail
          (Proxy :: Proxy "slow")
          "n/a"
          (Milliseconds 100.0)
          (pure 42)
      result <- runRIO program
      result `shouldEqual` (Right 42 :: Either _ _)

    it "raises the typed failure when the deadline fires first" do
      tc <- newTestClock
      let
        slowAction :: RIO (clock :: Clock) (slow :: String) Int
        slowAction = do
          sleep (Milliseconds 100.0)
          pure 1

        program :: RIO (clock :: Clock) (slow :: String) Int
        program = timeoutFail
          (Proxy :: Proxy "slow")
          "ran out"
          (Milliseconds 5.0)
          slowAction
      result <- runRIO (provideAll { clock: tc.clock } program)
      case result of
        Left v ->
          let
            payload =
              Variant.case_ # Variant.on (Proxy :: Proxy "slow") identity $ v
          in
            payload `shouldEqual` "ran out"
        Right _ -> 1 `shouldEqual` 0

  describe "raceEither" do
    it "preserves the winning arm's tag (left wins)" do
      tc <- newTestClock
      let
        fastLeft :: RIO (clock :: Clock) () String
        fastLeft = pure "L"

        slowRight :: RIO (clock :: Clock) () Int
        slowRight = do
          sleep (Milliseconds 100.0)
          pure 99

        program :: RIO (clock :: Clock) () (Either String Int)
        program = raceEither fastLeft slowRight
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result `shouldEqual` (Left "L" :: Either String Int)

    it "preserves the winning arm's tag (right wins)" do
      tc <- newTestClock
      let
        slowLeft :: RIO (clock :: Clock) () String
        slowLeft = do
          sleep (Milliseconds 100.0)
          pure "L"

        fastRight :: RIO (clock :: Clock) () Int
        fastRight = pure 7

        program :: RIO (clock :: Clock) () (Either String Int)
        program = raceEither slowLeft fastRight
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result `shouldEqual` (Right 7 :: Either String Int)

  describe "validate (sequential)" do
    it "accumulates every typed failure in input order" do
      let
        check :: Int -> RIO () (bad :: Int) Int
        check n =
          if n < 0 then fail (Proxy :: Proxy "bad") n
          else pure n

        program :: RIO () () (Either (NonEmptyArray (Variant (bad :: Int))) (Array Int))
        program = validate check [ 1, -2, 3, -4, -5 ]
      result <- runRIO' program
      case result of
        Right _ -> 1 `shouldEqual` 0
        Left nea -> do
          let
            payloads =
              map
                (\v -> Variant.case_ # Variant.on (Proxy :: Proxy "bad") identity $ v)
                (NEA.toArray nea)
          payloads `shouldEqual` [ -2, -4, -5 ]

    it "returns Right with all successes when every action succeeds" do
      let
        check :: Int -> RIO () (bad :: Int) Int
        check n = pure (n * 2)

        program :: RIO () () (Either (NonEmptyArray (Variant (bad :: Int))) (Array Int))
        program = validate check [ 1, 2, 3 ]
      result <- runRIO' program
      result `shouldEqual` (Right [ 2, 4, 6 ] :: Either _ _)

  describe "partition (sequential)" do
    it "splits errors and successes preserving input order on each side" do
      let
        check :: Int -> RIO () (bad :: Int) String
        check n =
          if n < 0 then fail (Proxy :: Proxy "bad") n
          else pure (show n)

        program
          :: RIO () ()
               (Tuple (Array (Variant (bad :: Int))) (Array String))
        program = partition check [ 1, -2, 3, -4, 5 ]
      Tuple errs okays <- runRIO' program
      Array.length errs `shouldEqual` 2
      okays `shouldEqual` [ "1", "3", "5" ]
      let
        payloads =
          map
            (\v -> Variant.case_ # Variant.on (Proxy :: Proxy "bad") identity $ v)
            errs
      payloads `shouldEqual` [ -2, -4 ]
