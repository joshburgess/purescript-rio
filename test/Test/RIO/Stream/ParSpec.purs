module Test.RIO.Stream.ParSpec (spec) where

import Prelude

import Data.Array (range, sort) as Array
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO)
import RIO.Stream (Stream, fromArray, mapM, runCollect)
import RIO.Stream.Par (merge, mergeAll, mergeMap)

spec :: Spec Unit
spec = do
  describe "RIO.Stream.Par" do

    describe "mergeAll" do
      it "produces an empty stream from an empty input array" do
        r <- runRIO
          ( runCollect (mergeAll [] :: Stream () () Int)
          )
        r `shouldEqual` Right []

      it "yields every value across all inputs" do
        let
          s1 = fromArray [ 1, 2, 3 ]
          s2 = fromArray [ 10, 20, 30 ]
          s3 = fromArray [ 100, 200, 300 ]
        r <- runRIO (runCollect (mergeAll [ s1, s2, s3 ]))
        case r of
          Right xs ->
            Array.sort xs
              `shouldEqual` [ 1, 2, 3, 10, 20, 30, 100, 200, 300 ]
          Left _ ->
            Spec.fail "expected mergeAll to succeed"

      it "single-stream input behaves like the input" do
        r <- runRIO
          (runCollect (mergeAll [ fromArray (Array.range 1 5) ]))
        case r of
          Right xs -> Array.sort xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
          Left _ -> Spec.fail "expected single-stream mergeAll to succeed"

      it "interleaves slow producers (both contribute)" do
        let
          slow ms n = mapM
            (\v -> liftAff (delay (Milliseconds ms)) *> pure v)
            (fromArray [ n, n + 1, n + 2 ])
          s1 = slow 5.0 1
          s2 = slow 5.0 100
        r <- runRIO (runCollect (mergeAll [ s1, s2 ]))
        case r of
          Right xs -> do
            Array.sort xs `shouldEqual` [ 1, 2, 3, 100, 101, 102 ]
          Left _ -> Spec.fail "expected mergeAll to succeed"

      it "propagates a typed failure from one producer" do
        let
          bad :: Stream () (boom :: String) Int
          bad = mapM
            (\_ -> fail (Proxy :: Proxy "boom") "kaboom")
            (fromArray [ 1 ])

          good :: Stream () (boom :: String) Int
          good = fromArray [ 2, 3 ]
        r <- runRIO (runCollect (mergeAll [ good, bad ]))
        case r of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected mergeAll to surface the typed failure"

    describe "merge" do
      it "two-stream merge collects every element" do
        r <- runRIO
          ( runCollect
              ( merge (fromArray [ 1, 2, 3 ])
                  (fromArray [ 4, 5, 6 ])
              )
          )
        case r of
          Right xs ->
            Array.sort xs `shouldEqual` [ 1, 2, 3, 4, 5, 6 ]
          Left _ -> Spec.fail "expected merge to succeed"

    describe "mergeMap" do
      it "fans each outer element out into its inner stream" do
        let
          outer = fromArray [ 1, 2, 3 ]
          f n = fromArray [ n, n * 10 ]
        r <- runRIO (runCollect (mergeMap f outer))
        case r of
          Right xs ->
            Array.sort xs `shouldEqual` [ 1, 2, 3, 10, 20, 30 ]
          Left _ -> Spec.fail "expected mergeMap to succeed"

      it "empty outer produces empty output" do
        let
          program :: RIO () () (Array Int)
          program = runCollect
            (mergeMap (\n -> fromArray [ n ]) (fromArray []))
        r <- runRIO program
        r `shouldEqual` Right []

      it "empty inners contribute nothing" do
        let
          program :: RIO () () (Array Int)
          program = runCollect
            ( mergeMap (\_ -> fromArray ([] :: Array Int))
                (fromArray [ 1, 2, 3 ])
            )
        r <- runRIO program
        r `shouldEqual` Right []
