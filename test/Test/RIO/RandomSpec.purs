module Test.RIO.RandomSpec (spec) where

import Prelude

import Data.Array (length, range) as Array
import Data.Foldable (all, sum)
import Data.Traversable (traverse)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, provideAll, runRIO')
import RIO.Random (Random, nextBoolean, nextInt, nextNumber, nextRange)
import RIO.Test.Random (newTestRandom)

spec :: Spec Unit
spec = do
  describe "RIO.Random + RIO.Test.Random" do
    describe "determinism" do
      it "same seed produces the same sequence" do
        tr1 <- newTestRandom 42
        tr2 <- newTestRandom 42
        let
          drawThree :: RIO (random :: Random) () (Array Number)
          drawThree = traverse (\_ -> nextNumber) [ unit, unit, unit ]
        as <- runRIO' (provideAll { random: tr1.random } drawThree)
        bs <- runRIO' (provideAll { random: tr2.random } drawThree)
        as `shouldEqual` bs

      it "setSeed resets the stream" do
        tr <- newTestRandom 7
        let
          program :: RIO (random :: Random) () Number
          program = nextNumber
        a <- runRIO' (provideAll { random: tr.random } program)
        _ <- runRIO' (provideAll { random: tr.random } program)
        _ <- runRIO' (provideAll { random: tr.random } program)
        tr.setSeed 7
        b <- runRIO' (provideAll { random: tr.random } program)
        a `shouldEqual` b

    describe "nextNumber" do
      it "stays inside [0, 1) across a batch" do
        tr <- newTestRandom 1
        let
          batch :: RIO (random :: Random) () (Array Number)
          batch = traverse (\_ -> nextNumber)
            (Array.range 1 50)
        xs <- runRIO' (provideAll { random: tr.random } batch)
        Array.length xs `shouldEqual` 50
        all (\n -> n >= 0.0 && n < 1.0) xs `shouldEqual` true

    describe "nextInt" do
      it "stays inside the requested closed range" do
        tr <- newTestRandom 1
        let
          batch :: RIO (random :: Random) () (Array Int)
          batch = traverse (\_ -> nextInt 1 6) (Array.range 1 100)
        xs <- runRIO' (provideAll { random: tr.random } batch)
        Array.length xs `shouldEqual` 100
        all (\n -> n >= 1 && n <= 6) xs `shouldEqual` true

    describe "nextRange" do
      it "stays inside [min, max)" do
        tr <- newTestRandom 1
        let
          batch :: RIO (random :: Random) () (Array Number)
          batch = traverse (\_ -> nextRange 2.5 5.5)
            (Array.range 1 50)
        xs <- runRIO' (provideAll { random: tr.random } batch)
        all (\n -> n >= 2.5 && n < 5.5) xs `shouldEqual` true

    describe "nextBoolean" do
      it "produces both values across a long-enough run" do
        tr <- newTestRandom 1
        let
          batch :: RIO (random :: Random) () (Array Boolean)
          batch = traverse (\_ -> nextBoolean) (Array.range 1 50)
        xs <- runRIO' (provideAll { random: tr.random } batch)
        let trueCount = sum (map (\b -> if b then 1 else 0) xs)
        (trueCount > 0 && trueCount < 50) `shouldEqual` true
