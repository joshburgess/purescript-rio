module Test.RIO.Aff.Random.ShufflePickSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.Random (Random, pickRandom, shuffle)
import RIO.Aff.Test.Random (newTestRandom)

spec :: Spec Unit
spec = describe "RIO.Aff.Random (shuffle / pickRandom)" do

  describe "shuffle" do
    it "preserves length" do
      tr <- newTestRandom 1
      let
        xs = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

        program :: RIO (random :: Random) () (Array Int)
        program = shuffle xs
      result <- runRIO' (provideAll { random: tr.random } program)
      Array.length result `shouldEqual` Array.length xs

    it "preserves the multiset of elements (sorted result matches sorted input)" do
      tr <- newTestRandom 2
      let
        xs = [ 5, 2, 8, 1, 9, 3, 7, 4, 6 ]

        program :: RIO (random :: Random) () (Array Int)
        program = shuffle xs
      result <- runRIO' (provideAll { random: tr.random } program)
      Array.sort result `shouldEqual` Array.sort xs

    it "is deterministic given the same seed" do
      tr1 <- newTestRandom 42
      tr2 <- newTestRandom 42
      let
        xs = [ 'a', 'b', 'c', 'd', 'e', 'f' ]

        program :: RIO (random :: Random) () (Array Char)
        program = shuffle xs
      a <- runRIO' (provideAll { random: tr1.random } program)
      b <- runRIO' (provideAll { random: tr2.random } program)
      a `shouldEqual` b

    it "is a no-op on an empty array" do
      tr <- newTestRandom 1
      let
        program :: RIO (random :: Random) () (Array Int)
        program = shuffle []
      result <- runRIO' (provideAll { random: tr.random } program)
      result `shouldEqual` []

    it "is a no-op on a singleton" do
      tr <- newTestRandom 1
      let
        program :: RIO (random :: Random) () (Array Int)
        program = shuffle [ 99 ]
      result <- runRIO' (provideAll { random: tr.random } program)
      result `shouldEqual` [ 99 ]

  describe "pickRandom" do
    it "returns Just an element from the array" do
      tr <- newTestRandom 1
      let
        xs = [ 10, 20, 30, 40, 50 ]

        program :: RIO (random :: Random) () (Maybe Int)
        program = pickRandom xs
      result <- runRIO' (provideAll { random: tr.random } program)
      case result of
        Just n -> Array.elem n xs `shouldEqual` true
        Nothing -> 1 `shouldEqual` 0

    it "returns Nothing on an empty array" do
      tr <- newTestRandom 1
      let
        program :: RIO (random :: Random) () (Maybe Int)
        program = pickRandom []
      result <- runRIO' (provideAll { random: tr.random } program)
      result `shouldEqual` Nothing

    it "always returns the single element on a singleton" do
      tr <- newTestRandom 7
      let
        program :: RIO (random :: Random) () (Maybe Int)
        program = pickRandom [ 7 ]
      result <- runRIO' (provideAll { random: tr.random } program)
      result `shouldEqual` Just 7

    it "is deterministic given the same seed" do
      tr1 <- newTestRandom 99
      tr2 <- newTestRandom 99
      let
        xs = [ 1, 2, 3, 4, 5 ]

        program :: RIO (random :: Random) () (Maybe Int)
        program = pickRandom xs
      a <- runRIO' (provideAll { random: tr1.random } program)
      b <- runRIO' (provideAll { random: tr2.random } program)
      a `shouldEqual` b
