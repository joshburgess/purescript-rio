module Test.RIO.Concurrency.NeverFilterSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Concurrency (filterPar, never, race, timeout)

spec :: Spec Unit
spec = describe "RIO.Concurrency (never / filterPar)" do

  describe "never" do
    it "never wins a race against a completing branch" do
      let
        program :: RIO () () String
        program = race
          never
          ( do
              liftAff (delay (Milliseconds 5.0))
              pure "winner"
          )
      result <- runRIO' program
      result `shouldEqual` "winner"

    it "is interrupted cleanly by timeout (returns Nothing)" do
      let
        program :: RIO () () (Maybe Int)
        program = timeout (Milliseconds 5.0) never
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "filterPar" do
    it "keeps only elements where the predicate returns true" do
      let
        program :: RIO () () (Array Int)
        program = filterPar (\n -> pure (n `mod` 2 == 0))
          [ 1, 2, 3, 4, 5, 6 ]
      result <- runRIO' program
      result `shouldEqual` [ 2, 4, 6 ]

    it "preserves input order among survivors" do
      let
        program :: RIO () () (Array String)
        program = filterPar
          (\s -> pure (s /= "drop"))
          [ "a", "drop", "b", "drop", "c" ]
      result <- runRIO' program
      result `shouldEqual` [ "a", "b", "c" ]

    it "returns an empty array when no element matches" do
      let
        program :: RIO () () (Array Int)
        program = filterPar (\_ -> pure false) [ 1, 2, 3 ]
      result <- runRIO' program
      result `shouldEqual` []

    it "is a no-op on the empty input" do
      let
        program :: RIO () () (Array Int)
        program = filterPar (\_ -> pure true) []
      result <- runRIO' program
      result `shouldEqual` []
