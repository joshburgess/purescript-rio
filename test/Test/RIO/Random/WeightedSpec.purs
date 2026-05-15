module Test.RIO.Random.WeightedSpec (spec) where

import Prelude

import Data.Array (filter, length, replicate) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, provideAll, runRIO')
import RIO.Random (Random, weighted)
import RIO.Test.Random (newTestRandom)

spec :: Spec Unit
spec = describe "RIO.Random (weighted)" do

  it "returns Nothing on an empty array" do
    tr <- newTestRandom 1
    let
      program :: RIO (random :: Random) () (Maybe Int)
      program = weighted []
    result <- runRIO' (provideAll { random: tr.random } program)
    result `shouldEqual` Nothing

  it "returns Nothing when every weight is non-positive" do
    tr <- newTestRandom 2
    let
      program :: RIO (random :: Random) () (Maybe Int)
      program = weighted
        [ Tuple 0.0 1
        , Tuple (-1.0) 2
        , Tuple 0.0 3
        ]
    result <- runRIO' (provideAll { random: tr.random } program)
    result `shouldEqual` Nothing

  it "always returns the only positive-weight entry" do
    tr <- newTestRandom 3
    let
      program :: RIO (random :: Random) () (Maybe Int)
      program = weighted
        [ Tuple 0.0 1
        , Tuple 5.0 42
        , Tuple (-2.0) 3
        ]
    result <- runRIO' (provideAll { random: tr.random } program)
    result `shouldEqual` Just 42

  it "is deterministic given the same seed" do
    tr1 <- newTestRandom 99
    tr2 <- newTestRandom 99
    let
      pairs =
        [ Tuple 1.0 "a"
        , Tuple 2.0 "b"
        , Tuple 3.0 "c"
        , Tuple 4.0 "d"
        ]

      program :: RIO (random :: Random) () (Maybe String)
      program = weighted pairs
    a <- runRIO' (provideAll { random: tr1.random } program)
    b <- runRIO' (provideAll { random: tr2.random } program)
    a `shouldEqual` b

  it "produces a sample distribution roughly matching the weights" do
    tr <- newTestRandom 1234
    countsRef <- liftEffect (Ref.new ([] :: Array (Tuple String Int)))
    let
      pairs =
        [ Tuple 90.0 "common"
        , Tuple 10.0 "rare"
        ]
      n = 5000

      program :: RIO (random :: Random) () Unit
      program = do
        for_ (Array.replicate n unit) \_ -> do
          pick <- weighted pairs
          let label = fromMaybe "miss" pick
          liftEffect (Ref.modify_ (bumpCount label) countsRef)
    _ <- runRIO' (provideAll { random: tr.random } program)
    counts <- liftEffect (Ref.read countsRef)
    let
      commonCount = lookupCount "common" counts
      rareCount = lookupCount "rare" counts
      missCount = lookupCount "miss" counts
    -- with a 90 / 10 split over 5000 draws the expected counts are
    -- around 4500 / 500; allow generous slack since this is a
    -- finite-sample chi-squared-ish check.
    missCount `shouldEqual` 0
    (commonCount + rareCount) `shouldEqual` n
    (commonCount > 4200) `shouldEqual` true
    (commonCount < 4800) `shouldEqual` true
    (rareCount > 200) `shouldEqual` true
    (rareCount < 800) `shouldEqual` true

  where
  bumpCount :: String -> Array (Tuple String Int) -> Array (Tuple String Int)
  bumpCount label arr =
    let
      existing = Array.filter (\(Tuple k _) -> k == label) arr
    in
      case Array.length existing of
        0 -> arr <> [ Tuple label 1 ]
        _ -> map (\(Tuple k v) -> if k == label then Tuple k (v + 1) else Tuple k v) arr

  lookupCount :: String -> Array (Tuple String Int) -> Int
  lookupCount label arr =
    case Array.filter (\(Tuple k _) -> k == label) arr of
      [ Tuple _ v ] -> v
      _ -> 0
