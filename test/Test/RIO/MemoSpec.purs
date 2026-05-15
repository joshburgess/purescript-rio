module Test.RIO.MemoSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO, runRIO')
import RIO.Concurrency (fork, join, parSequence)
import RIO.Memo (memoize)

spec :: Spec Unit
spec = describe "RIO.Memo" do

  it "runs the underlying action exactly once across repeated calls" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () (Array Int)
      program = do
        getOnce <- memoize
          ( do
              _ <- liftEffect (Ref.modify_ (_ + 1) counter)
              pure 42
          )
        a <- getOnce
        b <- getOnce
        c <- getOnce
        pure [ a, b, c ]
    values <- runRIO' program
    values `shouldEqual` [ 42, 42, 42 ]
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 1

  it "single-flights concurrent first calls (only one underlying run)" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () (Array Int)
      program = do
        getOnce <- memoize
          ( do
              _ <- liftEffect (Ref.modify_ (_ + 1) counter)
              -- a real async boundary so the second caller has a
              -- chance to observe the in-flight AVar
              _ <- liftAff (delay (Milliseconds 5.0))
              pure 7
          )
        -- launch many fibers that all hit the memo before it settles
        fibers <- parSequence
          [ fork getOnce
          , fork getOnce
          , fork getOnce
          , fork getOnce
          ]
        results <- parSequence (map join fibers)
        pure results
    values <- runRIO' program
    values `shouldEqual` [ 7, 7, 7, 7 ]
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 1

  it "caches a typed failure: subsequent calls see the same failure" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () (boom :: Int) Int
      program = do
        getOnce <- memoize
          ( do
              _ <- liftEffect (Ref.modify_ (_ + 1) counter)
              fail (Proxy :: Proxy "boom") 99
          )
        _ <- getOnce
        getOnce
    result <- runRIO program
    case result of
      Left _ -> pure unit
      Right _ -> 1 `shouldEqual` 0
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 1

  it "independent memo cells from the same template run independently" do
    counter <- liftEffect (Ref.new 0)
    let
      template :: RIO () () Int
      template = do
        _ <- liftEffect (Ref.modify_ (_ + 1) counter)
        pure 1

      program :: RIO () () Int
      program = do
        cellA <- memoize template
        cellB <- memoize template
        _ <- cellA
        _ <- cellA
        _ <- cellB
        _ <- cellB
        pure 0
    _ <- runRIO' program
    -- two cells, each ran once
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 2
