module Test.RIO.Aff.IterateReplicateSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, iterate, replicateM, replicateM_, runRIO, runRIO')

spec :: Spec Unit
spec = describe "RIO.Aff.Core (replicateM / replicateM_ / iterate)" do

  describe "replicateM" do
    it "runs the action n times and collects results in order" do
      counter <- liftEffect (Ref.new 0)
      let
        bump :: RIO () () Int
        bump = liftEffect (Ref.modify (_ + 1) counter)
      result <- runRIO' (replicateM 4 bump)
      result `shouldEqual` [ 1, 2, 3, 4 ]
      finalCount <- liftEffect (Ref.read counter)
      finalCount `shouldEqual` 4

    it "returns an empty array for n <= 0 without invoking the action" do
      ran <- liftEffect (Ref.new false)
      let
        action :: RIO () () Unit
        action = liftEffect (Ref.write true ran)
      r1 <- runRIO' (replicateM 0 action)
      r2 <- runRIO' (replicateM (-3) action)
      r1 `shouldEqual` []
      r2 `shouldEqual` []
      fired <- liftEffect (Ref.read ran)
      fired `shouldEqual` false

    it "stops at the first typed failure" do
      counter <- liftEffect (Ref.new 0)
      let
        action :: RIO () (boom :: Int) Int
        action = do
          n <- liftEffect (Ref.modify (_ + 1) counter)
          if n >= 3 then fail (Proxy :: Proxy "boom") n
          else pure n
      result <- runRIO (replicateM 10 action)
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read counter)
      seen `shouldEqual` 3

  describe "replicateM_" do
    it "runs the action n times and discards results" do
      counter <- liftEffect (Ref.new 0)
      let
        bump :: RIO () () Int
        bump = liftEffect (Ref.modify (_ + 1) counter)
      runRIO' (replicateM_ 5 bump)
      finalCount <- liftEffect (Ref.read counter)
      finalCount `shouldEqual` 5

    it "is a no-op for n <= 0" do
      ran <- liftEffect (Ref.new false)
      let
        action :: RIO () () Unit
        action = liftEffect (Ref.write true ran)
      runRIO' (replicateM_ 0 action)
      runRIO' (replicateM_ (-1) action)
      fired <- liftEffect (Ref.read ran)
      fired `shouldEqual` false

  describe "iterate" do
    it "iterates until the predicate becomes false" do
      final <- runRIO'
        (iterate 0 (\n -> n < 5) (\n -> pure (n + 1)))
      final `shouldEqual` 5

    it "returns the seed unchanged when the predicate is false at start" do
      ran <- liftEffect (Ref.new false)
      final <- runRIO'
        ( iterate 42 (\_ -> false)
            ( \n -> do
                liftEffect (Ref.write true ran)
                pure (n + 1)
            )
        )
      final `shouldEqual` 42
      fired <- liftEffect (Ref.read ran)
      fired `shouldEqual` false

    it "threads state through each step" do
      log <- liftEffect (Ref.new [])
      final <- runRIO'
        ( iterate 1 (\n -> n <= 8)
            ( \n -> do
                _ <- liftEffect (Ref.modify (\xs -> xs <> [ n ]) log)
                pure (n * 2)
            )
        )
      final `shouldEqual` 16
      visited <- liftEffect (Ref.read log)
      visited `shouldEqual` [ 1, 2, 4, 8 ]

    it "surfaces a typed failure from the step function" do
      let
        program :: RIO () (boom :: Int) Int
        program = iterate 0 (\n -> n < 10) \n ->
          if n == 3 then fail (Proxy :: Proxy "boom") n
          else pure (n + 1)
      result <- runRIO program
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
