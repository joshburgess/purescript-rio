module Test.RIO.Aff.FoldForeverSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core
  ( RIO
  , ask
  , fail
  , forkScoped
  , interrupt
  , runRIO
  , runRIO'
  , scoped
  )
import RIO.Aff.Concurrency (forever)
import RIO.Aff.Error (foldRIO)

spec :: Spec Unit
spec = describe "RIO.Aff.Error.foldRIO / RIO.Aff.Concurrency.forever" do

  describe "foldRIO" do
    it "runs onSuccess and clears the error row" do
      let
        program :: RIO () () String
        program =
          foldRIO
            (\_ -> pure "should not run")
            (\n -> pure ("got " <> show n))
            (pure 42 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      result `shouldEqual` "got 42"

    it "runs onError when the input fails, clearing the error row" do
      let
        program :: RIO () () String
        program =
          foldRIO
            ( \v ->
                let
                  n =
                    Variant.case_
                      # Variant.on (Proxy :: Proxy "boom") identity
                      $ v
                in
                  pure ("handled " <> show n)
            )
            (\n -> pure ("ok " <> show n))
            (fail (Proxy :: Proxy "boom") 7 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      result `shouldEqual` "handled 7"

    it "lets onError introduce a fresh failure on the new row" do
      let
        program :: RIO () (replaced :: String) Int
        program =
          foldRIO
            (\_ -> fail (Proxy :: Proxy "replaced") "rewritten")
            pure
            (fail (Proxy :: Proxy "boom") 1 :: RIO () (boom :: Int) Int)
      result <- runRIO program
      case result of
        Left v ->
          let
            payload =
              Variant.case_
                # Variant.on (Proxy :: Proxy "replaced") identity
                $ v
          in
            payload `shouldEqual` "rewritten"
        Right _ -> 1 `shouldEqual` 0

  describe "forever" do
    it "loops until the fiber is interrupted, then stops cleanly" do
      counter <- liftEffect (Ref.new 0)
      let
        tick :: forall r. RIO r () Unit
        tick = do
          _ <- liftEffect (Ref.modify_ (_ + 1) counter)
          liftAff (delay (Milliseconds 2.0))

        program :: RIO () () Unit
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          fib <- forkScoped scope (forever tick)
          liftAff (delay (Milliseconds 25.0))
          interrupt fib
      result <- runRIO program
      result `shouldEqual` (Right unit :: Either _ Unit)
      seen <- liftEffect (Ref.read counter)
      (seen > 0) `shouldEqual` true

    it "is terminated by scope exit (no explicit interrupt needed)" do
      counter <- liftEffect (Ref.new 0)
      let
        tick :: forall r. RIO r () Unit
        tick = do
          _ <- liftEffect (Ref.modify_ (_ + 1) counter)
          liftAff (delay (Milliseconds 2.0))

        program :: RIO () () Unit
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          _ <- forkScoped scope (forever tick)
          liftAff (delay (Milliseconds 15.0))
      result <- runRIO program
      result `shouldEqual` (Right unit :: Either _ Unit)
      -- After the scope returns, the background loop should have
      -- been interrupted. Give the runtime a moment, then assert
      -- the counter isn't continuing to climb.
      liftAff (delay (Milliseconds 20.0))
      a <- liftEffect (Ref.read counter)
      liftAff (delay (Milliseconds 20.0))
      b <- liftEffect (Ref.read counter)
      a `shouldEqual` b
