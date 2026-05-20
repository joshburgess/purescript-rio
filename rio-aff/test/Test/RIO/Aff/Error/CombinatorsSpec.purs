module Test.RIO.Aff.Error.CombinatorsSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, runRIO, runRIO')
import RIO.Aff.Error
  ( absolve
  , either
  , fromEither
  , fromMaybe
  , tap
  , tapError
  )

spec :: Spec Unit
spec = describe "RIO.Aff.Error (T1 combinators)" do

  describe "tap" do
    it "runs the action on success and passes the value through" do
      sink <- liftEffect (Ref.new (0 :: Int))
      let
        program :: RIO () () Int
        program = tap
          (\a -> liftEffect (Ref.write a sink))
          (pure 42)
      result <- runRIO program
      result `shouldEqual` (Right 42 :: Either _ Int)
      seen <- liftEffect (Ref.read sink)
      seen `shouldEqual` 42

    it "skips the action and propagates a typed failure" do
      sink <- liftEffect (Ref.new (0 :: Int))
      let
        program :: RIO () (boom :: Int) Int
        program = tap
          (\_ -> liftEffect (Ref.write 999 sink))
          (fail (Proxy :: Proxy "boom") 1)
      result <- runRIO program
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read sink)
      seen `shouldEqual` 0

  describe "tapError" do
    it "runs the action on a typed failure and re-raises unchanged" do
      sink <- liftEffect (Ref.new "")
      let
        program :: RIO () (boom :: String) Int
        program = tapError
          ( \v -> liftEffect
              ( Ref.write
                  ( Variant.case_
                      # Variant.on (Proxy :: Proxy "boom") identity
                      $ v
                  )
                  sink
              )
          )
          (fail (Proxy :: Proxy "boom") "kaboom")
      result <- runRIO program
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read sink)
      seen `shouldEqual` "kaboom"

    it "is a no-op on success" do
      sink <- liftEffect (Ref.new false)
      let
        program :: RIO () (boom :: String) Int
        program = tapError
          (\_ -> liftEffect (Ref.write true sink))
          (pure 7)
      result <- runRIO program
      result `shouldEqual` (Right 7 :: Either _ Int)
      ran <- liftEffect (Ref.read sink)
      ran `shouldEqual` false

  describe "fromEither" do
    it "lifts a Right into the success channel" do
      let
        program :: RIO () (boom :: Int) String
        program = fromEither (Right "ok")
      result <- runRIO program
      result `shouldEqual` (Right "ok" :: Either _ String)

    it "lifts a Left into a typed failure" do
      let
        program :: RIO () (boom :: Int) String
        program = fromEither
          (Left (Variant.inj (Proxy :: Proxy "boom") 13))
      result <- runRIO program
      case result of
        Left v ->
          let
            n =
              Variant.case_
                # Variant.on (Proxy :: Proxy "boom") identity
                $ v
          in
            n `shouldEqual` 13
        Right _ -> 1 `shouldEqual` 0

  describe "fromMaybe" do
    it "lifts Just into the success channel" do
      let
        program :: RIO () (missing :: String) Int
        program = fromMaybe
          (Variant.inj (Proxy :: Proxy "missing") "x")
          (Just 5)
      result <- runRIO program
      result `shouldEqual` (Right 5 :: Either _ Int)

    it "lifts Nothing into the supplied typed failure" do
      let
        program :: RIO () (missing :: String) Int
        program = fromMaybe
          (Variant.inj (Proxy :: Proxy "missing") "alice")
          Nothing
      result <- runRIO program
      case result of
        Left v ->
          let
            who =
              Variant.case_
                # Variant.on (Proxy :: Proxy "missing") identity
                $ v
          in
            who `shouldEqual` "alice"
        Right _ -> 1 `shouldEqual` 0

  describe "either" do
    it "wraps a success as Right and clears the error row" do
      let
        program :: RIO () () (Either (Variant.Variant (boom :: Int)) Int)
        program = either
          (pure 1 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      case result of
        Right 1 -> pure unit
        _ -> 1 `shouldEqual` 0

    it "wraps a typed failure as Left and clears the error row" do
      let
        program :: RIO () () (Either (Variant.Variant (boom :: Int)) Int)
        program = either
          (fail (Proxy :: Proxy "boom") 42 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      case result of
        Left v ->
          let
            n =
              Variant.case_
                # Variant.on (Proxy :: Proxy "boom") identity
                $ v
          in
            n `shouldEqual` 42
        Right _ -> 1 `shouldEqual` 0

  describe "absolve" do
    it "lifts an inner Right back into a plain success" do
      let
        inner :: RIO () (boom :: Int) (Either (Variant.Variant (boom :: Int)) Int)
        inner = pure (Right 99)

        program :: RIO () (boom :: Int) Int
        program = absolve inner
      result <- runRIO program
      result `shouldEqual` (Right 99 :: Either _ Int)

    it "lifts an inner Left into a typed failure on the row" do
      let
        inner :: RIO () (boom :: Int) (Either (Variant.Variant (boom :: Int)) Int)
        inner = pure
          (Left (Variant.inj (Proxy :: Proxy "boom") 7))

        program :: RIO () (boom :: Int) Int
        program = absolve inner
      result <- runRIO program
      case result of
        Left v ->
          let
            n =
              Variant.case_
                # Variant.on (Proxy :: Proxy "boom") identity
                $ v
          in
            n `shouldEqual` 7
        Right _ -> 1 `shouldEqual` 0

    it "round-trips through either: absolve (either p) == p" do
      let
        program :: RIO () (boom :: Int) Int
        program = absolve (either (pure 11 :: RIO () (boom :: Int) Int))
      result <- runRIO program
      result `shouldEqual` (Right 11 :: Either _ Int)
