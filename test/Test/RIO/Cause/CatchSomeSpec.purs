module Test.RIO.Cause.CatchSomeSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception as Exception
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Cause (Cause(..), catchSomeCause)
import RIO.Core (RIO, die, fail, runRIO, sandbox)

type Errs = (boom :: Int)

renderBoom :: Variant Errs -> String
renderBoom = Variant.case_ # Variant.on (Proxy :: Proxy "boom") show

spec :: Spec Unit
spec = describe "RIO.Cause.catchSomeCause" do

  it "handles a typed Fail leaf and discharges it" do
    let
      classify :: Cause Errs -> Maybe (RIO () Errs Int)
      classify = case _ of
        Fail _ -> Just (pure 0)
        _ -> Nothing

      program :: RIO () Errs Int
      program = catchSomeCause classify
        (fail (Proxy :: Proxy "boom") 7)
    result <- runRIO program
    result `shouldEqual` (Right 0 :: Either _ _)

  it "handles a Die leaf when the classifier matches it" do
    let
      classify :: Cause Errs -> Maybe (RIO () Errs Int)
      classify = case _ of
        Die _ -> Just (pure 1)
        _ -> Nothing

      program :: RIO () Errs Int
      program = catchSomeCause classify
        (die (Exception.error "kapow"))
    result <- runRIO program
    result `shouldEqual` (Right 1 :: Either _ _)

  it "re-raises a typed Fail unchanged when the classifier declines" do
    let
      classify :: Cause Errs -> Maybe (RIO () Errs Int)
      classify _ = Nothing

      program :: RIO () Errs Int
      program = catchSomeCause classify
        (fail (Proxy :: Proxy "boom") 9)
    result <- runRIO program
    case result of
      Left v -> renderBoom v `shouldEqual` "9"
      Right _ -> 1 `shouldEqual` 0

  it "re-raises a defect unchanged when the classifier declines (observable via sandbox)" do
    let
      classify :: Cause () -> Maybe (RIO () () Int)
      classify _ = Nothing

      program :: RIO () () (Either _ Int)
      program = sandbox
        ( catchSomeCause classify
            (die (Exception.error "boom"))
        )
    result <- runRIO program
    case result of
      Right (Left err) ->
        Exception.message err `shouldEqual` "boom"
      _ -> 1 `shouldEqual` 0

  it "passes a success through untouched" do
    let
      classify :: Cause Errs -> Maybe (RIO () Errs Int)
      classify _ = Just (pure 0)

      program :: RIO () Errs Int
      program = catchSomeCause classify (pure 5)
    result <- runRIO program
    result `shouldEqual` (Right 5 :: Either _ _)
