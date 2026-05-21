module Test.RIO.Fiber.STM.TMapSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TMap (TMap)
import RIO.Fiber.STM.TMap as TMap
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TMap" do
  it "empty starts with size 0 and no member" do
    m <- liftEffect (TMap.empty :: _ (TMap String Int))
    let
      prog :: F.RIO () () { size :: Int, has :: Boolean }
      prog = do
        size <- STM.atomically (TMap.size m)
        has <- STM.atomically (TMap.member "a" m)
        pure { size, has }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.size `shouldEqual` 0
        r.has `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "insert + lookup round-trips" do
    m <- liftEffect (TMap.empty :: _ (TMap String Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = do
        STM.atomically (TMap.insert "a" 1 m)
        STM.atomically (TMap.insert "b" 2 m)
        STM.atomically (TMap.lookup "a" m)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Just 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "insert overwrites a previous value at the same key" do
    m <- liftEffect (TMap.empty :: _ (TMap String Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = do
        STM.atomically (TMap.insert "a" 1 m)
        STM.atomically (TMap.insert "a" 99 m)
        STM.atomically (TMap.lookup "a" m)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Just 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "delete removes the key" do
    m <- liftEffect (TMap.empty :: _ (TMap String Int))
    let
      prog :: F.RIO () () { before :: Boolean, after :: Boolean }
      prog = do
        STM.atomically (TMap.insert "a" 1 m)
        before <- STM.atomically (TMap.member "a" m)
        STM.atomically (TMap.delete "a" m)
        after <- STM.atomically (TMap.member "a" m)
        pure { before, after }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.before `shouldEqual` true
        r.after `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "modify can update or delete the entry at a key" do
    m <- liftEffect (TMap.empty :: _ (TMap String Int))
    let
      prog :: F.RIO () () { after :: Maybe Int, gone :: Maybe Int }
      prog = do
        STM.atomically (TMap.insert "a" 1 m)
        STM.atomically (TMap.modify "a" (\v -> Just (v + 10)) m)
        after <- STM.atomically (TMap.lookup "a" m)
        STM.atomically (TMap.modify "a" (\_ -> Nothing) m)
        gone <- STM.atomically (TMap.lookup "a" m)
        pure { after, gone }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.after `shouldEqual` Just 11
        r.gone `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "toArray snapshots in key order" do
    m <- liftEffect (TMap.empty :: _ (TMap String Int))
    let
      prog :: F.RIO () () (Array (Tuple String Int))
      prog = do
        STM.atomically (TMap.insert "b" 2 m)
        STM.atomically (TMap.insert "a" 1 m)
        STM.atomically (TMap.insert "c" 3 m)
        STM.atomically (TMap.toArray m)
    out <- runAff prog {}
    case out of
      Success xs ->
        xs `shouldEqual`
          [ Tuple "a" 1, Tuple "b" 2, Tuple "c" 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
