module Test.RIO.Fiber.RandomSpec (spec) where

import Prelude

import Data.Array (index, length, replicate)
import Data.Array as Data.Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Random (Random(..))
import RIO.Fiber.Random as Random
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | Deterministic Random backed by canned arrays. Each `next*` call
-- | advances its own counter and returns the next element; once the
-- | array is exhausted the corresponding fallback is returned.
mkCannedRandom
  :: Array Number
  -> Array Int
  -> Array Boolean
  -> Effect Random
mkCannedRandom nums ints bools = do
  numIdx <- Ref.new 0
  intIdx <- Ref.new 0
  boolIdx <- Ref.new 0
  let
    bump
      :: forall a
       . Ref.Ref Int
      -> Array a
      -> a
      -> Effect a
    bump idx xs fallback = do
      i <- Ref.read idx
      Ref.write (i + 1) idx
      case index xs i of
        Just a -> pure a
        Nothing -> pure fallback
  pure $ Random
    { number: bump numIdx nums 0.0
    , int: \_ _ -> bump intIdx ints 0
    , boolean: bump boolIdx bools false
    }

spec :: Spec Unit
spec = describe "rio-fiber: Random" do
  describe "default service" do
    it "nextNumber returns a Number in [0, 1)" do
      let
        prog :: F.RIO () () Number
        prog = Random.nextNumber
      out <- runAff prog {}
      case out of
        Success n
          | n >= 0.0 && n < 1.0 -> pure unit
          | otherwise -> fail ("not in [0,1): " <> show n)
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "nextInt lo hi returns a value in the closed interval" do
      let
        prog :: F.RIO () () (Array Int)
        prog = traverse (\_ -> Random.nextInt 10 20) (replicate 50 unit)
      out <- runAff prog {}
      case out of
        Success xs -> do
          length xs `shouldEqual` 50
          let
            allIn = case findOutOfRange xs 10 20 of
              Nothing -> true
              Just _ -> false
          allIn `shouldEqual` true
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "nextInt where lo == hi always returns that value" do
      let
        prog :: F.RIO () () (Array Int)
        prog = traverse (\_ -> Random.nextInt 7 7) (replicate 20 unit)
      out <- runAff prog {}
      case out of
        Success xs ->
          case findOutOfRange xs 7 7 of
            Nothing -> pure unit
            Just bad -> fail ("got " <> show bad <> " from nextInt 7 7")
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "nextBoolean returns a Boolean" do
      out <- runAff (Random.nextBoolean :: F.RIO () () Boolean) {}
      case out of
        Success _ -> pure unit
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "withRandom" do
    it "scopes the override to the body and returns canned values" do
      fake <- liftEffect
        (mkCannedRandom [ 0.1, 0.2, 0.3 ] [ 42, 43, 44 ] [ true, false ])
      let
        prog :: F.RIO () ()
          { n1 :: Number
          , n2 :: Number
          , i1 :: Int
          , i2 :: Int
          , b1 :: Boolean
          , b2 :: Boolean
          }
        prog = Random.withRandom fake do
          n1 <- Random.nextNumber
          n2 <- Random.nextNumber
          i1 <- Random.nextInt 0 100
          i2 <- Random.nextInt 0 100
          b1 <- Random.nextBoolean
          b2 <- Random.nextBoolean
          pure { n1, n2, i1, i2, b1, b2 }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.n1 `shouldEqual` 0.1
          r.n2 `shouldEqual` 0.2
          r.i1 `shouldEqual` 42
          r.i2 `shouldEqual` 43
          r.b1 `shouldEqual` true
          r.b2 `shouldEqual` false
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "restores the previous Random after the body returns" do
      fake <- liftEffect (mkCannedRandom [ 0.99 ] [ 12345 ] [ true ])
      let
        prog :: F.RIO () () { inside :: Int, outsideMatchesFake :: Boolean }
        prog = do
          inside <- Random.withRandom fake (Random.nextInt 0 1)
          outside <- Random.nextInt 0 1
          -- The canned int (12345) is outside [0,1], so if `outside`
          -- equals that value, the override leaked. The default service
          -- should return either 0 or 1.
          pure
            { inside
            , outsideMatchesFake: outside == 12345
            }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.inside `shouldEqual` 12345
          r.outsideMatchesFake `shouldEqual` false
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "child fibers inherit the parent Random override" do
      fake <- liftEffect (mkCannedRandom [] [ 99 ] [])
      let
        prog :: F.RIO () () Int
        prog = Random.withRandom fake do
          f <- F.fork (Random.nextInt 0 0)
          F.join f
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 99
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "setRandom" do
    it "setRandom changes the active implementation for subsequent reads" do
      fake <- liftEffect (mkCannedRandom [ 0.5 ] [ 7 ] [ false ])
      let
        prog :: F.RIO () () { x :: Number, y :: Int }
        prog = do
          Random.setRandom fake
          x <- Random.nextNumber
          y <- Random.nextInt 0 0
          pure { x, y }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.x `shouldEqual` 0.5
          r.y `shouldEqual` 7
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "shuffle" do
    it "preserves length and elements" do
      let
        xs = [ 1, 2, 3, 4, 5, 6, 7, 8 ]
        prog :: F.RIO () () (Array Int)
        prog = Random.shuffle xs
      out <- runAff prog {}
      case out of
        Success ys -> do
          length ys `shouldEqual` length xs
          Data.Array.sort ys `shouldEqual` Data.Array.sort xs
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "on an empty array yields an empty array" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Random.shuffle []
      out <- runAff prog {}
      case out of
        Success ys -> ys `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "is deterministic under a canned Random override" do
      -- Fisher-Yates picks j in [0, i] for i = n-1, n-2, ..., 1.
      -- For [10, 20, 30] (n = 3): i=2 picks j=0 → [30, 20, 10];
      -- i=1 picks j=1 → no swap → [30, 20, 10].
      fake <- liftEffect (mkCannedRandom [] [ 0, 1 ] [])
      let
        prog :: F.RIO () () (Array Int)
        prog = Random.withRandom fake (Random.shuffle [ 10, 20, 30 ])
      out <- runAff prog {}
      case out of
        Success ys -> ys `shouldEqual` [ 30, 20, 10 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "choice" do
    it "returns Nothing on the empty array" do
      let
        prog :: F.RIO () () (Maybe Int)
        prog = Random.choice []
      out <- runAff prog {}
      case out of
        Success m -> m `shouldEqual` Nothing
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "returns Just on a non-empty array (chosen index from active Random)" do
      fake <- liftEffect (mkCannedRandom [] [ 2 ] [])
      let
        prog :: F.RIO () () (Maybe Int)
        prog = Random.withRandom fake (Random.choice [ 10, 20, 30, 40 ])
      out <- runAff prog {}
      case out of
        Success m -> m `shouldEqual` Just 30
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "uuid" do
    it "returns a non-empty string of the expected v4 shape" do
      let
        prog :: F.RIO () () String
        prog = Random.uuid
      out <- runAff prog {}
      case out of
        Success s -> do
          -- 8-4-4-4-12 hex digits separated by hyphens = 36 chars total.
          String.length s `shouldEqual` 36
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "successive uuids differ" do
      let
        prog :: F.RIO () () { a :: String, b :: String }
        prog = do
          a <- Random.uuid
          b <- Random.uuid
          pure { a, b }
      out <- runAff prog {}
      case out of
        Success r -> (r.a == r.b) `shouldEqual` false
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "bytes" do
    it "returns an array of the requested length" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Random.bytes 16
      out <- runAff prog {}
      case out of
        Success xs -> length xs `shouldEqual` 16
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "every byte is in [0, 255]" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Random.bytes 64
      out <- runAff prog {}
      case out of
        Success xs -> case findOutOfRange xs 0 255 of
          Nothing -> pure unit
          Just bad -> fail ("byte out of range: " <> show bad)
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "n <= 0 yields the empty array" do
      let
        prog :: F.RIO () () (Array Int)
        prog = Random.bytes 0
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` []
        other -> fail ("expected Success, got " <> describeOutcome other)

findOutOfRange :: Array Int -> Int -> Int -> Maybe Int
findOutOfRange xs lo hi = go 0
  where
  go i = case index xs i of
    Nothing -> Nothing
    Just x ->
      if x < lo || x > hi then Just x
      else go (i + 1)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
