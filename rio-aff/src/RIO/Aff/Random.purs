-- | A `Random` service plus a live implementation.
-- |
-- | Mirrors `RIO.Aff.Clock` / `RIO.Aff.Test.Clock`: the live implementation
-- | delegates to `Effect.Random`, the test implementation lives in
-- | `RIO.Aff.Test.Random` and gives you a seedable, deterministic
-- | sequence for replayable tests.
-- |
-- | The service exposes four operations: a uniform `Number` in
-- | `[0, 1)`, a uniform `Int` in a closed range, a uniform `Number`
-- | in a half-open range, and a fair `Boolean`. Range checks (high
-- | less than low) are not
-- | enforced; the result is unspecified in that case, matching
-- | `Effect.Random.randomInt`.
-- |
-- | ```purescript
-- | rollDie :: forall r e. RIO (random :: Random | r) e Int
-- | rollDie = nextInt 1 6
-- | ```
module RIO.Aff.Random
  ( Random
  , bytes
  , nextBoolean
  , nextInt
  , nextNumber
  , nextRange
  , pickRandom
  , shuffle
  , uuid
  , weighted
  , liveRandom
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (random, randomBool, randomInt, randomRange) as Random
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask)

foreign import _uuid :: Effect String
foreign import _bytes :: Int -> Effect (Array Int)

-- | The service record.
-- |
-- |   * `nextNumber` returns a uniform `Number` in `[0, 1)`.
-- |   * `nextInt low high` returns a uniform `Int` in the closed
-- |     interval `[low, high]`. Behaviour is unspecified if
-- |     `low > high`.
-- |   * `nextRange min max` returns a uniform `Number` in
-- |     `[min, max)`. Behaviour is unspecified if `max < min`.
-- |   * `nextBoolean` returns `true` or `false` with equal
-- |     probability.
type Random =
  { nextNumber :: Aff Number
  , nextInt :: Int -> Int -> Aff Int
  , nextRange :: Number -> Number -> Aff Number
  , nextBoolean :: Aff Boolean
  }

-- | Draw a uniform `Number` in `[0, 1)`.
nextNumber :: forall r e. RIO (random :: Random | r) e Number
nextNumber = do
  s <- ask (Proxy :: Proxy "random")
  liftAff s.nextNumber

-- | Draw a uniform `Int` in the closed interval `[low, high]`.
nextInt
  :: forall r e
   . Int
  -> Int
  -> RIO (random :: Random | r) e Int
nextInt low high = do
  s <- ask (Proxy :: Proxy "random")
  liftAff (s.nextInt low high)

-- | Draw a uniform `Number` in the half-open interval
-- | `[min, max)`.
nextRange
  :: forall r e
   . Number
  -> Number
  -> RIO (random :: Random | r) e Number
nextRange min max = do
  s <- ask (Proxy :: Proxy "random")
  liftAff (s.nextRange min max)

-- | Draw a fair `Boolean`.
nextBoolean :: forall r e. RIO (random :: Random | r) e Boolean
nextBoolean = do
  s <- ask (Proxy :: Proxy "random")
  liftAff s.nextBoolean

-- | Pick a uniformly random element from an array. Returns
-- | `Nothing` if the array is empty.
pickRandom
  :: forall r e a
   . Array a
  -> RIO (random :: Random | r) e (Maybe a)
pickRandom xs = case Array.length xs of
  0 -> pure Nothing
  n -> do
    i <- nextInt 0 (n - 1)
    pure (Array.index xs i)

-- | Fisher-Yates shuffle. Each permutation is equally likely if
-- | the underlying `Random` is uniform.
-- |
-- | Implementation note: this is O(n^2) because PureScript arrays
-- | are immutable; the algorithm repeatedly draws a random index
-- | into the remaining tail and splits it out. For the use cases
-- | a shuffle is normally reached for (test fixtures, deck
-- | dealing, small batches) this is fine; do not call on hot paths
-- | over very large arrays.
shuffle
  :: forall r e a
   . Array a
  -> RIO (random :: Random | r) e (Array a)
shuffle = go []
  where
  go acc remaining = case Array.length remaining of
    0 -> pure acc
    n -> do
      i <- nextInt 0 (n - 1)
      let { before, after } = Array.splitAt i remaining
      case Array.uncons after of
        Nothing -> pure (acc <> before)
        Just { head, tail } -> go (Array.snoc acc head) (before <> tail)

-- | Pick an element of an array of `(weight, value)` pairs with
-- | probability proportional to its weight. Returns `Nothing`
-- | when the array is empty or every weight is non-positive.
-- |
-- | Non-positive weights are skipped during selection. If you
-- | pass a mix of positive and non-positive weights only the
-- | positive entries participate; the total against which the
-- | draw is made is the sum of the positive weights.
-- |
-- | Mirrors ZIO `Random.weighted` and Effect-TS
-- | `Random.weighted`.
-- |
-- | ```purescript
-- | -- 70 % "cache", 25 % "primary", 5 % "fallback"
-- | choice <- weighted
-- |   [ Tuple 70.0 "cache"
-- |   , Tuple 25.0 "primary"
-- |   , Tuple  5.0 "fallback"
-- |   ]
-- | ```
weighted
  :: forall r e a
   . Array (Tuple Number a)
  -> RIO (random :: Random | r) e (Maybe a)
weighted pairs =
  let
    total = foldl
      (\acc (Tuple w _) -> if w > 0.0 then acc + w else acc)
      0.0
      pairs
  in
    if total <= 0.0 then pure Nothing
    else do
      draw <- nextRange 0.0 total
      pure (selectAt draw pairs)
  where
  selectAt :: Number -> Array (Tuple Number a) -> Maybe a
  selectAt draw xs = case Array.uncons xs of
    Nothing -> Nothing
    Just { head: Tuple w a, tail } ->
      if w > 0.0 && draw < w then Just a
      else selectAt (draw - (if w > 0.0 then w else 0.0)) tail

-- | A random v4 UUID drawn from the platform CSPRNG (Web Crypto on
-- | modern Node and browsers). The returned string is the canonical
-- | hyphenated form, e.g. `f47ac10b-58cc-4372-a567-0e02b2c3d479`.
-- |
-- | Bypasses the `Random` service: tests that need to override
-- | `nextNumber` / `nextInt` for reproducibility will not affect
-- | `uuid` (Web Crypto has no swap point on the service record).
uuid :: forall r e. RIO r e String
uuid = liftEffect _uuid

-- | A random byte array of length `n` drawn from the platform
-- | CSPRNG. Each element is in `[0, 255]`. Suitable for nonces,
-- | salts, and session tokens. Negative or zero `n` yields an
-- | empty array.
-- |
-- | Bypasses the `Random` service for the same reason as `uuid`.
bytes :: forall r e. Int -> RIO r e (Array Int)
bytes n
  | n <= 0 = pure []
  | otherwise = liftEffect (_bytes n)

-- | A production-ready implementation backed by `Effect.Random`,
-- | which in turn delegates to `Math.random()`. Provide it via
-- | `provide` / `provideAll` or a `Layer`.
liveRandom :: Random
liveRandom =
  { nextNumber: liftEffect Random.random
  , nextInt: \low high -> liftEffect (Random.randomInt low high)
  , nextRange: \min max -> liftEffect (Random.randomRange min max)
  , nextBoolean: liftEffect Random.randomBool
  }
