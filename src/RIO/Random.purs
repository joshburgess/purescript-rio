-- | A `Random` service plus a live implementation.
-- |
-- | Mirrors `RIO.Clock` / `RIO.Test.Clock`: the live implementation
-- | delegates to `Effect.Random`, the test implementation lives in
-- | `RIO.Test.Random` and gives you a seedable, deterministic
-- | sequence for replayable tests.
-- |
-- | The service exposes three operations: a uniform `Number` in
-- | `[0, 1)`, a uniform `Int` in a closed range, and a fair
-- | `Boolean`. Range checks (high less than low) are not
-- | enforced; the result is unspecified in that case, matching
-- | `Effect.Random.randomInt`.
-- |
-- | ```purescript
-- | rollDie :: forall r e. RIO (random :: Random | r) e Int
-- | rollDie = nextInt 1 6
-- | ```
module RIO.Random
  ( Random
  , nextBoolean
  , nextInt
  , nextNumber
  , nextRange
  , pickRandom
  , shuffle
  , liveRandom
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (random, randomBool, randomInt, randomRange) as Random
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask)

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
