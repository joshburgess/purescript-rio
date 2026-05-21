-- | A seedable, deterministic `Random` for tests.
-- |
-- | `newTestRandom seed` allocates a `Random` whose draws are
-- | produced by a linear congruential generator (LCG) over a
-- | 31-bit state, kept in a `Ref`. The generator is intentionally
-- | simple: fast, reproducible across machines, good enough for
-- | unit tests; it is not suitable for cryptography or
-- | property-based statistical testing.
-- |
-- | A test that asserts on randomized behaviour holds the whole
-- | `TestRandom` record, passes `random` to the program under
-- | test, and uses `setSeed` to reset to a known state between
-- | assertions if needed.
-- |
-- | ```purescript
-- | itRIO "returns the same number under the same seed" do
-- |   tr <- liftEffect (newTestRandom 42)
-- |   _ <- withRandom tr.random nextNumber
-- |   liftEffect (tr.setSeed 42)
-- |   ...
-- | ```
module RIO.Fiber.Test.Random
  ( TestRandom
  , newTestRandom
  ) where

import Prelude

import Data.Int (floor, toNumber)
import Data.Number (floor) as Number
import Effect (Effect)
import Effect.Ref as Ref

import RIO.Fiber.Random (Random(..))

-- | A `Random` paired with the controller used to reset its
-- | internal state. Tests typically hold the whole record.
type TestRandom =
  { random :: Random
  , setSeed :: Int -> Effect Unit
  }

-- | Allocate a fresh test random with the given starting seed.
newTestRandom :: Int -> Effect TestRandom
newTestRandom seed0 = do
  stateRef <- Ref.new (normalize (toNumber seed0))
  let
    advance :: Effect Number
    advance = do
      s <- Ref.read stateRef
      let s' = step s
      Ref.write s' stateRef
      pure s'

    -- LCG output is in [0, modulus); dividing by modulus puts it
    -- in [0, 1).
    nextNumber :: Effect Number
    nextNumber = do
      n <- advance
      pure (n / modulus)

    nextInt :: Int -> Int -> Effect Int
    nextInt low high = do
      n <- advance
      let
        span = high - low + 1
        scaled = (n / modulus) * toNumber span
      pure (low + floor scaled)

    nextBoolean :: Effect Boolean
    nextBoolean = do
      n <- advance
      pure (n / modulus < 0.5)

    setSeed :: Int -> Effect Unit
    setSeed s = Ref.write (normalize (toNumber s)) stateRef

  pure
    { random: Random
        { number: nextNumber
        , int: nextInt
        , boolean: nextBoolean
        }
    , setSeed
    }

-- LCG constants (Numerical Recipes). State and arithmetic live in
-- `Number` to avoid the 32-bit wraparound that flooring through
-- `Int` would introduce. The multiplier-times-state product fits
-- in `Number` exactly because `1664525 * (2^31 - 1)` is well
-- below `2^53`.

multiplier :: Number
multiplier = 1664525.0

increment :: Number
increment = 1013904223.0

modulus :: Number
modulus = 2147483647.0

-- | Fold a seed into `[0, modulus)`. Negative seeds wrap; positive
-- | seeds above the modulus get reduced.
normalize :: Number -> Number
normalize s =
  let
    m = s - modulus * Number.floor (s / modulus)
  in
    if m < 0.0 then m + modulus else m

-- | One LCG step: `s' = (a * s + c) mod m`.
step :: Number -> Number
step s =
  let
    product = multiplier * s + increment
  in
    product - modulus * Number.floor (product / modulus)
