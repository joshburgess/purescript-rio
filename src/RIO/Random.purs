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
  , liveRandom
  ) where

import Prelude

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
