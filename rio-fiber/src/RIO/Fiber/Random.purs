-- | A swappable randomness service.
-- |
-- | Production code reads randomness via `nextNumber` / `nextInt` /
-- | `nextBoolean` instead of calling `Effect.Random` directly. Tests
-- | can swap the implementation with `withRandom` to get a
-- | deterministic sequence (e.g. a `Ref Int` returning a canned
-- | array of values), turning a flaky property test into a
-- | repeatable one.
-- |
-- | The default implementation delegates to `Effect.Random`. The
-- | current implementation lives in a module-level `FiberRef`, so a
-- | `withRandom` block scopes the override to the wrapped action
-- | and any child fibers forked from inside it.
module RIO.Fiber.Random
  ( Random(..)
  , defaultRandom
  , nextNumber
  , nextInt
  , nextBoolean
  , withRandom
  , getRandom
  , setRandom
  ) where

import Prelude

import Effect (Effect)
import Effect.Random (random, randomInt, randomBool) as R
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, ensuring, liftEffect)
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, newFiberRef, setFiberRef)

-- | A randomness implementation. Tests can substitute a
-- | deterministic one (e.g. backed by a seeded PRNG or a canned
-- | array).
newtype Random = Random
  { number :: Effect Number
  , int :: Int -> Int -> Effect Int
  , boolean :: Effect Boolean
  }

-- | The default randomness service: delegates to `Effect.Random`.
defaultRandom :: Random
defaultRandom = Random
  { number: R.random
  , int: R.randomInt
  , boolean: R.randomBool
  }

randomRef :: FiberRef Random
randomRef = unsafePerformEffect (newFiberRef defaultRandom)

-- | A random `Number` in `[0, 1)` from the active service.
nextNumber :: forall r e. RIO r e Number
nextNumber = do
  Random rng <- getFiberRef randomRef
  liftEffect rng.number

-- | A random `Int` uniformly in the closed interval `[lo, hi]`.
nextInt :: forall r e. Int -> Int -> RIO r e Int
nextInt lo hi = do
  Random rng <- getFiberRef randomRef
  liftEffect (rng.int lo hi)

-- | A random `Boolean` from the active service.
nextBoolean :: forall r e. RIO r e Boolean
nextBoolean = do
  Random rng <- getFiberRef randomRef
  liftEffect rng.boolean

-- | Read the active randomness implementation.
getRandom :: forall r e. RIO r e Random
getRandom = getFiberRef randomRef

-- | Replace the active randomness implementation for the current
-- | fiber and its descendants.
setRandom :: forall r e. Random -> RIO r e Unit
setRandom = setFiberRef randomRef

-- | Run `body` with `rng` as the active randomness service,
-- | restoring the previous implementation on exit.
withRandom :: forall r e a. Random -> RIO r e a -> RIO r e a
withRandom rng body = do
  prev <- getRandom
  setRandom rng
  ensuring (setRandom prev) body
