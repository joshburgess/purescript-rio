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
  , shuffle
  , choice
  , uuid
  , bytes
  , withRandom
  , getRandom
  , setRandom
  ) where

import Prelude

import Data.Array (index, length)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (random, randomInt, randomBool) as R
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, locally, newFiberRef, setFiberRef)

foreign import _uuid :: Effect String
foreign import _bytes :: Int -> Effect (Array Int)
foreign import _shuffleWith
  :: forall a. (Int -> Int -> Effect Int) -> Array a -> Effect (Array a)

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

-- | Return a uniformly-random permutation of the input array using
-- | Fisher-Yates with `nextInt`. Defers to the active `Random`
-- | service, so a `withRandom` override reseeds shuffles too.
shuffle :: forall r e a. Array a -> RIO r e (Array a)
shuffle xs = do
  Random rng <- getRandom
  if length xs <= 1 then pure xs
  else liftEffect (_shuffleWith rng.int xs)

-- | Pick a uniformly-random element from the array. Returns `Nothing`
-- | when the array is empty; otherwise samples a single index via the
-- | active `Random` service.
choice :: forall r e a. Array a -> RIO r e (Maybe a)
choice xs = do
  let n = length xs
  if n == 0 then pure Nothing
  else do
    i <- nextInt 0 (n - 1)
    pure (index xs i)

-- | A random v4 UUID. Uses the platform's CSPRNG (Web Crypto on
-- | modern Node and browsers). The returned string is the canonical
-- | hyphenated form, e.g. `f47ac10b-58cc-4372-a567-0e02b2c3d479`.
uuid :: forall r e. RIO r e String
uuid = liftEffect _uuid

-- | A random byte array of length `n`, drawn from the platform CSPRNG.
-- | Each element is in `[0, 255]`. Suitable for nonces, salts, and
-- | session tokens. Negative or zero `n` yields an empty array.
bytes :: forall r e. Int -> RIO r e (Array Int)
bytes n
  | n <= 0 = pure []
  | otherwise = liftEffect (_bytes n)

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
withRandom rng body = locally randomRef rng body
