-- | A refreshable cell, intended for values that can change at
-- | runtime without restarting the process. The primary motivating
-- | case is a `Secret` whose underlying credential rotates on a
-- | schedule, but the type is generic in the cell's payload.
-- |
-- | ```purescript
-- | -- Wire a Config descriptor to a rotating cell that re-reads
-- | -- the source every time `refresh` is called.
-- | program = do
-- |   src <- liftEffect envSource
-- |   Tuple key refresh <- withRotation
-- |     (load (Proxy :: _ "config") src apiKeyConfig)
-- |
-- |   -- handle a SIGHUP or a polling tick by calling refresh:
-- |   _ <- fork (forever (sleep (Milliseconds 300_000.0) *> refresh))
-- |
-- |   -- read the current value:
-- |   k <- readRotating key
-- |   ...
-- | ```
-- |
-- | The cell itself does not impose a rotation policy: it just
-- | gives you read / write primitives and a `withRotation` helper
-- | that wires a loader's result into the cell. Polling, signal
-- | handling, or whatever else is left to the caller.
module RIO.Fiber.Config.Rotating
  ( Rotating
  , newRotating
  , readRotating
  , writeRotating
  , withRotation
  ) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Fiber.Core (RIO, liftEffect)

-- | A thread-safe cell holding a current value. Reads always see
-- | the most recent write; reads and writes are atomic with
-- | respect to each other.
newtype Rotating a = Rotating (Ref a)

-- | Allocate a new cell holding `initial`.
newRotating :: forall a. a -> Effect (Rotating a)
newRotating a = Rotating <$> Ref.new a

-- | Read the cell's current value.
readRotating :: forall r e a. Rotating a -> RIO r e a
readRotating (Rotating ref) = liftEffect (Ref.read ref)

-- | Overwrite the cell with a new value. Subsequent reads see
-- | the new value.
writeRotating :: forall r e a. Rotating a -> a -> RIO r e Unit
writeRotating (Rotating ref) a = liftEffect (Ref.write a ref)

-- | Run `loader` once to populate a fresh cell, and return both
-- | the cell and a `refresh` action that re-runs the loader and
-- | overwrites the cell with the result. Typical use is to call
-- | `refresh` on a timer, a signal, or any other rotation trigger.
-- |
-- | If `loader` fails on a subsequent call, the cell keeps the
-- | last successful value; the failure propagates from `refresh`
-- | on the chosen error row.
withRotation
  :: forall r e a
   . RIO r e a
  -> RIO r e (Tuple (Rotating a) (RIO r e Unit))
withRotation loader = do
  initial <- loader
  cell <- liftEffect (newRotating initial)
  pure (Tuple cell (loader >>= writeRotating cell))
