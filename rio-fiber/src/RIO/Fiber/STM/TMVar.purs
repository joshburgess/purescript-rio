-- | A one-slot transactional cell.
-- |
-- | A `TMVar a` is either empty or holds a single value. `take`
-- | blocks (retries) when empty; `put` blocks when full. Built on
-- | `TVar (Maybe a)` so it composes with `orElse` and the rest of
-- | STM.
module RIO.Fiber.STM.TMVar
  ( TMVar
  , newEmpty
  , new
  , take
  , tryTake
  , put
  , tryPut
  , read
  , tryRead
  , isEmpty
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A one-slot transactional cell.
newtype TMVar a = TMVar (TVar (Maybe a))

-- | A fresh empty TMVar.
newEmpty :: forall a. Effect (TMVar a)
newEmpty = TMVar <$> STM.newTVar Nothing

-- | A TMVar pre-filled with `a`.
new :: forall a. a -> Effect (TMVar a)
new a = TMVar <$> STM.newTVar (Just a)

-- | Take the value, leaving the cell empty. Retries when empty.
take :: forall a. TMVar a -> STM a
take (TMVar tv) = do
  m <- STM.readTVar tv
  case m of
    Nothing -> STM.retry
    Just a -> do
      STM.writeTVar tv Nothing
      pure a

-- | Try to take the value without retrying. `Nothing` if empty.
tryTake :: forall a. TMVar a -> STM (Maybe a)
tryTake (TMVar tv) = do
  m <- STM.readTVar tv
  case m of
    Nothing -> pure Nothing
    Just a -> do
      STM.writeTVar tv Nothing
      pure (Just a)

-- | Put the value into the cell. Retries when full.
put :: forall a. TMVar a -> a -> STM Unit
put (TMVar tv) a = do
  m <- STM.readTVar tv
  case m of
    Just _ -> STM.retry
    Nothing -> STM.writeTVar tv (Just a)

-- | Try to put the value without retrying. Returns `false` if the
-- | cell is full.
tryPut :: forall a. TMVar a -> a -> STM Boolean
tryPut (TMVar tv) a = do
  m <- STM.readTVar tv
  case m of
    Just _ -> pure false
    Nothing -> do
      STM.writeTVar tv (Just a)
      pure true

-- | Look at the value without consuming it. Retries when empty.
read :: forall a. TMVar a -> STM a
read (TMVar tv) = do
  m <- STM.readTVar tv
  case m of
    Nothing -> STM.retry
    Just a -> pure a

-- | Look at the value without consuming or retrying.
tryRead :: forall a. TMVar a -> STM (Maybe a)
tryRead (TMVar tv) = STM.readTVar tv

-- | Predicate: is the cell currently empty?
isEmpty :: forall a. TMVar a -> STM Boolean
isEmpty (TMVar tv) = do
  m <- STM.readTVar tv
  pure case m of
    Nothing -> true
    Just _ -> false
