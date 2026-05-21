-- | A one-slot transactional cell.
-- |
-- | A `TMVar a` is either empty or holds a single value. `takeTMVar`
-- | retries when empty; `putTMVar` retries when full. Built on
-- | `TVar (Maybe a)` so it composes with `orElse` and the rest of
-- | STM.
module RIO.Aff.STM.TMVar
  ( TMVar
  , isEmptyTMVar
  , newEmptyTMVar
  , newTMVar
  , putTMVar
  , readTMVar
  , takeTMVar
  , tryPutTMVar
  , tryReadTMVar
  , tryTakeTMVar
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import RIO.Aff.STM (STM, TVar, newTVar, readTVar, retry, writeTVar)

-- | A one-slot transactional cell.
newtype TMVar a = TMVar (TVar (Maybe a))

-- | A fresh empty TMVar.
newEmptyTMVar :: forall e a. STM e (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

-- | A TMVar pre-filled with `a`.
newTMVar :: forall e a. a -> STM e (TMVar a)
newTMVar a = TMVar <$> newTVar (Just a)

-- | Take the value, leaving the cell empty. Retries when empty.
takeTMVar :: forall e a. TMVar a -> STM e a
takeTMVar (TMVar tv) = do
  m <- readTVar tv
  case m of
    Nothing -> retry
    Just a -> do
      writeTVar tv Nothing
      pure a

-- | Try to take the value without retrying. `Nothing` if empty.
tryTakeTMVar :: forall e a. TMVar a -> STM e (Maybe a)
tryTakeTMVar (TMVar tv) = do
  m <- readTVar tv
  case m of
    Nothing -> pure Nothing
    Just a -> do
      writeTVar tv Nothing
      pure (Just a)

-- | Put the value into the cell. Retries when full.
putTMVar :: forall e a. TMVar a -> a -> STM e Unit
putTMVar (TMVar tv) a = do
  m <- readTVar tv
  case m of
    Just _ -> retry
    Nothing -> writeTVar tv (Just a)

-- | Try to put the value without retrying. Returns `false` if the
-- | cell is full.
tryPutTMVar :: forall e a. TMVar a -> a -> STM e Boolean
tryPutTMVar (TMVar tv) a = do
  m <- readTVar tv
  case m of
    Just _ -> pure false
    Nothing -> do
      writeTVar tv (Just a)
      pure true

-- | Look at the value without consuming it. Retries when empty.
readTMVar :: forall e a. TMVar a -> STM e a
readTMVar (TMVar tv) = do
  m <- readTVar tv
  case m of
    Nothing -> retry
    Just a -> pure a

-- | Look at the value without consuming or retrying.
tryReadTMVar :: forall e a. TMVar a -> STM e (Maybe a)
tryReadTMVar (TMVar tv) = readTVar tv

-- | Predicate: is the cell currently empty?
isEmptyTMVar :: forall e a. TMVar a -> STM e Boolean
isEmptyTMVar (TMVar tv) = do
  m <- readTVar tv
  pure case m of
    Nothing -> true
    Just _ -> false
