-- | Phase 7 review: service interfaces for the demo app.
-- |
-- | Four services. `Clock` is the production service from
-- | `RIO.Aff.Clock`, re-exported here so consumers of this spike see
-- | the full surface in one place. The rest follow the
-- | `docs/02-services.md` convention: records of `Aff`-valued
-- | operations.
module Spike.Phase7Review.Services
  ( Database
  , Logger
  , UserService
  , module RIO.Aff.Clock
  ) where

import Prelude (Unit)

import Data.Maybe (Maybe)
import Effect.Aff (Aff, Milliseconds)

import RIO.Aff.Clock (Clock)

type Logger =
  { log :: String -> Aff Unit
  }

type Database =
  { fetchUser :: Int -> Aff (Maybe String)
  }

-- | Two operations. `greet` is straight-line; `greetAfter` sleeps
-- | through the `Clock` service before greeting, which is the
-- | hook the `RIO.Aff.Test.Clock` scenario drives.
type UserService =
  { greet :: Int -> Aff String
  , greetAfter :: Milliseconds -> Int -> Aff String
  }
