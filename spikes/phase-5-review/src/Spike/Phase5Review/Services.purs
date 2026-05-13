-- | Phase 5 review: service interfaces for the demo app.
-- |
-- | Each service is a record of `Aff`-valued operations, matching the
-- | convention from `docs/02-services.md`. Layers (in `Layers.purs`)
-- | produce these records, optionally registering finalizers in the
-- | provided `scope` service.
module Spike.Phase5Review.Services
  ( Cache
  , Clock
  , Config
  , Database
  , Logger
  , UserService
  ) where

import Prelude (Unit)

import Data.Maybe (Maybe)
import Effect.Aff (Aff)

type Config =
  { databaseUrl :: String
  , cacheCap :: Int
  }

type Logger =
  { log :: String -> Aff Unit
  }

type Clock =
  { now :: Aff Int
  }

type Cache =
  { get :: String -> Aff (Maybe String)
  , put :: String -> String -> Aff Unit
  }

type Database =
  { fetchUser :: Int -> Aff (Maybe String)
  }

type UserService =
  { greet :: Int -> Aff String
  }
