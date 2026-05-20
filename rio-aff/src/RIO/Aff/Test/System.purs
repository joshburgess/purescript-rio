-- | A mutable `System` for tests.
-- |
-- | `newTestSystem` allocates a `System` whose `lookupEnv`,
-- | `getArgs`, and `getCwd` read from `Ref`s seeded with the
-- | values you pass in. The same record exposes setter / mutator
-- | functions so tests can mutate the simulated process state
-- | between calls.
-- |
-- | ```purescript
-- | itRIO "reads HOME when it is set" do
-- |   ts <- liftAff (newTestSystem
-- |     { env: Map.singleton "HOME" "/tmp"
-- |     , args: []
-- |     , cwd: "/"
-- |     })
-- |   home <- runWith ts.system (System.lookupEnv "HOME")
-- |   home `shouldEqual` Just "/tmp"
-- |   liftAff (ts.setEnv "HOME" "/elsewhere")
-- |   ...
-- | ```
module RIO.Aff.Test.System
  ( TestSystem
  , TestSystemConfig
  , newTestSystem
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Aff.System (System)

-- | The initial state seeded into a fresh test system.
type TestSystemConfig =
  { env :: Map String String
  , args :: Array String
  , cwd :: String
  }

-- | A `System` paired with the controls used to mutate its
-- | underlying state.
-- |
-- |   * `system` is the service to provide to the program under
-- |     test.
-- |   * `setEnv` / `unsetEnv` mutate the simulated env map.
-- |   * `setArgs` replaces the entire argument vector.
-- |   * `setCwd` replaces the current working directory.
type TestSystem =
  { system :: System
  , setEnv :: String -> String -> Aff Unit
  , unsetEnv :: String -> Aff Unit
  , setArgs :: Array String -> Aff Unit
  , setCwd :: String -> Aff Unit
  }

-- | Allocate a fresh test system. The body is pure `Effect`
-- | work; the `Aff` wrapper lets tests `bind` it alongside the
-- | rest of their setup.
newTestSystem :: TestSystemConfig -> Aff TestSystem
newTestSystem cfg = liftEffect do
  envRef <- Ref.new cfg.env
  argsRef <- Ref.new cfg.args
  cwdRef <- Ref.new cfg.cwd
  let
    lookupEnv :: String -> Aff (Maybe String)
    lookupEnv k = liftEffect (Map.lookup k <$> Ref.read envRef)

    getArgs :: Aff (Array String)
    getArgs = liftEffect (Ref.read argsRef)

    getCwd :: Aff String
    getCwd = liftEffect (Ref.read cwdRef)

    setEnv :: String -> String -> Aff Unit
    setEnv k v = liftEffect (Ref.modify_ (Map.insert k v) envRef)

    unsetEnv :: String -> Aff Unit
    unsetEnv k = liftEffect (Ref.modify_ (Map.delete k) envRef)

    setArgs :: Array String -> Aff Unit
    setArgs xs = liftEffect (Ref.write xs argsRef)

    setCwd :: String -> Aff Unit
    setCwd s = liftEffect (Ref.write s cwdRef)

  pure
    { system: { lookupEnv, getArgs, getCwd }
    , setEnv
    , unsetEnv
    , setArgs
    , setCwd
    }
