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
-- |   ts <- liftEffect (newTestSystem
-- |     { env: Map.singleton "HOME" "/tmp"
-- |     , args: []
-- |     , cwd: "/"
-- |     })
-- |   home <- provideAt (Proxy :: Proxy "system") ts.system
-- |     (System.lookupEnv "HOME")
-- |   home `shouldEqual` Just "/tmp"
-- |   liftEffect (ts.setEnv "HOME" "/elsewhere")
-- |   ...
-- | ```
module RIO.Fiber.Test.System
  ( TestSystem
  , TestSystemConfig
  , newTestSystem
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref as Ref

import RIO.Fiber.System (System)

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
  , setEnv :: String -> String -> Effect Unit
  , unsetEnv :: String -> Effect Unit
  , setArgs :: Array String -> Effect Unit
  , setCwd :: String -> Effect Unit
  }

-- | Allocate a fresh test system.
newTestSystem :: TestSystemConfig -> Effect TestSystem
newTestSystem cfg = do
  envRef <- Ref.new cfg.env
  argsRef <- Ref.new cfg.args
  cwdRef <- Ref.new cfg.cwd
  let
    lookupEnv :: String -> Effect (Maybe String)
    lookupEnv k = Map.lookup k <$> Ref.read envRef

    getArgs :: Effect (Array String)
    getArgs = Ref.read argsRef

    getCwd :: Effect String
    getCwd = Ref.read cwdRef

    setEnv :: String -> String -> Effect Unit
    setEnv k v = Ref.modify_ (Map.insert k v) envRef

    unsetEnv :: String -> Effect Unit
    unsetEnv k = Ref.modify_ (Map.delete k) envRef

    setArgs :: Array String -> Effect Unit
    setArgs xs = Ref.write xs argsRef

    setCwd :: String -> Effect Unit
    setCwd s = Ref.write s cwdRef

  pure
    { system: { lookupEnv, getArgs, getCwd }
    , setEnv
    , unsetEnv
    , setArgs
    , setCwd
    }
