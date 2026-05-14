-- | Loads typed configuration from a `.env` file via
-- | `rio-config-file`, demonstrating the full path from source
-- | adapter to a rendered `ConfigError` on the failure path.
-- |
-- | Reads `examples/config-loader/.env` (relative to the project
-- | root, which is the working directory when running via
-- | `npx spago run`). The file ships with every key set so the
-- | success path runs out of the box; comment out a key,
-- | corrupt a value's type, or remove the file to exercise the
-- | failure shapes.
-- |
-- | Run with:
-- |
-- | ```
-- | npx spago run -p rio-example-config-loader
-- | ```
module Example.ConfigLoader.Main where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Type.Proxy (Proxy(..))

import RIO.Config
  ( Config
  , ConfigError
  , Secret
  , boolean
  , int
  , load
  , prettyConfigError
  , secret
  , string
  , withDefault
  )
import RIO.Core (runRIO)
import RIO.Config.File (dotenvFileSource)

type AppConfig =
  { port :: Int
  , dbUrl :: String
  , debug :: Boolean
  , apiKey :: Secret
  }

appConfig :: Config AppConfig
appConfig = { port: _, dbUrl: _, debug: _, apiKey: _ }
  <$> withDefault 8080 (int "PORT")
  <*> string "DATABASE_URL"
  <*> withDefault false (boolean "DEBUG")
  <*> secret "API_KEY"

main :: Effect Unit
main = launchAff_ do
  src <- dotenvFileSource "examples/config-loader/.env"
  -- `load` raises `ConfigError` on the row tag we hand it. The
  -- ambient `e` row at the call site is `(config :: ConfigError)`.
  outcome <- runRIO (load (Proxy :: Proxy "config") src appConfig)
  liftEffect (report outcome)

report :: Either (Variant (config :: ConfigError)) AppConfig -> Effect Unit
report = case _ of
  Right cfg -> printConfig cfg
  Left v -> Console.error
    (prettyConfigError (Variant.case_ # Variant.on (Proxy :: Proxy "config") identity $ v))

printConfig :: AppConfig -> Effect Unit
printConfig cfg = do
  Console.log "Loaded configuration:"
  Console.log ("  PORT         : " <> show cfg.port)
  Console.log ("  DATABASE_URL : " <> cfg.dbUrl)
  Console.log ("  DEBUG        : " <> show cfg.debug)
  -- `Secret`'s Show instance renders `<redacted>`; the value is
  -- only revealed by calling `unSecret`, which is intentionally
  -- noisy in code review.
  Console.log ("  API_KEY      : " <> show cfg.apiKey)
