module Example.Logger.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console

import Example.Logger (Logger, consoleLogger, err, info, warn)
import RIO.Aff.Core (RIO, provideAll, runRIO)

program :: forall e. RIO (logger :: Logger) e Unit
program = do
  info "starting up"
  warn "this is a warning"
  err "and an error, for variety"
  info "done"

main :: Effect Unit
main = launchAff_ do
  result <- runRIO (provideAll { logger: consoleLogger } program)
  liftEffect case result of
    Right _ -> Console.log "example: ok"
    Left _ -> Console.log "example: failed"
