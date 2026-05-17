-- Case: a program that asks for a service from its env row is handed
-- directly to `runRIO'`, which fixes the env row at `()`.
--
-- The inner program asks for `logger :: Logger`. `runRIO'` accepts
-- only `RIO () () a`, so the open service requirement must be
-- discharged first (via `provide` or `provideAll`). The compiler
-- must reject the call. Case 02 covers the same shape for the error
-- row; this case covers the env row.
module Scratch where

import Prelude

import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask, runRIO')

type Logger = { name :: String }

inner :: forall e. RIO (logger :: Logger) e String
inner = do
  logger <- ask (Proxy :: Proxy "logger")
  pure logger.name

-- `runRIO'` requires `RIO () () a`. The `logger` requirement is
-- still on the env row, so the call must not typecheck.
result :: Aff String
result = runRIO' inner
