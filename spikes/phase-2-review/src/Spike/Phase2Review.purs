-- | Phase 2 review cycle: 10 realistic service compositions written
-- | against the real `RIO.Core` API (not the Phase 0.4 spike prototype).
-- |
-- | Every top-level binding here is intentionally unannotated. The
-- | compiler reports each inferred type via a `MissingTypeDeclaration`
-- | warning on a fresh build, and those warnings are reproduced in
-- | `FINDINGS.md`. If a binding fails to type-check without an
-- | annotation, that's a regression and goes in the findings as such.
module Spike.Phase2Review where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))

import RIO.Core (ask, asks, fail, provide, provideAll)

-- ---------------------------------------------------------------------------
-- Fixtures: a handful of plausible services and a couple of error tags.
-- These are *types only* (records of operations); they're never
-- instantiated except as targets for `ask`/`provide`.
-- ---------------------------------------------------------------------------

type Logger =
  { log :: String -> Aff Unit
  }

type Config =
  { host :: String
  , port :: Int
  , greeting :: String
  }

type Database =
  { find :: Int -> Aff (Array String)
  , insert :: String -> Aff Int
  }

type Clock =
  { now :: Aff Int
  }

-- ---------------------------------------------------------------------------
-- Example 1: single `ask`.
--
-- The simplest possible service program: read one service. Inference
-- should pick `r` open and the value type from the service.
-- ---------------------------------------------------------------------------

example1 = ask (Proxy :: Proxy "logger")

-- ---------------------------------------------------------------------------
-- Example 2: `asks` projection.
--
-- One step of work via `asks`. The inner record's row should also be
-- inferred open (any record with at least `greeting`).
-- ---------------------------------------------------------------------------

example2 = asks (Proxy :: Proxy "config") _.greeting

-- ---------------------------------------------------------------------------
-- Example 3: compose three disjoint services in one do-block.
--
-- Pushes one step past the spike's two-service example. The row should
-- inflate to all three labels with the tail open.
-- ---------------------------------------------------------------------------

example3 = do
  logger <- ask (Proxy :: Proxy "logger")
  cfg <- ask (Proxy :: Proxy "config")
  db <- ask (Proxy :: Proxy "database")
  pure { logger, cfg, db }

-- ---------------------------------------------------------------------------
-- Example 4: `fail` with a structured payload.
--
-- A typed failure with a record payload. The error row should be open
-- with `notFound` at the head.
-- ---------------------------------------------------------------------------

example4 = fail (Proxy :: Proxy "notFound") { id: 7, kind: "user" }

-- ---------------------------------------------------------------------------
-- Example 5: `provide` shrinks the service row.
--
-- A two-service program; `provide` discharges one. The leaked `Lacks`
-- constraint from the Phase 0.4 spike's LE-1 should be absent because
-- we deliberately dropped it in Phase 2.2.
-- ---------------------------------------------------------------------------

example5 =
  let
    inner = do
      logger <- ask (Proxy :: Proxy "logger")
      cfg <- ask (Proxy :: Proxy "config")
      pure { logger, cfg }

    fakeLogger :: Logger
    fakeLogger = { log: \_ -> pure unit }
  in
    provide (Proxy :: Proxy "logger") fakeLogger inner

-- ---------------------------------------------------------------------------
-- Example 6: `provideAll` discharges the full row.
--
-- After `provideAll`, the program is runnable (`RIO () e a`). The
-- result type should still be inferred from the body.
-- ---------------------------------------------------------------------------

example6 =
  let
    inner = do
      logger <- ask (Proxy :: Proxy "logger")
      cfg <- ask (Proxy :: Proxy "config")
      liftAff (logger.log "ready")
      pure cfg.greeting
  in
    provideAll
      { logger: { log: \_ -> pure unit } :: Logger
      , config: { host: "h", port: 80, greeting: "hi" } :: Config
      }
      inner

-- ---------------------------------------------------------------------------
-- Example 7: idiomatic service smart constructor.
--
-- The convention from `docs/02-services.md`: ask the service, then
-- `liftAff` the operation. The inferred row should carry `logger`,
-- the inferred result `Unit`.
-- ---------------------------------------------------------------------------

logInfo msg = do
  logger <- ask (Proxy :: Proxy "logger")
  liftAff (logger.log msg)

-- ---------------------------------------------------------------------------
-- Example 8: services + lifts + a typed failure in one program.
--
-- The "kitchen sink" for this phase: two services (`config`, `logger`),
-- a `liftEffect`, a `fail`, and a structured result. Services aggregate;
-- error row picks up `notFound`; value type is the record literal at
-- the end. No annotations anywhere.
-- ---------------------------------------------------------------------------

example8 = do
  cfg <- ask (Proxy :: Proxy "config")
  logger <- ask (Proxy :: Proxy "logger")
  liftEffect (Console.log "starting")
  _ <- fail (Proxy :: Proxy "notFound") { id: 99 }
  liftAff (logger.log "won't reach")
  pure { host: cfg.host, port: cfg.port }

-- ---------------------------------------------------------------------------
-- Example 9: branching keeps the row union.
--
-- A new pattern (not exercised by the Phase 0.4 spike): an `if` whose
-- two branches `ask` different services. The unifying row at the join
-- point should contain both labels, both open.
-- ---------------------------------------------------------------------------

example9 useDb = do
  cfg <- ask (Proxy :: Proxy "config")
  if useDb then do
    db <- ask (Proxy :: Proxy "database")
    rows <- liftAff (db.find cfg.port)
    pure rows
  else do
    logger <- ask (Proxy :: Proxy "logger")
    liftAff (logger.log "skip")
    pure []

-- ---------------------------------------------------------------------------
-- Example 10: a generic helper, reused with different services.
--
-- A reusable helper that takes a getter and uses `asks` to project from
-- whatever service is at the named key. Calling it with two distinct
-- proxies should produce two distinct, unioned row requirements.
-- ---------------------------------------------------------------------------

example10 = do
  greeting <- asks (Proxy :: Proxy "config") _.greeting
  rows <- asks (Proxy :: Proxy "database") _.find
  one <- liftAff (rows 1)
  pure { greeting, one }
