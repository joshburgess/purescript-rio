module Spike.RowInference.Examples where

-- Every binding in this module is intentionally written WITHOUT an explicit
-- type signature. The point of the spike is to see what the compiler can
-- infer on its own. If you find yourself wanting to add a signature to make
-- something compile, that finding goes in FINDINGS.md.

import Prelude

import Data.Variant (Variant)
import Effect.Class.Console as Console
import Spike.RowInference.Prototype (RIO, ask, asks, catchTag, fail, liftAff, provide)
import Type.Proxy (Proxy(..))

-- Service "types" are just records of operations. Defined here so we have
-- nominal types to reference; not under inspection by the spike.

type Logger =
  { info :: String -> RIO () () Unit
  , warn :: String -> RIO () () Unit
  }

type Database =
  { fetch :: Int -> RIO () () String
  , save :: String -> RIO () () Unit
  }

type Config =
  { greeting :: String
  }

-- Reusable proxies. (We avoid `_logger = Proxy :: Proxy "logger"` style at
-- this stage because that requires an annotation; using Proxy inline tests
-- what works without one.)

-- =============================================================================
-- 1. Simple `ask`: read one service.
-- Expected: inferred row contains a single label.
example1 = ask (Proxy :: Proxy "logger")

-- =============================================================================
-- 2. Two reads from the same service.
-- Expected: row still has a single label; no duplication.
example2 = do
  log1 <- ask (Proxy :: Proxy "logger")
  log2 <- ask (Proxy :: Proxy "logger")
  pure { log1, log2 }

-- =============================================================================
-- 3. Compose two effects requiring DISJOINT services.
-- Expected: row contains both labels, inferred automatically. This is the
-- canonical row-union test.
example3 = do
  logger <- ask (Proxy :: Proxy "logger")
  db <- ask (Proxy :: Proxy "database")
  pure { logger, db }

-- =============================================================================
-- 4. `asks` with a field selector.
-- Expected: row contains the service; result type is the projected field type.
example4 = asks (Proxy :: Proxy "config") _.greeting

-- =============================================================================
-- 5. `provide` one service into an effect that needed two.
-- Expected: after provide, the resulting row no longer contains "logger".
example5 =
  let
    needsTwo = do
      _ <- ask (Proxy :: Proxy "logger")
      _ <- ask (Proxy :: Proxy "database")
      pure unit
    fakeLogger =
      { info: \_ -> liftAff (pure unit) :: RIO () () Unit
      , warn: \_ -> liftAff (pure unit) :: RIO () () Unit
      }
  in
    provide (Proxy :: Proxy "logger") fakeLogger needsTwo

-- =============================================================================
-- 6. `fail` with a single error tag.
-- Expected: the error row contains exactly that tag.
example6 = fail (Proxy :: Proxy "notFound") { id: 42 }

-- =============================================================================
-- 7. `catchTag` removes one error from the row.
-- Expected: starts with `(notFound :: _, parse :: _)`, after catch is `(parse :: _)`.
example7 =
  let
    program = do
      _ <- fail (Proxy :: Proxy "notFound") { id: 99 }
      fail (Proxy :: Proxy "parse") "bad json"
  in
    catchTag (Proxy :: Proxy "notFound") (\_ -> pure "fallback") program

-- =============================================================================
-- 8. Compose two effects with DISJOINT error tags.
-- Expected: error row is the union of both tags.
example8 = do
  _ <- fail (Proxy :: Proxy "notFound") { id: 1 }
  fail (Proxy :: Proxy "parse") "oops"

-- =============================================================================
-- 9. Kitchen sink: services + errors composed together.
-- Expected: both service row and error row carry the union.
example9 = do
  logger <- ask (Proxy :: Proxy "logger")
  _ <- liftAff (Console.log "hi")
  _ <- fail (Proxy :: Proxy "notFound") { id: 7 }
  db <- ask (Proxy :: Proxy "database")
  pure { logger, db }

-- =============================================================================
-- 10. Layer-like: build a value from one service that is then "providable"
-- as another. Tests whether the inference works across a `provide` boundary
-- when the inner program uses the same service the layer reads from.
example10 =
  let
    buildGreeter = do
      cfg <- ask (Proxy :: Proxy "config")
      pure (\name -> cfg.greeting <> ", " <> name <> "!")
    program greeter = do
      _ <- liftAff (Console.log (greeter "world"))
      pure unit
  in
    do
      greeter <- buildGreeter
      program greeter

-- =============================================================================
-- Helper: forces every example above to be evaluated as an expression and
-- ensures none of them have escaped to runtime via dead-code elimination
-- during compilation. (PureScript will refuse to compile examples that don't
-- type-check, so referencing them here gates the build on inference.)

allExamples =
  { e1: example1 :: RIO _ _ _
  , e2: example2 :: RIO _ _ _
  , e3: example3 :: RIO _ _ _
  , e4: example4 :: RIO _ _ _
  , e5: example5 :: RIO _ _ _
  , e6: example6 :: RIO _ _ _
  , e7: example7 :: RIO _ _ _
  , e8: example8 :: RIO _ _ _
  , e9: example9 :: RIO _ _ _
  , e10: example10 :: RIO _ _ _
  }

-- Suppress an unused-import warning for the Variant import on some compiler
-- versions; the type is implicit in the inferred error rows above.
_variantPin :: forall e. Variant e -> Variant e
_variantPin = identity
