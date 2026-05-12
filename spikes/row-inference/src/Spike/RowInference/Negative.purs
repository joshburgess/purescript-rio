module Spike.RowInference.Negative where

-- Each of the bindings below is commented out. To inspect the compiler's
-- error message for a given mistake, uncomment exactly one and run
-- `npx spago build -p spike-row-inference`. Findings are recorded in
-- FINDINGS.md.

-- Imports below are referenced only by the (currently commented) negative
-- cases. Uncomment them along with the case you want to reproduce.
--
-- import Prelude
-- import Effect.Aff (Aff)
-- import Spike.RowInference.Prototype (RIO, ask, catchTag, fail, provide, runRIO)
-- import Type.Proxy (Proxy(..))

-- --------------------------------------------------------------------------
-- NEG-1: providing a service of the wrong type
-- --------------------------------------------------------------------------
-- neg1 =
--   provide (Proxy :: Proxy "logger") (42 :: Int)
--     (ask (Proxy :: Proxy "logger") :: RIO ( logger :: String ) () String)

-- --------------------------------------------------------------------------
-- NEG-2: catching a tag that doesn't exist in the error row
-- --------------------------------------------------------------------------
-- neg2 =
--   let
--     program = fail (Proxy :: Proxy "parse") "oops"
--   in
--     catchTag (Proxy :: Proxy "notFound") (\_ -> pure "fallback")
--       (program :: RIO () ( parse :: String ) String)

-- --------------------------------------------------------------------------
-- NEG-3: running a program before all services are provided
-- --------------------------------------------------------------------------
-- neg3 :: Aff _
-- neg3 = runRIO (ask (Proxy :: Proxy "logger"))

-- Placeholder so the module isn't empty.
_pin :: Int
_pin = 0
