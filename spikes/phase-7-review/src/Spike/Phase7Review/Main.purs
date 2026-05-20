-- | Entry point for the Phase 7 review spec suite.
-- |
-- | Run with `npx spago run -p spike-phase-7-review`. Exits
-- | non-zero if any of the four spec scenarios fails, matching
-- | the contract of the other workspace-package smoke tests.
module Spike.Phase7Review.Main (main) where

import Prelude

import Effect (Effect)

import RIO.Aff.Spec (runSpecRIO)

import Spike.Phase7Review.Spec (spec)

main :: Effect Unit
main = runSpecRIO spec
