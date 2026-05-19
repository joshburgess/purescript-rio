-- | Test-side helpers: re-export the rio-fiber Aff bridge under the
-- | name the older specs use, so spec bodies can `await` an outcome
-- | with normal `do`.
module Test.RIO.Fiber.Helpers
  ( module Aff
  ) where

import RIO.Fiber.Aff (runAff) as Aff
