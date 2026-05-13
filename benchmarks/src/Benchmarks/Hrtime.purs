-- | A nanosecond-resolution clock read via `process.hrtime()`. Used by
-- | the benchmark harness; the production `RIO.Clock` keeps its
-- | millisecond resolution because that's what real applications need.
module Benchmarks.Hrtime
  ( hrtimeNs
  ) where

import Effect (Effect)

foreign import hrtimeNs :: Effect Number
