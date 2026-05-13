module Benchmarks.Exit (exit) where

import Prelude

import Effect (Effect)

foreign import exitImpl :: Int -> Effect Unit

exit :: Int -> Effect Unit
exit = exitImpl
