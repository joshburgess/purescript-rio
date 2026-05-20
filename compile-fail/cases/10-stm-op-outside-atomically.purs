-- Case: an STM operation (`readTRef`) is bound directly inside a `RIO`
-- do-block, with no `atomically` around it.
--
-- `readTRef :: TRef a -> STM e a`. Inside a `RIO`-typed do-block the
-- bind expects `RIO`, not `STM`, so the call must be rejected. The
-- fix is to wrap the STM transaction in `atomically`, which lifts
-- `STM e a` to `RIO r e a` once and consistently.
module Scratch where

import Prelude

import Effect.Aff (Aff)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (newTRef, readTRef, atomically)

-- `readTRef ref` is `STM e Int`, not `RIO () () Int`. Binding it
-- directly inside the `RIO` do-block must not typecheck. (Replacing
-- the body with `atomically (newTRef 0 >>= readTRef)` would compile.)
program :: RIO () () Int
program = do
  ref <- atomically (newTRef 0)
  readTRef ref

result :: Aff Int
result = runRIO' program
