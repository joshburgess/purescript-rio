-- | A one-shot, write-once cell that fiber `A` can `await` on and
-- | fiber `B` can complete with a `succeed` or `fail`. Building
-- | block for higher-level concurrency: handshakes, promise-style
-- | "the worker is ready" signals, and STM-free coordination.
-- |
-- | Implemented over `Effect.Aff.AVar` so multiple `await`ers all
-- | wake when the cell is filled; once filled it stays filled (the
-- | `succeed` / `fail` family use `tryPut`, returning `False` on a
-- | second attempt rather than overwriting).
-- |
-- | `Deferred e a` carries the same `Either (Variant e) a` shape as
-- | a fiber's result, so `succeedDeferred` produces a value on the
-- | awaiter's success path and `failDeferred` surfaces a typed
-- | failure on the awaiter's `e` row.
module RIO.Deferred
  ( Deferred
  , awaitDeferred
  , failDeferred
  , makeDeferred
  , pollDeferred
  , succeedDeferred
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect.AVar (AVarStatus(..))
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar

import RIO.Internal (RIO(..), mkRIO, rioFail)

-- | A write-once cell that carries either a typed failure in row
-- | `e` or a value of type `a`.
-- |
-- | The constructor is hidden; use `makeDeferred` to create one,
-- | `succeedDeferred` / `failDeferred` to fill it, and
-- | `awaitDeferred` to read it (blocking until filled).
newtype Deferred :: Row Type -> Type -> Type
newtype Deferred e a = Deferred (AVar (Either (Variant e) a))

-- | Create an empty `Deferred`. The result is infallible from the
-- | caller's perspective; the error row on the creator side is
-- | left free so `makeDeferred` composes inside a do-block with
-- | any error row.
-- |
-- | ```purescript
-- | -- worker signals readiness through a Deferred
-- | program = do
-- |   ready <- makeDeferred
-- |   _ <- fork (initWorker *> succeedDeferred ready unit)
-- |   awaitDeferred ready
-- |   useWorker
-- | ```
makeDeferred :: forall r e' e a. RIO r e' (Deferred e a)
makeDeferred = mkRIO \_ -> do
  avar <- AVar.empty
  pure (Deferred avar)

-- | Fill the cell with a success. Returns `True` if this call
-- | filled it, `False` if it was already filled. Infallible from
-- | the caller's row, so the parent's `e` row stays open.
-- |
-- | ```purescript
-- | -- the first worker to find the answer wins
-- | _ <- fork (search1 *> succeedDeferred answer "from-1")
-- | _ <- fork (search2 *> succeedDeferred answer "from-2")
-- | winner <- awaitDeferred answer
-- | ```
succeedDeferred
  :: forall r e' e a
   . Deferred e a
  -> a
  -> RIO r e' Boolean
succeedDeferred (Deferred avar) a = mkRIO \_ -> do
  AVar.tryPut (Right a) avar

-- | Fill the cell with a typed failure. Returns `True` if this
-- | call filled it, `False` if it was already filled.
-- |
-- | ```purescript
-- | -- worker that may fail; awaiters see the failure on their row
-- | failDeferred d (Variant.inj (Proxy :: _ "timeout") unit)
-- | ```
failDeferred
  :: forall r e' e a
   . Deferred e a
  -> Variant e
  -> RIO r e' Boolean
failDeferred (Deferred avar) v = mkRIO \_ -> do
  AVar.tryPut (Left v) avar

-- | Wait for the cell to be filled and surface its result.
-- |
-- | If `succeedDeferred` filled it, returns the value on the
-- | success channel. If `failDeferred` filled it, raises the
-- | typed failure on row `e`. Reading is non-destructive, so
-- | multiple awaiters all see the same result.
-- |
-- | ```purescript
-- | -- block until the worker hands back its result
-- | result <- awaitDeferred answer
-- | ```
awaitDeferred :: forall r e a. Deferred e a -> RIO r e a
awaitDeferred (Deferred avar) = mkRIO \_ -> do
  result <- AVar.read avar
  case result of
    Right a -> pure a
    Left v -> rioFail v

-- | Non-blocking probe: returns `Nothing` if the cell is empty,
-- | `Just (Left v)` if filled with a typed failure, `Just (Right a)`
-- | if filled with a success.
-- |
-- | Useful for "did the worker finish yet?" checks; for the
-- | blocking version reach for `awaitDeferred`.
-- |
-- | ```purescript
-- | -- poll before falling back to a synchronous path
-- | check <- pollDeferred d
-- | case check of
-- |   Just (Right a) -> useFast a
-- |   _ -> useSlow
-- | ```
pollDeferred
  :: forall r e' e a
   . Deferred e a
  -> RIO r e' (Maybe (Either (Variant e) a))
pollDeferred (Deferred avar) = mkRIO \_ -> do
  s <- AVar.status avar
  pure case s of
    Filled a -> Just a
    _ -> Nothing
