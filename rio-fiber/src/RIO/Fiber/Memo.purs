-- | Single-shot memoization for an `RIO` action.
-- |
-- | `memoize program` returns a new program that, when run for the
-- | first time, runs `program` and caches its outcome. Every
-- | subsequent invocation observes the cached outcome without
-- | re-running the underlying action.
-- |
-- | This is the per-action counterpart of `RIO.Fiber.Cache`, which
-- | keys on an input. Reach for `memoize` when an action takes no
-- | key but you want "run once, return the same answer thereafter":
-- | e.g. loading immutable configuration, opening a long-lived
-- | connection, or computing a derived value that's expensive to
-- | produce.
-- |
-- | ## Single-flight
-- |
-- | If two fibers invoke the memoized action concurrently before it
-- | has completed, only one runs the underlying program; the other
-- | awaits and observes the same outcome. This prevents the
-- | thundering-herd pattern where N concurrent first-calls each
-- | trigger an independent expensive computation.
-- |
-- | ## Failure caching
-- |
-- | The structured `Cause` of any non-success outcome is captured
-- | and replayed on every subsequent call. A typed-failure leaf is
-- | replayed via `fail` so callers' `catchAll` machinery sees it;
-- | any other shape (defect, interrupt, composite) is replayed via
-- | `failCause`. The underlying action does not run a second time
-- | to "try again". If retry semantics are required, wrap the
-- | action in a `RIO.Fiber.Schedule` retry loop before memoizing,
-- | not after.
-- |
-- | ```purescript
-- | program = do
-- |   getConfig <- memoize loadConfig   -- prepare the cell
-- |   c1 <- getConfig                   -- runs loadConfig once
-- |   c2 <- getConfig                   -- returns the same result
-- |   useConfig c1 c2
-- | ```
module RIO.Fiber.Memo
  ( memoize
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref

import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core (RIO, causeOf, ensuring, fail, failCause, liftEffect)
import RIO.Fiber.Deferred (Deferred)
import RIO.Fiber.Deferred as Deferred
import RIO.Fiber.FiberId as FiberId

-- | Wrap an action so it runs at most once. The outer `RIO`
-- | prepares the memo cell; the returned inner `RIO` is the
-- | memoized action.
-- |
-- | The outer error row `e'` is left free because preparing the
-- | cell never raises a typed failure on its own.
memoize
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' (RIO r e a)
memoize action = liftEffect do
  cell <- Ref.new Nothing
  pure (memoCell action cell)

memoCell
  :: forall r e a
   . RIO r e a
  -> Ref.Ref (Maybe (Deferred () (Either (Cause e) a)))
  -> RIO r e a
memoCell action cell = do
  decision <- liftEffect do
    existing <- Ref.read cell
    case existing of
      Just d -> pure (Awaiter d)
      Nothing -> do
        d <- Deferred.make
        Ref.write (Just d) cell
        pure (Owner d)
  case decision of
    Awaiter d -> Deferred.awaitPure d >>= reproduce
    Owner d ->
      ensuring
        -- If the owner is interrupted before causeOf returns, fill
        -- the cell with an Interrupt cause so awaiters do not block
        -- forever. Idempotent: a no-op once the cell is already set.
        (void (Deferred.succeed d (Left (Cause.interrupt FiberId.externalFiberId))))
        ( do
            outcome <- causeOf action
            _ <- Deferred.succeed d outcome
            reproduce outcome
        )

reproduce :: forall r e a. Either (Cause e) a -> RIO r e a
reproduce = case _ of
  Right a -> pure a
  Left cause -> case Cause.firstFailure cause of
    Just v -> fail v
    Nothing -> failCause cause

data Decision e a
  = Awaiter (Deferred () (Either (Cause e) a))
  | Owner (Deferred () (Either (Cause e) a))
