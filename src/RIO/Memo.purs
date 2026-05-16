-- | Single-shot memoization for an `RIO` action.
-- |
-- | `memoize program` returns a new program that, when run for the
-- | first time, runs `program` and caches its outcome. Every
-- | subsequent invocation observes the cached outcome without
-- | re-running the underlying action.
-- |
-- | This is the per-action counterpart of `RIO.Cache`, which keys on
-- | an input. Reach for `memoize` when an action takes no key but
-- | you want "run once, return the same answer thereafter" - e.g.
-- | loading immutable configuration, opening a long-lived
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
-- | Both typed failures and defects are cached. A program that
-- | fails on the first call will fail with the same payload on
-- | every subsequent call - the underlying action does not run a
-- | second time to "try again". If retry semantics are required,
-- | wrap the action in a `RIO.Schedule` retry loop before
-- | memoizing, not after.
-- |
-- | ```purescript
-- | program = do
-- |   getConfig <- memoize loadConfig   -- prepare the cell
-- |   c1 <- getConfig                   -- runs loadConfig once
-- |   c2 <- getConfig                   -- returns the same result
-- |   useConfig c1 c2
-- | ```
module RIO.Memo
  ( memoize
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.AVar (empty) as AVarEff
import Effect.Aff (attempt, throwError)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (read, tryPut) as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Data.Variant (Variant)

import RIO.Internal (RIO(..), rioFail, unRIO)

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
memoize action = RIO \_ -> do
  cell <- liftEffect (Ref.new Nothing)
  pure (memoCell action cell)

memoCell
  :: forall r e a
   . RIO r e a
  -> Ref.Ref (Maybe (AVar (Either String (Either (Variant e) a))))
  -> RIO r e a
memoCell action cell = RIO \r -> do
  decision <- liftEffect do
    existing <- Ref.read cell
    case existing of
      Just avar -> pure (Awaiter avar)
      Nothing -> do
        avar <- AVarEff.empty
        Ref.write (Just avar) cell
        pure (Owner avar)
  case decision of
    Awaiter avar -> do
      result <- AVar.read avar
      case result of
        Right (Right a) -> pure a
        Right (Left v) -> rioFail v
        Left msg -> throwError (error msg)
    Owner avar -> do
      attempted <- attempt (unRIO action r)
      case attempted of
        Right outcome -> do
          _ <- AVar.tryPut (Right outcome) avar
          case outcome of
            Right a -> pure a
            Left v -> rioFail v
        Left err -> do
          _ <- AVar.tryPut (Left (show err)) avar
          throwError err

data Decision e a
  = Awaiter (AVar (Either String (Either (Variant e) a)))
  | Owner (AVar (Either String (Either (Variant e) a)))
