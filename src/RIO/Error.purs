-- | Typed-error primitives for `RIO`.
-- |
-- | Phase 1.3 ships the raising side (`fail`). Phase 3.1 adds `catchTag`,
-- | which handles one tagged failure and shrinks the error row. Phase 3.2
-- | adds `catchAll` and `mapError`, which replace the row in bulk.
-- | Phase 3.3 adds the defect primitives (`die`, `sandbox`, `unsandbox`),
-- | distinguishing typed failures (in the row) from `Aff` exceptions
-- | (defects).
module RIO.Error
  ( fail
  , rethrow
  , catchTag
  , catchAll
  , mapError
  , die
  , sandbox
  , unsandbox
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (attempt, throwError)
import Effect.Exception (Error)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

import RIO.Internal (RIO(..), unRIO)

-- | Raise a typed failure tagged with the symbol `sym`. The tag and its
-- | payload type are added to the inferred error row, where they can be
-- | caught later by `catchTag` (Phase 3) or surfaced via `runRIO`.
-- |
-- | ```purescript
-- | -- Inferred type:
-- | --   forall r b. RIO r (notFound :: { id :: Int } | _) b
-- | notFound id = fail (Proxy :: _ "notFound") { id }
-- | ```
-- |
-- | The result type is polymorphic in `b` because `fail` never returns a
-- | value; downstream binds inherit the failure unchanged.
fail
  :: forall sym a r e' e b
   . IsSymbol sym
  => Row.Cons sym a e' e
  => Proxy sym
  -> a
  -> RIO r e b
fail sym v = RIO \_ -> pure (Left (Variant.inj sym v))

-- | Fail with an already-constructed `Variant`.
-- |
-- | The dual of `catchAll`: useful inside a `catchAll` handler when you
-- | want to inspect the failure, decide whether to handle or pass it
-- | along, and pass-along means "re-raise it unchanged in the same
-- | row."
-- |
-- | ```purescript
-- | catchAll (\v -> if shouldHandle v then pure fallback else rethrow v) program
-- | ```
rethrow :: forall r e a. Variant e -> RIO r e a
rethrow v = RIO \_ -> pure (Left v)

-- | Catch one tagged failure and remove it from the error row.
-- |
-- | The handler receives the payload that was passed to `fail` and may
-- | itself fail, succeed, or do anything else expressible in `RIO r e'`.
-- | The remaining error row `e'` is `e` with the named tag removed, as
-- | encoded by the `Cons sym a e' e` constraint.
-- |
-- | If the handler can introduce new tags, list them in its return type:
-- |
-- | ```purescript
-- | catchTag (Proxy :: _ "notFound") (\_ -> fail (Proxy :: _ "parse") "x") program
-- | ```
-- |
-- | starts with `(notFound :: _, parse :: _ | r)` and ends with
-- | `(parse :: _ | r)` because `notFound` was removed and `parse` is
-- | reintroduced by the handler. Tags that aren't named in the proxy
-- | pass through unchanged.
catchTag
  :: forall sym a e' e r b
   . IsSymbol sym
  => Row.Cons sym a e' e
  => Proxy sym
  -> (a -> RIO r e' b)
  -> RIO r e b
  -> RIO r e' b
catchTag sym handler inner = RIO \r -> do
  res <- unRIO inner r
  case res of
    Right a -> pure (Right a)
    Left v -> Variant.on sym
      (\payload -> unRIO (handler payload) r)
      (\v' -> pure (Left v'))
      v

-- | Catch every failure with a single handler and replace the error
-- | row with whatever the handler's return type uses.
-- |
-- | The handler runs on the raw `Variant e`. Use it for cross-cutting
-- | strategies (log-and-rethrow, convert all failures to a default value,
-- | translate to a different error scheme). For handling one tag at a
-- | time without touching the others, prefer `catchTag`.
-- |
-- | `catchAll` is the dual of `fail` at the row level: `fail` introduces
-- | a tag, `catchAll` replaces the whole row.
-- |
-- | ```purescript
-- | -- collapse every typed failure into a default value
-- | safeProgram :: RIO r () Int
-- | safeProgram = catchAll (\_ -> pure 0) program
-- | ```
catchAll
  :: forall r e e' a
   . (Variant e -> RIO r e' a)
  -> RIO r e a
  -> RIO r e' a
catchAll handler inner = RIO \r -> do
  res <- unRIO inner r
  case res of
    Right a -> pure (Right a)
    Left v -> unRIO (handler v) r

-- | Transform the failure value by a pure function, replacing the row.
-- |
-- | Equivalent to `catchAll (fail-with-the-new-tag)` but expressed as a
-- | total function on `Variant` so the handler can't introduce new
-- | effects or read services. Useful for error translation at module
-- | boundaries.
-- |
-- | ```purescript
-- | -- relabel every "notFound" into "lookupFailed" without changing
-- | -- the rest of the row
-- | translated :: RIO r (lookupFailed :: Int | other) Todo
-- | translated =
-- |   mapError
-- |     (Variant.on (Proxy :: _ "notFound")
-- |       (\id -> Variant.inj (Proxy :: _ "lookupFailed") id)
-- |       identity)
-- |     program
-- | ```
mapError
  :: forall r e e' a
   . (Variant e -> Variant e')
  -> RIO r e a
  -> RIO r e' a
mapError f inner = RIO \r -> do
  res <- unRIO inner r
  case res of
    Right a -> pure (Right a)
    Left v -> pure (Left (f v))

-- | Raise an `Aff` exception as a defect.
-- |
-- | Defects are unrecoverable in the typed-error sense: they bypass the
-- | error row entirely and surface through the underlying `Aff`'s
-- | exception channel. Use this for programmer errors, invariant
-- | violations, and other "should never happen" cases. For recoverable,
-- | domain-modelled failures, use `fail` instead.
-- |
-- | A defect raised by `die` can be observed (and converted back to a
-- | value) with `sandbox`.
-- |
-- | ```purescript
-- | -- assert an invariant; if violated, raise a defect rather than a
-- | -- typed failure (callers cannot handle it in the row)
-- | checkInvariant :: forall r e. Boolean -> RIO r e Unit
-- | checkInvariant ok =
-- |   if ok then pure unit
-- |   else die (Exception.error "invariant violated")
-- | ```
die :: forall r e a. Error -> RIO r e a
die err = RIO \_ -> throwError err

-- | Reify defects into the success channel.
-- |
-- | The inner program runs to completion. If it succeeds, the result is
-- | `Right a`. If it raises an `Aff` exception (whether from `die` or
-- | from a lifted `Aff` that threw), the exception becomes `Left err`
-- | in the success channel and is no longer a defect from the caller's
-- | perspective. Typed failures in `e` continue to propagate as typed
-- | failures; `sandbox` does **not** absorb them.
-- |
-- | The output's error row is the same `e` as the input's, so typed
-- | failures remain typed and the caller can still handle them with
-- | `catchTag` / `catchAll`.
-- |
-- | ```purescript
-- | -- recover from a defect (e.g. third-party Aff that may throw)
-- | safeFetch :: forall r e. URL -> RIO r e (Either Error Response)
-- | safeFetch url = sandbox (liftAff (Http.fetch url))
-- | ```
sandbox :: forall r e a. RIO r e a -> RIO r e (Either Error a)
sandbox inner = RIO \r -> do
  outcome <- attempt (unRIO inner r)
  case outcome of
    Right (Right a) -> pure (Right (Right a))
    Right (Left typedFail) -> pure (Left typedFail)
    Left defect -> pure (Right (Left defect))

-- | Inverse of `sandbox`.
-- |
-- | Reads the `Either Error a` in the success channel and, if it's a
-- | `Left`, re-raises the error as a defect (via `die`). If it's a
-- | `Right`, threads the value through unchanged. Typed failures in the
-- | input are preserved unchanged.
-- |
-- | ```purescript
-- | -- look at a defect; if it is one we accept, swallow it,
-- | -- otherwise re-raise it unchanged
-- | rethrowUnknown :: forall r e a. RIO r e (Either Error a) -> RIO r e a
-- | rethrowUnknown = unsandbox
-- | ```
unsandbox :: forall r e a. RIO r e (Either Error a) -> RIO r e a
unsandbox inner = RIO \r -> do
  res <- unRIO inner r
  case res of
    Right (Right a) -> pure (Right a)
    Right (Left err) -> throwError err
    Left typedFail -> pure (Left typedFail)
