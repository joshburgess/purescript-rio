-- | Error-handling combinators for `RIO.Fiber`.
-- |
-- | `fail`, `catchAll`, `die`, `failCause`, and `causeOf` are the
-- | primitives and live in `RIO.Fiber.Core`. This module layers the
-- | usual derived combinators on top: `catchTag` to handle one tag,
-- | `mapError` / `rethrow` for re-routing, `tapError` / `tapBoth`
-- | for telemetry, `orElse*` and `option` for fallbacks,
-- | `refineOrDie*` for boundary contracts, plus cause-level
-- | combinators (`catchAllCause`, `catchSomeCause`, `foldCauseRIO`,
-- | `attemptCause`, `tapErrorCause`, `tapDefectCause`) that see the
-- | full structured `Cause`.
module RIO.Fiber.Error
  ( absolve
  , attemptCause
  , catchAllCause
  , catchSome
  , catchSomeCause
  , catchTag
  , class CatchableErrorTag
  , class FindErrorTag
  , class FindErrorTagInRow
  , either
  , foldCauseRIO
  , foldRIO
  , matchCause
  , matchCauseRIO
  , fromEither
  , fromMaybe
  , mapBoth
  , mapError
  , option
  , orDie
  , orElse
  , orElseFail
  , orElseSucceed
  , refineOrDie
  , refineOrDieWith
  , rethrow
  , tap
  , tapBoth
  , tapDefectCause
  , tapError
  , tapErrorCause
  , unsandbox
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception (Error)
import Effect.Exception (error) as Exception
import Prim.Row (class Cons) as Row
import Prim.RowList (class RowToList, RowList) as RL
import Prim.RowList (Cons, Nil) as RLP
import Prim.TypeError (class Fail, Above, Beside, Text)
import Type.Proxy (Proxy)

import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core
  ( RIO
  , catchAll
  , causeOf
  , die
  , fail
  , failCause
  )

-- | Internal helper that walks an `e`-row's `RowList` to look up a
-- | tag symbol. The funcdep `sym l -> a` means the payload type is
-- | determined by the symbol and the row list, so when a user's
-- | `catchTag` handler claims a payload type different from what
-- | the row carries, the conflict surfaces as a clean "could not
-- | match" at the handler argument rather than as a `Prim.Row.Cons`
-- | row-mismatch.
-- |
-- | The `Nil` instance carries a `Fail` constraint that fires when
-- | the tag is genuinely absent from the row, producing a friendlier
-- | message than the default `Cons` constraint error.
class FindErrorTag (sym :: Symbol) (l :: RL.RowList Type) (a :: Type) | sym l -> a

instance findErrorTagFound :: FindErrorTag sym (RLP.Cons sym a tail) a
else instance findErrorTagRecur ::
  FindErrorTag sym tail a =>
  FindErrorTag sym (RLP.Cons k v tail) a
else instance findErrorTagMissing ::
  Fail
    ( Above
        ( Beside
            (Beside (Text "RIO.Fiber.catchTag: the error tag '") (Text sym))
            (Text "' is not present in the error row.")
        )
        ( Text
            "Check the tag name (case-sensitive) and the program's error type."
        )
    ) =>
  FindErrorTag sym RLP.Nil a

-- | Bridge between `FindErrorTag` (which operates on a `RowList`)
-- | and the row-shaped constraint we want to expose on `catchTag`.
class FindErrorTagInRow (sym :: Symbol) (e :: Row Type) (a :: Type) | sym e -> a

instance findErrorTagInRow ::
  ( RL.RowToList e l
  , FindErrorTag sym l a
  ) =>
  FindErrorTagInRow sym e a

-- | A `catchTag`-flavoured constraint that bundles the row-list
-- | lookup of the tag's payload type. The funcdep `sym e -> a` lets
-- | the compiler determine the handler's payload type from the
-- | program's error row, so a wrong-typed handler surfaces a single
-- | clean "could not match" error rather than a row-mismatch
-- | message naming the whole row.
class CatchableErrorTag (sym :: Symbol) (a :: Type) (e :: Row Type) | sym e -> a

instance catchableErrorTag ::
  FindErrorTagInRow sym e a =>
  CatchableErrorTag sym a e

-- | Fail with an already-constructed `Variant`.
-- |
-- | The dual of `catchAll`: useful inside a `catchAll` handler when
-- | you want to inspect the failure, decide whether to handle or
-- | pass it along, and pass-along means "re-raise it unchanged in
-- | the same row."
rethrow :: forall r e a. Variant e -> RIO r e a
rethrow = fail

-- | Catch one tagged failure and remove it from the error row.
-- |
-- | The handler receives the payload that was passed to `fail` and
-- | may itself fail, succeed, or do anything else expressible in
-- | `RIO r e'`. The remaining error row `e'` is `e` with the named
-- | tag removed, as encoded by the `Cons sym a e' e` constraint.
catchTag
  :: forall sym a e' e r b
   . IsSymbol sym
  => CatchableErrorTag sym a e
  => Row.Cons sym a e' e
  => Proxy sym
  -> (a -> RIO r e' b)
  -> RIO r e b
  -> RIO r e' b
catchTag sym handler = catchAll (Variant.on sym handler fail)

-- | Catch some failures, leave the rest. The classifier decides per
-- | failure whether to handle it (`Just (handler …)` recovers) or
-- | pass it through unchanged (`Nothing` re-raises on the same row).
-- |
-- | Unlike `catchAll`, the error row is preserved: handled failures
-- | are discharged but unhandled ones keep flowing through `e`.
-- | Unlike `catchTag`, the classifier can look at any tag (or any
-- | combination of tags) at once rather than naming one.
catchSome
  :: forall r e a
   . (Variant e -> Maybe (RIO r e a))
  -> RIO r e a
  -> RIO r e a
catchSome classify =
  catchAll
    ( \v -> case classify v of
        Just handler -> handler
        Nothing -> rethrow v
    )

-- | Transform the failure value by a pure function, replacing the
-- | row. Equivalent to `catchAll (fail-with-the-new-tag)` but
-- | expressed as a total function on `Variant` so the handler can't
-- | introduce new effects or read services. Useful for error
-- | translation at module boundaries.
mapError
  :: forall r e e' a
   . (Variant e -> Variant e')
  -> RIO r e a
  -> RIO r e' a
mapError f = catchAll (\v -> fail (f v))

-- | Run a side-effecting action on the success value and pass the
-- | value through unchanged. If `inner` fails, the failure
-- | propagates without running `f`. If `f` itself fails or raises a
-- | defect, that failure takes over.
tap :: forall r e a. (a -> RIO r e Unit) -> RIO r e a -> RIO r e a
tap f inner = do
  a <- inner
  f a
  pure a

-- | Run a side-effecting action on a typed failure and re-raise the
-- | failure unchanged. The handler sees the full row's `Variant`
-- | and runs in the same row, so it can read services and perform
-- | effectful work before the failure continues upward. If the
-- | handler itself fails, *that* failure replaces the original.
tapError
  :: forall r e a
   . (Variant e -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapError f = catchAll (\v -> f v *> rethrow v)

-- | Fire one of two side-effecting handlers depending on whether
-- | the inner action succeeded or raised a typed failure, then
-- | re-emit the original outcome unchanged. The two-arm sibling of
-- | `tap` / `tapError`.
tapBoth
  :: forall r e a
   . (Variant e -> RIO r e Unit)
  -> (a -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapBoth onErr onOk inner =
  catchAll
    (\v -> onErr v *> rethrow v)
    (inner >>= \a -> onOk a *> pure a)

-- | Lift a pure `Either (Variant e) a` into `RIO`. `Left` becomes a
-- | typed failure on the row; `Right` becomes a success.
fromEither :: forall r e a. Either (Variant e) a -> RIO r e a
fromEither (Right a) = pure a
fromEither (Left v) = fail v

-- | Lift a pure `Maybe a` into `RIO`. `Nothing` becomes the supplied
-- | typed failure; `Just` becomes a success.
fromMaybe :: forall r e a. Variant e -> Maybe a -> RIO r e a
fromMaybe _ (Just a) = pure a
fromMaybe v Nothing = fail v

-- | Reflect a typed failure into the success channel as `Left`. A
-- | success becomes `Right a`; a typed failure becomes
-- | `Left (Variant e)` while the error row collapses to whatever
-- | the caller fixes it at (commonly `()`).
-- |
-- | Defects and interrupts continue to propagate; this only
-- | reifies the typed-error row.
either
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' (Either (Variant e) a)
either inner =
  catchAll (\v -> pure (Left v)) (map Right inner)

-- | Collapse a `RIO r e (Either (Variant e) a)` into `RIO r e a` by
-- | turning a `Left v` in the success channel into a typed failure
-- | on the row.
absolve :: forall r e a. RIO r e (Either (Variant e) a) -> RIO r e a
absolve inner = do
  result <- inner
  case result of
    Right a -> pure a
    Left v -> rethrow v

-- | Handle both arms of an `RIO` in one combinator: transform a
-- | typed failure via `onError` and a success via `onSuccess`, with
-- | both branches returning the same result type. The error row may
-- | change. Defects still propagate as defects.
foldRIO
  :: forall r e e' a b
   . (Variant e -> RIO r e' b)
  -> (a -> RIO r e' b)
  -> RIO r e a
  -> RIO r e' b
foldRIO onError onSuccess inner = do
  result <- either inner
  case result of
    Right a -> onSuccess a
    Left v -> onError v

-- | Try the first action; if it fails with a typed error, run the
-- | fallback and use its result. The first action's error row is
-- | discarded; only the fallback's row is observable.
orElse :: forall r e e' a. RIO r e a -> RIO r e' a -> RIO r e' a
orElse first fallback = catchAll (\_ -> fallback) first

-- | Replace any typed failure with a pure success value. The error
-- | row is discharged. Defects still propagate.
orElseSucceed :: forall r e e' a. a -> RIO r e a -> RIO r e' a
orElseSucceed a = catchAll (\_ -> pure a)

-- | Replace any typed failure with a different typed failure. The
-- | original payload is discarded; the new failure sits on a
-- | replacement row chosen by the caller.
orElseFail
  :: forall r e e' a
   . Variant e'
  -> RIO r e a
  -> RIO r e' a
orElseFail v = catchAll (\_ -> rethrow v)

-- | Reflect a fallible action into the success channel as `Maybe a`:
-- | `Just a` on success, `Nothing` on any typed failure. The error
-- | row collapses to whatever the caller fixes it at (usually `()`).
option :: forall r e e' a. RIO r e a -> RIO r e' (Maybe a)
option inner = catchAll (\_ -> pure Nothing) (map Just inner)

-- | Convert a typed failure into a defect via a user-supplied
-- | translator. The error row is discharged on the resulting
-- | action; any failure that occurs becomes a `Die` cause.
orDie
  :: forall r e e' a
   . (Variant e -> Error)
  -> RIO r e a
  -> RIO r e' a
orDie toErr = catchAll (\v -> die (toErr v))

-- | Narrow the error row, defecting any failure that does not fit
-- | the new row. The classifier decides which failures belong on
-- | the narrower row `e'` (return `Just`) and which should be raised
-- | as defects (return `Nothing`).
refineOrDie
  :: forall r e e' a
   . (Variant e -> Maybe (Variant e'))
  -> RIO r e a
  -> RIO r e' a
refineOrDie classify =
  refineOrDieWith classify
    (\_ -> Exception.error "RIO.Fiber.refineOrDie: unrefined failure")

-- | Like `refineOrDie`, but the caller supplies the defect's `Error`
-- | per leftover failure.
refineOrDieWith
  :: forall r e e' a
   . (Variant e -> Maybe (Variant e'))
  -> (Variant e -> Error)
  -> RIO r e a
  -> RIO r e' a
refineOrDieWith classify toErr =
  catchAll
    ( \v -> case classify v of
        Just v' -> rethrow v'
        Nothing -> die (toErr v)
    )

-- | Map both arms of an `RIO`: transform the typed failure with
-- | `onError` and the success value with `onSuccess`, replacing the
-- | error row in the process. The bimap for `RIO`.
mapBoth
  :: forall r e e' a b
   . (Variant e -> Variant e')
  -> (a -> b)
  -> RIO r e a
  -> RIO r e' b
mapBoth onError onSuccess inner =
  mapError onError (map onSuccess inner)

-- | Run the inner action and capture its failure outcome as a
-- | `Cause`. Successes pass through; defects, typed failures, and
-- | interrupts each surface as the corresponding `Cause` leaf.
-- |
-- | Sugar for `causeOf`; the row variable on the outer `RIO` is
-- | discharged so it can be embedded anywhere.
attemptCause :: forall r e e' a. RIO r e a -> RIO r e' (Either (Cause e) a)
attemptCause = causeOf

-- | Inverse of `causeOf` / `attemptCause`. Collapses a sandboxed
-- | outcome back onto the fiber's failure channels: `Right a`
-- | becomes a plain success, `Left cause` becomes a `failCause`
-- | that replays the original structure (typed failure, defect,
-- | interrupt, or composite). The action's outer row picks up the
-- | inner cause's failure row.
-- |
-- | This is the pair to `causeOf`: anything you captured with
-- | `causeOf` can be re-raised verbatim with `unsandbox`, so the
-- | combinator pair is a round trip in either order.
unsandbox :: forall r e a. RIO r () (Either (Cause e) a) -> RIO r e a
unsandbox inner = do
  result <- catchAll Variant.case_ inner
  case result of
    Right a -> pure a
    Left cause -> failCause cause

-- | Handle every failure cause with a recovery action that sees the
-- | full structured `Cause`. The handler runs on the original row's
-- | failures and recovers into a fresh row `e'`.
catchAllCause
  :: forall r e e' a
   . (Cause e -> RIO r e' a)
  -> RIO r e a
  -> RIO r e' a
catchAllCause handler inner = do
  result <- causeOf inner
  case result of
    Right a -> pure a
    Left cause -> handler cause

-- | Catch some failures with cause visibility. The classifier sees
-- | the full `Cause` and decides per failure whether to handle it
-- | (`Just (handler …)` recovers) or let it propagate unchanged
-- | (`Nothing` re-raises the original cause via `failCause`).
catchSomeCause
  :: forall r e a
   . (Cause e -> Maybe (RIO r e a))
  -> RIO r e a
  -> RIO r e a
catchSomeCause classify inner = do
  result <- causeOf inner
  case result of
    Right a -> pure a
    Left cause -> case classify cause of
      Just handler -> handler
      Nothing -> failCause cause

-- | Handle both arms with cause visibility on the failure arm.
-- | Mirrors `foldRIO` but lets the failure handler see the full
-- | structured `Cause` rather than just a single `Variant e`.
foldCauseRIO
  :: forall r e e' a b
   . (Cause e -> RIO r e' b)
  -> (a -> RIO r e' b)
  -> RIO r e a
  -> RIO r e' b
foldCauseRIO onCause onSuccess inner = do
  result <- causeOf inner
  case result of
    Right a -> onSuccess a
    Left cause -> onCause cause

-- | Match on a computation's outcome with pure handlers, discharging
-- | the typed-error row. Like `foldCauseRIO`, but the handlers
-- | produce a pure value rather than another `RIO`. Useful for
-- | summarising an outcome into a result type at the boundary of a
-- | subsystem.
matchCause
  :: forall r e e' a b
   . (Cause e -> b)
  -> (a -> b)
  -> RIO r e a
  -> RIO r e' b
matchCause onCause onSuccess inner = do
  result <- causeOf inner
  pure case result of
    Right a -> onSuccess a
    Left cause -> onCause cause

-- | Canonical name for `foldCauseRIO`. Same semantics: pattern-match
-- | on success vs. failure with the full `Cause` visible, deferring
-- | the result back into `RIO`.
matchCauseRIO
  :: forall r e e' a b
   . (Cause e -> RIO r e' b)
  -> (a -> RIO r e' b)
  -> RIO r e a
  -> RIO r e' b
matchCauseRIO = foldCauseRIO

-- | Run a side-effecting handler on the failure cause and re-raise
-- | the cause unchanged. The handler sees the full `Cause`, so it
-- | can dispatch on defects, typed failures, interrupts, or
-- | composite causes.
tapErrorCause
  :: forall r e a
   . (Cause e -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapErrorCause f =
  catchAllCause (\c -> f c *> failCause c)

-- | Run a side-effecting handler whenever the cause contains a
-- | defect leaf, then re-raise the cause unchanged. Typed failures
-- | and pure-interrupt causes pass through without calling the
-- | handler.
tapDefectCause
  :: forall r e a
   . (Cause e -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapDefectCause f =
  catchAllCause \c ->
    if Cause.hasDefect c then f c *> failCause c
    else failCause c
