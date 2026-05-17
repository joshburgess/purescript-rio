-- | Typed-error primitives for `RIO`.
-- |
-- | Phase 1.3 ships the raising side (`fail`). Phase 3.1 adds `catchTag`,
-- | which handles one tagged failure and shrinks the error row. Phase 3.2
-- | adds `catchAll` and `mapError`, which replace the row in bulk.
-- | Phase 3.3 adds the defect primitives (`die`, `sandbox`, `unsandbox`),
-- | distinguishing typed failures (in the row) from `Aff` exceptions
-- | (defects).
module RIO.Error
  ( absolve
  , catchAll
  , catchSome
  , catchTag
  , class CatchableErrorTag
  , class FindErrorTag
  , class FindErrorTagInRow
  , die
  , either
  , fail
  , foldRIO
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
  , sandbox
  , tap
  , tapBoth
  , tapDefect
  , tapError
  , unsandbox
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (attempt, throwError)
import Effect.Exception (Error)
import Effect.Exception (error) as Exception
import Prim.Row (class Cons) as Row
import Prim.RowList (class RowToList, RowList) as RL
import Prim.RowList (Cons, Nil) as RLP
import Prim.TypeError (class Fail, Above, Beside, Text)
import Type.Proxy (Proxy)

import RIO.Internal
  ( RIO(..)
  , instrCatchAll
  , instrCatchTag
  , instrFail
  , matchTypedFailure
  , mkRIO
  , rioFail
  , unsafeUnRIO
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
            (Beside (Text "RIO.catchTag: the error tag '") (Text sym))
            (Text "' is not present in the error row.")
        )
        ( Text
            "Check the tag name (case-sensitive) and the program's error type."
        )
    ) =>
  FindErrorTag sym RLP.Nil a

-- | Bridge between `FindErrorTag` (which operates on a `RowList`)
-- | and the row-shaped constraint we want to expose on `catchTag`.
-- | The funcdep `sym e -> a` carries the lookup through.
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
fail sym v = RIO (instrFail (Variant.inj sym v))

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
rethrow v = RIO (instrFail v)

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
  => CatchableErrorTag sym a e
  => Row.Cons sym a e' e
  => Proxy sym
  -> (a -> RIO r e' b)
  -> RIO r e b
  -> RIO r e' b
catchTag sym handler (RIO innerI) =
  RIO
    ( instrCatchTag
        (reflectSymbol sym)
        (\payload -> case handler payload of RIO i -> i)
        innerI
    )

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
catchAll handler (RIO innerI) =
  RIO
    ( instrCatchAll
        (\v -> case handler v of RIO i -> i)
        innerI
    )

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
mapError f (RIO innerI) =
  RIO (instrCatchAll (\v -> instrFail (f v)) innerI)

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
die err = mkRIO \_ -> throwError err

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
sandbox inner = mkRIO \r -> do
  outcome <- attempt (unsafeUnRIO inner r)
  case outcome of
    Right a -> pure (Right a)
    Left err -> case matchTypedFailure err of
      Just v -> rioFail v
      Nothing -> pure (Left err)

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
unsandbox inner = do
  val <- inner
  case val of
    Right a -> pure a
    Left err -> die err

-- | Run a side-effecting action on the success value and pass the
-- | value through unchanged.
-- |
-- | If `inner` fails, the failure propagates without running `f`.
-- | If `f` itself fails or raises a defect, that failure takes
-- | over (the original value never reaches downstream).
-- |
-- | ```purescript
-- | result <- tap (\record -> logInfo ("loaded " <> record.id)) loadRecord
-- | ```
tap :: forall r e a. (a -> RIO r e Unit) -> RIO r e a -> RIO r e a
tap f inner = do
  a <- inner
  f a
  pure a

-- | Run a side-effecting action on a typed failure and re-raise the
-- | failure unchanged.
-- |
-- | The handler sees the full row's `Variant` and runs in the same
-- | row, so it can read services and perform effectful work (e.g.
-- | logging a metric, emitting a structured log line) before the
-- | failure continues upward. If the handler itself fails, *that*
-- | failure replaces the original.
-- |
-- | ```purescript
-- | runQuery
-- |   # tapError (\v -> incrementCounter "query.failure")
-- | ```
tapError
  :: forall r e a
   . (Variant e -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapError f inner =
  catchAll (\v -> f v >>= \_ -> rethrow v) inner

-- | Fire one of two side-effecting handlers depending on whether the
-- | inner action succeeded or raised a typed failure, then re-emit
-- | the original outcome unchanged. The two-arm sibling of `tap` /
-- | `tapError`.
-- |
-- | Mirrors ZIO `ZIO.tapBoth` / Effect `Effect.tapBoth`. Useful for
-- | structured telemetry that wants to record every outcome on a
-- | single call site without caring which arm fired.
-- |
-- | A handler that fails replaces the original outcome (same policy
-- | as `tap` and `tapError`). Defects bypass both handlers and
-- | propagate via `Aff`'s exception channel.
-- |
-- | ```purescript
-- | runJob
-- |   # tapBoth
-- |       (\v -> incrementCounter "job.failure")
-- |       (\_ -> incrementCounter "job.success")
-- | ```
tapBoth
  :: forall r e a
   . (Variant e -> RIO r e Unit)
  -> (a -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapBoth onErr onOk inner =
  catchAll
    (\v -> onErr v >>= \_ -> rethrow v)
    (inner >>= \a -> onOk a >>= \_ -> pure a)

-- | Fire a handler when the inner action raises a defect (an `Aff`
-- | exception, whether from `die`, a lifted `Aff`, or a runtime
-- | panic), then re-raise the defect unchanged. Typed failures and
-- | successes pass through without calling the handler.
-- |
-- | Mirrors ZIO `ZIO.tapDefect`. Pair with structured logging or a
-- | metrics emitter to surface "this should never happen" failures
-- | without converting them into typed errors.
-- |
-- | A handler that itself raises a defect replaces the original; a
-- | handler that fails with a typed error is *not* possible because
-- | the handler runs in the same error row `e` as the inner program
-- | but its typed failures cannot intercept a defect (the original
-- | defect always wins).
-- |
-- | ```purescript
-- | safeFetch url
-- |   # tapDefect (\err -> logError ("uncaught: " <> message err))
-- | ```
tapDefect
  :: forall r e a
   . (Error -> RIO r e Unit)
  -> RIO r e a
  -> RIO r e a
tapDefect f inner = mkRIO \r -> do
  attempted <- attempt (unsafeUnRIO inner r)
  case attempted of
    Right a -> pure a
    Left err -> case matchTypedFailure err of
      Just _ -> throwError err
      Nothing -> do
        _ <- attempt (unsafeUnRIO (f err) r)
        throwError err

-- | Lift a pure `Either (Variant e) a` into `RIO`. `Left` becomes a
-- | typed failure on the row; `Right` becomes a success.
-- |
-- | The dual of `either`: where `either` reflects a typed failure
-- | into the success channel, `fromEither` lifts a pure result back
-- | into the row.
fromEither :: forall r e a. Either (Variant e) a -> RIO r e a
fromEither (Right a) = mkRIO \_ -> pure a
fromEither (Left v) = mkRIO \_ -> rioFail v

-- | Lift a pure `Maybe a` into `RIO`. `Nothing` becomes the supplied
-- | typed failure; `Just` becomes a success.
-- |
-- | ```purescript
-- | userId <- fromMaybe
-- |   (Variant.inj (Proxy :: Proxy "notFound") "user")
-- |   (Map.lookup "alice" users)
-- | ```
fromMaybe :: forall r e a. Variant e -> Maybe a -> RIO r e a
fromMaybe _ (Just a) = mkRIO \_ -> pure a
fromMaybe v Nothing = mkRIO \_ -> rioFail v

-- | Reflect a typed failure into the success channel as `Left`. A
-- | success becomes `Right a`; a typed failure becomes
-- | `Left (Variant e)` while the error row collapses to whatever
-- | the caller fixes it at (commonly `()`).
-- |
-- | Defects (`die` / `Aff` exceptions) continue to propagate; this
-- | only reifies the typed-error row. Use `sandbox` when you also
-- | want to reify defects.
-- |
-- | ```purescript
-- | -- handle the failure locally as a value
-- | outcome <- either runQuery
-- | case outcome of
-- |   Right rows -> ...
-- |   Left v -> logFailure v
-- | ```
either
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' (Either (Variant e) a)
either inner =
  catchAll (\v -> pure (Left v)) (map Right inner)

-- | Collapse a `RIO r e (Either (Variant e2) a)` into
-- | `RIO r e a` by turning a `Left v` in the success channel into a
-- | typed failure on the row. The caller's row must already contain
-- | the tags from `e2` for the result to typecheck.
-- |
-- | This is the inverse of `either`: combine a wrapped Either back
-- | into the typed-error row.
-- |
-- | ```purescript
-- | -- run a sub-program that returned its failure as a value
-- | -- and surface it back on the parent's row
-- | absolve (either subprogram)
-- | ```
absolve :: forall r e a. RIO r e (Either (Variant e) a) -> RIO r e a
absolve inner = do
  result <- inner
  case result of
    Right a -> pure a
    Left v -> rethrow v

-- | Handle both arms of an `RIO` in one combinator: transform a
-- | typed failure via `onError` and a success via `onSuccess`, with
-- | both branches returning the same result type. The error row may
-- | change.
-- |
-- | This is `catchAll` and `>>=` rolled into one. Equivalent to ZIO
-- | `foldM` and Effect-TS `Effect.matchEffect`. Defects still
-- | propagate as defects.
-- |
-- | ```purescript
-- | -- decide how to react to either arm of a fallible computation
-- | foldRIO
-- |   (\err -> logFailure err *> pure fallback)
-- |   (\value -> finalize value)
-- |   runQuery
-- | ```
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
-- | discarded - only the fallback's row is observable.
-- |
-- | Defects still propagate; `orElse` only routes around typed
-- | failures.
-- |
-- | ```purescript
-- | -- read from the cache, falling back to the source on miss
-- | record <- fromCache key `orElse` fromSource key
-- | ```
orElse :: forall r e e' a. RIO r e a -> RIO r e' a -> RIO r e' a
orElse first fallback = catchAll (\_ -> fallback) first

-- | Catch some failures, leave the rest. The classifier decides per
-- | failure whether to handle it (`Just (handler …)` recovers) or
-- | pass it through unchanged (`Nothing` re-raises on the same row).
-- |
-- | Unlike `catchAll`, the error row is preserved: handled failures
-- | are discharged but unhandled ones keep flowing through `e`.
-- | Unlike `catchTag`, the classifier can look at any tag (or any
-- | combination of tags) at once rather than naming one.
-- |
-- | Mirrors ZIO `ZIO.catchSome` / Effect-TS `Effect.catchSome`.
-- |
-- | ```purescript
-- | -- recover from "notFound" with a default; let "parseError" surface
-- | safeLookup =
-- |   catchSome
-- |     (Variant.on (Proxy :: _ "notFound")
-- |       (\_ -> Just (pure defaultItem))
-- |       (\_ -> Nothing))
-- |     loadItem
-- | ```
catchSome
  :: forall r e a
   . (Variant e -> Maybe (RIO r e a))
  -> RIO r e a
  -> RIO r e a
catchSome classify inner =
  catchAll
    ( \v -> case classify v of
        Just handler -> handler
        Nothing -> rethrow v
    )
    inner

-- | Replace any typed failure with a pure success value. The error
-- | row is discharged. Defects still propagate.
-- |
-- | Mirrors ZIO `ZIO.orElseSucceed`. Sugar for
-- | `catchAll (\_ -> pure a)`.
-- |
-- | ```purescript
-- | -- treat any cache failure as a miss
-- | record <- orElseSucceed defaultRecord (fromCache key)
-- | ```
orElseSucceed :: forall r e e' a. a -> RIO r e a -> RIO r e' a
orElseSucceed a = catchAll (\_ -> pure a)

-- | Replace any typed failure with a different typed failure. The
-- | original payload is discarded; the new failure sits on a
-- | replacement row chosen by the caller.
-- |
-- | Mirrors ZIO `ZIO.orElseFail`. Useful at boundaries where one
-- | family of upstream errors should be summarised under a single
-- | downstream tag without case-analysis.
-- |
-- | ```purescript
-- | -- collapse every parse-level failure into one "invalid input" tag
-- | validated =
-- |   orElseFail (Variant.inj (Proxy :: _ "invalidInput") form) parseForm
-- | ```
orElseFail
  :: forall r e e' a
   . Variant e'
  -> RIO r e a
  -> RIO r e' a
orElseFail v = catchAll (\_ -> rethrow v)

-- | Reflect a fallible action into the success channel as
-- | `Maybe a`: `Just a` on success, `Nothing` on any typed
-- | failure. The error row collapses to whatever the caller fixes
-- | it at (usually `()`).
-- |
-- | Defects still propagate; this only soft-handles typed errors.
-- |
-- | ```purescript
-- | maybeUser <- option (loadUser uid)
-- | case maybeUser of
-- |   Just u -> renderProfile u
-- |   Nothing -> renderAnonymous
-- | ```
option :: forall r e e' a. RIO r e a -> RIO r e' (Maybe a)
option inner = catchAll (\_ -> pure Nothing) (map Just inner)

-- | Narrow the error row, defecting any failure that does not fit
-- | the new row.
-- |
-- | The classifier `(Variant e -> Maybe (Variant e'))` decides which
-- | failures belong on the narrower row `e'` (return `Just`) and
-- | which should be raised as defects (return `Nothing`). The
-- | classifier-built `Error` for a defected failure is fixed at
-- | `Exception.error "RIO.refineOrDie: unrefined failure"`; reach
-- | for `refineOrDieWith` when the defect's message needs to capture
-- | which leftover tag was thrown away.
-- |
-- | Mirrors ZIO `ZIO.refineOrDie` / Effect-TS `Effect.refineOrDie`.
-- | The typical use is at a module boundary: declare which subset of
-- | the inner program's errors are "in contract" and treat anything
-- | else as a programmer bug.
-- |
-- | ```purescript
-- | -- accept notFound; everything else becomes a defect
-- | accept :: RIO r (notFound :: Int) Todo
-- | accept = refineOrDie
-- |   (Variant.on (Proxy :: _ "notFound")
-- |     (Just <<< Variant.inj (Proxy :: _ "notFound"))
-- |     (\_ -> Nothing))
-- |   lookupTodo
-- | ```
refineOrDie
  :: forall r e e' a
   . (Variant e -> Maybe (Variant e'))
  -> RIO r e a
  -> RIO r e' a
refineOrDie classify =
  refineOrDieWith classify
    (\_ -> Exception.error "RIO.refineOrDie: unrefined failure")

-- | Like `refineOrDie`, but the caller supplies the defect's `Error`
-- | per leftover failure. Useful when "this kind of failure shouldn't
-- | reach here" wants a diagnostic message that names the actual
-- | leftover tag.
-- |
-- | Mirrors ZIO `ZIO.refineOrDieWith`.
refineOrDieWith
  :: forall r e e' a
   . (Variant e -> Maybe (Variant e'))
  -> (Variant e -> Error)
  -> RIO r e a
  -> RIO r e' a
refineOrDieWith classify toErr inner =
  catchAll
    ( \v -> case classify v of
        Just v' -> rethrow v'
        Nothing -> die (toErr v)
    )
    inner

-- | Convert a typed failure into a defect via a user-supplied
-- | translator. The error row is discharged on the resulting
-- | action; any failure that occurs becomes an `Aff` exception
-- | observable only through `sandbox`.
-- |
-- | Use this at boundaries where a failure indicates a programmer
-- | bug rather than a recoverable condition - the caller can no
-- | longer match on the typed error.
-- |
-- | ```purescript
-- | -- treat "missing config" as an internal invariant violation
-- | config <- orDie
-- |   (\_ -> Exception.error "internal: config not loaded")
-- |   loadConfig
-- | ```
orDie
  :: forall r e e' a
   . (Variant e -> Error)
  -> RIO r e a
  -> RIO r e' a
orDie toErr inner = catchAll (\v -> die (toErr v)) inner

-- | Map both arms of an `RIO`: transform the typed failure with
-- | `onError` and the success value with `onSuccess`, replacing the
-- | error row in the process. This is the bimap for `RIO`.
-- |
-- | ```purescript
-- | -- rename one tag and normalize the result in one pass
-- | normalize :: RIO r (lookupFailed :: Int | r') (Array Item)
-- | normalize =
-- |   mapBoth
-- |     (Variant.on (Proxy :: _ "notFound")
-- |       (\id -> Variant.inj (Proxy :: _ "lookupFailed") id)
-- |       identity)
-- |     (Array.sortBy compareById)
-- |     fetchItems
-- | ```
mapBoth
  :: forall r e e' a b
   . (Variant e -> Variant e')
  -> (a -> b)
  -> RIO r e a
  -> RIO r e' b
mapBoth onError onSuccess inner =
  mapError onError (map onSuccess inner)
