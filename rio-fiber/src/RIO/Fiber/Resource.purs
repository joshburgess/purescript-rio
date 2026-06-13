-- | Resource-safety primitives for `rio-fiber`.
-- |
-- | `acquireRelease` is the bracket-style primitive that guarantees a
-- | release runs on every path: success, typed failure, defect, or
-- | interrupt. `Scope` and `scoped` give LIFO finalizers for
-- | resources that share a lifetime, with full exit-aware reporting
-- | via `addFinalizerExit`.
-- |
-- | Most operations are thin re-exports of primitives that already
-- | live in `RIO.Fiber.Core` (the runtime-native `bracket` /
-- | `ensuring` / `ensuringWith`) or `RIO.Fiber.Scope` (`Scope`,
-- | `addFinalizer` family, `scoped`, `supervised`). The module
-- | bundles them under one import surface so call sites ported from
-- | `RIO.Aff.Resource` keep working with a one-line module change.
-- |
-- | What rio-fiber adds on top of the aff surface:
-- |
-- |   * `acquireRelease` here is `bracket` with the release row
-- |     widened to `()`, matching the aff signature exactly. The
-- |     release cannot raise typed failures (there is no caller-
-- |     visible place for one); defects in release propagate.
-- |   * `onInterrupt` fires the finalizer if and only if the
-- |     action's cause contains an interrupt, leaving success,
-- |     typed failure, and pure-defect paths untouched.
-- |   * `scoped` from `RIO.Fiber.Scope` is *callback-shaped*
-- |     (`Scope -> RIO r e a`) rather than row-label-shaped
-- |     (`(scope :: Scope | r)`) as in aff. The callback form is
-- |     what the fiber runtime exposes natively and avoids a
-- |     row-extension dance for each block. (`supervised` takes a
-- |     plain `RIO r e a` and tracks its scope implicitly.)
module RIO.Fiber.Resource
  ( module Exports
  , acquireRelease
  , onInterrupt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant

import RIO.Fiber.Cause (isInterrupted)
import RIO.Fiber.Core (RIO, bracket, catchAll, ensuringWith)
import RIO.Fiber.Core (bracket, ensuring, ensuringWith) as Exports
import RIO.Fiber.Scope
  ( Scope
  , addFinalizer
  , addFinalizerExit
  , addFinalizerRIO
  , closeScope
  , closeScopeAwait
  , closeScopeExit
  , closeScopeExitAwait
  , newScope
  , scoped
  , scopedAwait
  , supervised
  ) as Exports

-- | Run `acquire`, then `use`, then `release`, guaranteeing that
-- | `release` runs no matter how `use` ends: success, typed failure,
-- | defect (`die` or any host exception), or interrupt.
-- |
-- | The release path has an empty error row (`()`); a release action
-- | cannot fail with a typed error because there is no caller-visible
-- | place to put one. Defects in the release path *will* propagate
-- | and surface at the runner.
-- |
-- | If `acquire` itself fails (typed or defect), `release` is **not**
-- | called, because there is nothing to release. The typed failure or
-- | defect propagates unchanged.
-- |
-- | ```purescript
-- | -- open a file, read it, guarantee the handle is closed
-- | readContents :: forall r e. String -> RIO r e String
-- | readContents path = acquireRelease
-- |   (openRead path)
-- |   (\h -> close h)
-- |   (\h -> readAll h)
-- | ```
acquireRelease
  :: forall r e a b
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> (a -> RIO r e b)
  -> RIO r e b
acquireRelease acquire release use =
  bracket acquire (\a -> widenErrors (release a)) use

-- | Run `finalizer` only when `action` is interrupted. Normal
-- | completion, typed failure, and pure defects all skip the
-- | finalizer; this is the cancellation-specific counterpart to
-- | `ensuring` (which fires on every termination path).
-- |
-- | Mirrors ZIO `ZIO.onInterrupt` / Effect-TS `Effect.onInterrupt`.
-- | Use it when the cleanup is *the rollback you owe specifically
-- | on cancellation*, distinct from cleanup you would run on any
-- | exit: enqueue a "request was cancelled" entry, release a
-- | half-claimed lease, mark a half-applied write as aborted. For
-- | "always-on" cleanup, reach for `ensuring` or `acquireRelease`
-- | instead.
-- |
-- | The trigger is "the cause contains an interrupt anywhere", which
-- | mirrors the ZIO semantic. A cause that is purely a typed failure
-- | or a defect (no interrupt component) does not fire the handler.
-- |
-- | The finalizer's error row is `()`; it cannot fail with a typed
-- | error. Defects raised inside the finalizer propagate and surface
-- | at the runner. The finalizer runs inside the uninterruptible
-- | region installed by `ensuringWith`, so a late interrupt cannot
-- | abandon it midway.
-- |
-- | ```purescript
-- | -- mark a pending write aborted only if the caller cancelled
-- | applyWrite = onInterrupt
-- |   (commitTwoPhase writeId)
-- |   (markAborted writeId)
-- | ```
onInterrupt
  :: forall r e a
   . RIO r e a
  -> RIO r () Unit
  -> RIO r e a
onInterrupt action finalizer = ensuringWith action handler
  where
  handler (Right _) = pure unit
  handler (Left c)
    | isInterrupted c = widenErrors finalizer
    | otherwise = pure unit

-- | Treat an action whose typed-error row is the empty row `()` as
-- | an action over any row `e`. Sound because `Variant ()` is
-- | uninhabited: no `Left v` branch can be produced, so the
-- | `catchAll` handler can never actually fire.
widenErrors :: forall r e a. RIO r () a -> RIO r e a
widenErrors = catchAll (\v -> Variant.case_ v)
