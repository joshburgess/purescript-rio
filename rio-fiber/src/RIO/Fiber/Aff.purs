-- | Bridge between `rio-fiber` and `Effect.Aff`.
-- |
-- | Use this module to interoperate with code written against
-- | `purescript-aff`, the de facto async monad in the PureScript
-- | ecosystem. It provides both directions:
-- |
-- |   * `fromAff` lifts an `Aff` into `RIO`. Aff has no typed error
-- |     row, so any error it raises is surfaced as a defect (`Die`).
-- |     Cancellation propagates: interrupting the surrounding fiber
-- |     kills the embedded Aff.
-- |
-- |   * `runAff`, `runAffEither`, and `runAffThrow` run an `RIO`
-- |     program inside an `Aff`. The three flavours differ only in
-- |     how they project the `Outcome`: full structure, an Aff-shaped
-- |     `Either`, or a bare value (defects and interrupts become
-- |     `Aff` exceptions). Cancellation propagates: killing the Aff
-- |     interrupts the running fiber.
module RIO.Fiber.Aff
  ( fromAff
  , runAff
  , runAffEither
  , runAffThrow
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff, Canceler(..))
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, throwException)
import RIO.Fiber.Core (Outcome(..), RIO, async, die, runRIOCallback)

-- | Lift an `Aff` action into `RIO`. The result is delivered on the
-- | success channel; an Aff failure surfaces as a defect (`Die`), and
-- | interrupting the fiber kills the embedded Aff via `Aff.killFiber`.
fromAff :: forall r e a. Aff a -> RIO r e a
fromAff aff = do
  result <- async \cb -> do
    fiber <- Aff.runAff (cb <<< Right) aff
    pure
      ( Aff.launchAff_
          (Aff.killFiber (error "rio-fiber: fromAff cancelled") fiber)
      )
  case result of
    Right a -> pure a
    Left err -> die err

-- | Run an `RIO` program inside `Aff`, returning the full `Outcome`
-- | so the caller can inspect typed failures, defects, and interrupts
-- | separately. Interrupting the surrounding `Aff` propagates as an
-- | interruption request to the running fiber.
runAff :: forall r e a. RIO r e a -> Record r -> Aff (Outcome e a)
runAff rio env = Aff.makeAff \cb -> do
  cancel <- runRIOCallback rio env (cb <<< Right)
  pure (Canceler \_ -> liftEffect cancel)

-- | Run an `RIO` program inside `Aff`, projecting the outcome onto
-- | Aff's shape: typed failures land in `Left`, success in `Right`,
-- | and defects or interrupts are raised on Aff's error channel.
runAffEither
  :: forall r e a. RIO r e a -> Record r -> Aff (Either (Variant e) a)
runAffEither rio env = do
  outcome <- runAff rio env
  case outcome of
    Success a -> pure (Right a)
    Fail v -> pure (Left v)
    Die err -> liftEffect (throwException err)
    Interrupted ->
      liftEffect (throwException (error "rio-fiber: interrupted"))

-- | Run a fully discharged `RIO` program inside `Aff`. Every channel
-- | but success becomes an Aff exception: typed failures, defects,
-- | and interrupts all `throwException`. Convenient for one-shot
-- | scripts where the typed row is empty.
runAffThrow :: forall a. RIO () () a -> Aff a
runAffThrow rio = do
  outcome <- runAff rio {}
  case outcome of
    Success a -> pure a
    Fail v -> Variant.case_ v
    Die err -> liftEffect (throwException err)
    Interrupted ->
      liftEffect (throwException
        (error "rio-fiber: interrupted" :: Error))
