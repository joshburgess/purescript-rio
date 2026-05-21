-- | Bridge between `rio-fiber` and JavaScript `Promise`s.
-- |
-- | A JS Promise has no notion of cancellation, so cancellation does
-- | not propagate from the surrounding fiber into the underlying
-- | Promise. Interrupting the fiber lets it move on, but the Promise
-- | continues to run; if you need cancellation, wire it through an
-- | `AbortController` and surface it via `RIO.Fiber.AbortSignal`.
-- |
-- | Promise rejection is reified as a `Die` defect (`Effect.Exception.Error`)
-- | because JS rejections are untyped. If you want a typed failure
-- | row, use `Error.refineOrDie` or `catchAllCause` to convert the
-- | defect to a domain error after the bridge.
module RIO.Fiber.Promise
  ( Promise
  , fromPromise
  , fromPromiseEffect
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import RIO.Fiber.Core (RIO, async, die)

-- | An opaque handle to a JavaScript `Promise<A>`. Use `fromPromise`
-- | (when you have one in hand) or `fromPromiseEffect` (when you have
-- | an `Effect` that creates one) to bring its value into `RIO`.
foreign import data Promise :: Type -> Type

foreign import _runPromise
  :: forall a
   . Effect (Promise a)
  -> (a -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

-- | Lift a JavaScript `Promise<A>` into `RIO`. Resolution lands on
-- | the success channel; rejection becomes a `Die` defect carrying
-- | the rejection reason as an `Error`. Non-`Error` rejection values
-- | are coerced to an `Error` via their string form.
-- |
-- | Note: cancellation does not propagate. Interrupting the fiber
-- | does not abort the Promise; if you need that, expose the source
-- | as an `AbortController` instead.
fromPromise :: forall r e a. Promise a -> RIO r e a
fromPromise p = fromPromiseEffect (pure p)

-- | Variant of `fromPromise` for the common case where the source is
-- | an `Effect` that constructs the Promise (e.g. `fetch`). The
-- | constructor `Effect` is run once at the moment `fromPromise`
-- | begins; any synchronous exception it throws is reflected as
-- | `Die` like a rejection.
fromPromiseEffect :: forall r e a. Effect (Promise a) -> RIO r e a
fromPromiseEffect mk = do
  result <- async \cb -> do
    _runPromise mk
      (\a -> cb (Right (Right a)))
      (\err -> cb (Right (Left err)))
    pure (pure unit)
  case result of
    Right a -> pure a
    Left err -> die err
