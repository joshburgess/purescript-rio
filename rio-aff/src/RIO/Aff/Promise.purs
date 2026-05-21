-- | Bridge between `rio-aff` and JavaScript `Promise`s.
-- |
-- | A JS Promise has no notion of cancellation, so cancellation does
-- | not propagate from the surrounding fiber into the underlying
-- | Promise. Interrupting the fiber lets it move on, but the Promise
-- | continues to run; if you need cancellation, wire it through an
-- | `AbortController` and surface that to the source.
-- |
-- | Promise rejection is reified as a defect (`Effect.Exception.Error`
-- | on the Aff exception channel) because JS rejections are untyped.
-- | If you want a typed failure row, use `Error.refineOrDie` or
-- | `catchAllCause` to convert the defect to a domain error after the
-- | bridge.
module RIO.Aff.Promise
  ( Promise
  , fromPromise
  , fromPromiseEffect
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler) as Aff
import Effect.Exception (Error)

import RIO.Aff.Core (RIO)
import RIO.Aff.Internal (mkRIO)

-- | An opaque handle to a JavaScript `Promise<A>`. Use `fromPromise`
-- | (when you have one in hand) or `fromPromiseEffect` (when you
-- | have an `Effect` that creates one) to bring its value into
-- | `RIO`.
foreign import data Promise :: Type -> Type

foreign import _runPromise
  :: forall a
   . Effect (Promise a)
  -> (a -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

-- | Lift a JavaScript `Promise<A>` into `RIO`. Resolution lands on
-- | the success channel; rejection becomes a defect carrying the
-- | rejection reason as an `Error`. Non-`Error` rejection values
-- | are coerced to an `Error` via their string form.
-- |
-- | Note: cancellation does not propagate. Interrupting the fiber
-- | does not abort the Promise; if you need that, expose the source
-- | as an `AbortController` instead.
fromPromise :: forall r e a. Promise a -> RIO r e a
fromPromise p = fromPromiseEffect (pure p)

-- | Variant of `fromPromise` for the common case where the source
-- | is an `Effect` that constructs the Promise (e.g. `fetch`). The
-- | constructor `Effect` is run once at the moment `fromPromise`
-- | begins; any synchronous exception it throws is reflected as a
-- | defect like a rejection.
fromPromiseEffect :: forall r e a. Effect (Promise a) -> RIO r e a
fromPromiseEffect mk = mkRIO \_ -> Aff.makeAff \resume -> do
  _runPromise mk
    (\a -> resume (Right a))
    (\err -> resume (Left err))
  pure Aff.nonCanceler
