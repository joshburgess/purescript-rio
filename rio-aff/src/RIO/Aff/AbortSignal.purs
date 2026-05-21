-- | Minimal bindings to the host's `AbortController` /
-- | `AbortSignal` API, plus the `asyncAbortable` combinator that
-- | plumbs an `AbortSignal` into a register callback and aborts it
-- | when the fiber is interrupted.
-- |
-- | Use this for HTTP, IndexedDB, and similar JS APIs that accept
-- | an abort signal in their options. The common case is `fetch`:
-- | a `RIO.Aff.Concurrency.asyncInterrupt` caller has to allocate
-- | an `AbortController` by hand, thread the signal into the fetch
-- | options, and write the canceller as `() -> controller.abort()`.
-- | `asyncAbortable` removes that boilerplate.
module RIO.Aff.AbortSignal
  ( AbortSignal
  , AbortController
  , newAbortController
  , signalOf
  , abort
  , isAborted
  , asyncAbortable
  ) where

import Prelude

import Data.Either (Either)
import Data.Variant (Variant)
import Effect (Effect)

import RIO.Aff.Concurrency (asyncInterrupt)
import RIO.Aff.Core (RIO)

-- | An opaque handle to the host's `AbortSignal` object.
foreign import data AbortSignal :: Type

-- | An opaque handle to the host's `AbortController` object.
foreign import data AbortController :: Type

-- | Allocate a fresh controller. Its `.signal` starts un-aborted.
foreign import newAbortController :: Effect AbortController

-- | The controller's signal, ready to be passed into a JS API.
foreign import signalOf :: AbortController -> AbortSignal

-- | Trigger the controller. Any JS API that took the signal will
-- | observe `signal.aborted === true` and reject its in-flight
-- | work.
foreign import abort :: AbortController -> Effect Unit

-- | Peek at whether the signal has been aborted yet.
foreign import isAborted :: AbortSignal -> Effect Boolean

-- | An `asyncInterrupt` variant that hands the register callback
-- | an `AbortSignal` plumbed to fiber interruption: when the fiber
-- | is interrupted, the signal aborts. The register receives the
-- | signal (for forwarding to `fetch` and friends) and the resume
-- | callback (for delivering the result).
-- |
-- | Example:
-- |
-- | ```purescript
-- | fetchJson :: forall r. String -> RIO r (network :: String) Json
-- | fetchJson url = asyncAbortable \signal resume -> do
-- |   resp <- fetchWithSignal url signal
-- |   resp.json (\j -> resume (Right j)) (\_ -> resume (Left ...))
-- | ```
asyncAbortable
  :: forall r e a
   . ( AbortSignal
       -> (Either (Variant e) a -> Effect Unit)
       -> Effect Unit
     )
  -> RIO r e a
asyncAbortable register = asyncInterrupt \cb -> do
  controller <- newAbortController
  let signal = signalOf controller
  register signal cb
  pure (abort controller)
