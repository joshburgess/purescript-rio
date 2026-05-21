-- | Bridge between `Stream` and JS `AsyncIterable<A>`.
-- |
-- | * `fromAsyncIterable` consumes a JS async iterable as a `Stream`.
-- |   Each pull awaits the underlying `iterator.next()`. A rejection
-- |   surfaces as a `Die` defect (same convention as anywhere else
-- |   in rio-aff that bridges JS Promise rejections); clean
-- |   exhaustion ends the stream.
-- |
-- | * `toAsyncIterable` exposes a `Stream` to JS consumers by
-- |   forking a producer fiber into a caller-supplied `Scope`. The
-- |   fiber pumps values into a JS-side buffer that backs each
-- |   `iterator.next()`. Closing the scope interrupts the producer.
-- |
-- |   Typed failures are projected through `errToJs` and reified as
-- |   Promise rejections on the JS consumer side. Defects forward
-- |   their original `Error`. Clean termination simply ends the
-- |   iteration.
module RIO.Aff.Stream.AsyncIterable
  ( AsyncIterable
  , fromAsyncIterable
  , toAsyncIterable
  ) where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler) as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import RIO.Aff.Cause as Cause
import RIO.Aff.Cause (attemptCause)
import RIO.Aff.Concurrency (forkScoped)
import RIO.Aff.Core (RIO)
import RIO.Aff.Internal (mkRIO)
import RIO.Aff.Resource (Scope)
import RIO.Aff.Stream (Stream(..), Step(..))

-- | An opaque handle to a JS `AsyncIterable<A>`.
foreign import data AsyncIterable :: Type -> Type

foreign import data AsyncIterator :: Type -> Type
foreign import data Handle :: Type -> Type

foreign import _getAsyncIterator
  :: forall a. AsyncIterable a -> Effect (AsyncIterator a)

foreign import _pullNext
  :: forall a
   . AsyncIterator a
  -> (a -> Effect Unit)
  -> Effect Unit
  -> (Error -> Effect Unit)
  -> Effect Unit

foreign import _mkAsyncIterableHandle :: forall a. Effect (Handle a)
foreign import _handlePush :: forall a. Handle a -> a -> Effect Unit
foreign import _handleEnd :: forall a. Handle a -> Effect Unit
foreign import _handleFail :: forall a. Handle a -> Error -> Effect Unit
foreign import _handleIterable :: forall a. Handle a -> AsyncIterable a

-- | Stream over a JS `AsyncIterable<A>`. Each pull awaits the
-- | underlying `iterator.next()`; a Promise rejection becomes a
-- | `Die` defect, and clean exhaustion ends the stream.
fromAsyncIterable :: forall r e a. AsyncIterable a -> Stream r e a
fromAsyncIterable iterable = Stream do
  iter <- liftEffect (_getAsyncIterator iterable)
  pull iter
  where
  pull :: AsyncIterator a -> RIO r e (Step r e a)
  pull iter = do
    result <- mkRIO \_ -> Aff.makeAff \resume -> do
      _pullNext iter
        (\a -> resume (Right (Just a)))
        (resume (Right Nothing))
        (\err -> resume (Left err))
      pure Aff.nonCanceler
    case result of
      Nothing -> pure Done
      Just a -> pure (Yield a (Stream (pull iter)))

-- | Expose a `Stream` as a JS `AsyncIterable<A>`. A producer fiber
-- | is forked into `scope` and pumps every emitted value into a
-- | JS-side buffer that backs the iterator. The fiber is
-- | interrupted when the scope closes.
-- |
-- | Typed failures are mapped to JS `Error` via `errToJs` and
-- | surface as Promise rejections on the next consumer `next()`.
-- | Defects forward their original `Error`.
toAsyncIterable
  :: forall r e a
   . Scope
  -> (Variant e -> Error)
  -> Stream r e a
  -> RIO r e (AsyncIterable a)
toAsyncIterable scope errToJs source = do
  handle <- liftEffect _mkAsyncIterableHandle
  _ <- forkScoped scope (producer handle source)
  pure (_handleIterable handle)
  where
  producer :: Handle a -> Stream r e a -> RIO r e Unit
  producer handle stream = do
    outcome <- attemptCause (pump handle stream)
    case outcome of
      Right _ -> liftEffect (_handleEnd handle)
      Left cause ->
        case Array.head (Cause.failures cause) of
          Just v -> liftEffect (_handleFail handle (errToJs v))
          Nothing ->
            case Array.head (Cause.defects cause) of
              Just err -> liftEffect (_handleFail handle err)
              Nothing -> liftEffect (_handleEnd handle)

  pump :: Handle a -> Stream r e a -> RIO r e Unit
  pump handle (Stream pullStream) = do
    step <- pullStream
    case step of
      Done -> pure unit
      Yield a rest -> do
        liftEffect (_handlePush handle a)
        pump handle rest
