-- | Bridge between `Stream` and JS `AsyncIterable<A>`.
-- |
-- | * `fromAsyncIterable` consumes a JS async iterable as a `Stream`.
-- |   Each pull awaits the underlying `iterator.next()`. A rejection
-- |   surfaces as a `Die` defect (same convention as
-- |   `RIO.Fiber.Promise.fromPromise`); clean exhaustion ends the
-- |   stream.
-- |
-- | * `toAsyncIterable` exposes a `Stream` to JS consumers by forking
-- |   a producer fiber into a caller-supplied `Scope`. The fiber
-- |   pumps values into a JS-side buffer that backs each
-- |   `iterator.next()`. Closing the scope interrupts the producer.
-- |
-- |   Typed failures are projected through `errToJs` and reified as
-- |   Promise rejections on the JS consumer side. Defects forward
-- |   their original `Error` to the consumer. Interruption-only
-- |   termination simply ends the iteration cleanly.
module RIO.Fiber.Stream.AsyncIterable
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
import Effect.Exception (Error)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Scope (Scope)
import RIO.Fiber.Scope as Scope
import RIO.Fiber.Stream (Stream(..), Step(..))

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
-- | underlying `iterator.next()`; a Promise rejection becomes a `Die`
-- | defect, and clean exhaustion ends the stream.
fromAsyncIterable :: forall r e a. AsyncIterable a -> Stream r e a
fromAsyncIterable iterable = Stream do
  iter <- F.liftEffect (_getAsyncIterator iterable)
  pull iter
  where
  pull iter = do
    result <- F.async \cb -> do
      _pullNext iter
        (\a -> cb (Right (Right (Just a))))
        (cb (Right (Right Nothing)))
        (\err -> cb (Right (Left err)))
      pure (pure unit)
    case result of
      Left err -> F.die err
      Right Nothing -> pure Done
      Right (Just a) -> pure (Yield a (Stream (pull iter)))

-- | Expose a `Stream` as a JS `AsyncIterable<A>`. A producer fiber is
-- | forked into `scope` and pumps every emitted value into a JS-side
-- | buffer that backs the iterator. The fiber is interrupted when
-- | the scope closes.
-- |
-- | Typed failures are mapped to JS `Error` via `errToJs` and surface
-- | as Promise rejections on the next consumer `next()`. Defects
-- | forward their original `Error`.
toAsyncIterable
  :: forall r e a
   . Scope
  -> (Variant e -> Error)
  -> Stream r e a
  -> RIO r e (AsyncIterable a)
toAsyncIterable scope errToJs source = do
  handle <- F.liftEffect _mkAsyncIterableHandle
  _ <- Scope.forkScoped scope (producer handle source)
  pure (_handleIterable handle)
  where
  producer
    :: Handle a -> Stream r e a -> RIO r e Unit
  producer handle stream = do
    outcome <- F.causeOf (pump handle stream)
    case outcome of
      Right _ -> F.liftEffect (_handleEnd handle)
      Left cause ->
        case Array.head (Cause.failures cause) of
          Just v -> F.liftEffect (_handleFail handle (errToJs v))
          Nothing ->
            case Array.head (Cause.defects cause) of
              Just err -> F.liftEffect (_handleFail handle err)
              Nothing -> F.liftEffect (_handleEnd handle)

  pump :: Handle a -> Stream r e a -> RIO r e Unit
  pump handle (Stream pullStream) = do
    step <- pullStream
    case step of
      Done -> pure unit
      Yield a rest -> do
        F.liftEffect (_handlePush handle a)
        pump handle rest
