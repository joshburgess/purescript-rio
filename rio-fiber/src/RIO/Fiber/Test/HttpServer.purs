-- | A recording dispatcher for testing HTTP handlers.
-- |
-- | `RIO.Fiber.HttpServer`'s `listen` / `shutdown` shape is about
-- | binding to a port; for tests we usually don't need a port,
-- | just a way to drive a handler with synthetic requests and
-- | observe what it produced. `newRecordingHttpServer` returns:
-- |
-- |   * `dispatch`: feed a `ServerRequest` through a `Handler` and
-- |     get the `ServerResponse`. Every call is captured.
-- |   * `httpServer`: a `listen` / `shutdown` no-op so existing
-- |     handler wiring that expects the service record still
-- |     type-checks. The recorder doesn't open sockets; calls
-- |     have no effect.
-- |   * `snapshot`: return every (request, response) pair the
-- |     dispatcher saw, in call order.
-- |
-- | ```purescript
-- | itRIO "responds 200 to /health" do
-- |   rec <- liftEffect newRecordingHttpServer
-- |   resp <- fromAff (rec.dispatch handler healthRequest)
-- |   liftEffect (resp.status `shouldEqual` 200)
-- |   calls <- liftEffect rec.snapshot
-- |   liftEffect (Array.length calls `shouldEqual` 1)
-- | ```
module RIO.Fiber.Test.HttpServer
  ( RecordingHttpServer
  , newRecordingHttpServer
  ) where

import Prelude

import Data.Array (snoc) as Array
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Fiber.HttpServer (Handler, HttpServer, ServerRequest, ServerResponse)

-- | The recording server paired with the controllers used to
-- | drive and inspect it.
type RecordingHttpServer =
  { httpServer :: HttpServer
  , dispatch :: Handler -> ServerRequest -> Aff ServerResponse
  , snapshot :: Effect (Array (Tuple ServerRequest ServerResponse))
  }

-- | Allocate a fresh recording server. The returned `httpServer`
-- | is a no-op (`listen` returns immediately, `shutdown` does
-- | nothing) so calling code that expects the service shape still
-- | type-checks even though no socket is opened.
newRecordingHttpServer :: Effect RecordingHttpServer
newRecordingHttpServer = do
  callsRef <- Ref.new ([] :: Array (Tuple ServerRequest ServerResponse))
  let
    dispatch :: Handler -> ServerRequest -> Aff ServerResponse
    dispatch handler req = do
      resp <- handler req
      liftEffect (Ref.modify_ (\xs -> Array.snoc xs (Tuple req resp)) callsRef)
      pure resp

    snapshot :: Effect (Array (Tuple ServerRequest ServerResponse))
    snapshot = Ref.read callsRef

    httpServer :: HttpServer
    httpServer =
      { listen: \_ -> pure unit
      , shutdown: pure unit
      }
  pure { httpServer, dispatch, snapshot }
