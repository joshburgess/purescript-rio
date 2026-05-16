-- | A recording `HttpClient` for tests.
-- |
-- | `newRecordingHttpClient` takes a list of canned responses and
-- | returns:
-- |
-- |   * `httpClient`: an `HttpClient` to provide as the
-- |     `httpClient` service to the program under test.
-- |   * `snapshot`: an `Effect` that returns every request the
-- |     program made, in send order.
-- |
-- | The mock walks the canned responses in order. For request `n`
-- | it returns the `n`-th canned response. If there are more
-- | requests than canned responses, the mock returns an
-- | `HttpTransport` error tagged `"recording-http-client: no canned response"`
-- | so tests fail loudly instead of silently rerunning the last
-- | response.
-- |
-- | ```purescript
-- | itRIO "calls the upstream once" do
-- |   rec <- liftAff (newRecordingHttpClient
-- |     [ Right { status: 200, statusText: "OK", headers: [], body: "{}" } ])
-- |   _ <- runWith rec.httpClient (callUpstream req)
-- |   reqs <- liftEffect rec.snapshot
-- |   Array.length reqs `shouldEqual` 1
-- | ```
module RIO.Test.HttpClient
  ( RecordingHttpClient
  , newRecordingHttpClient
  ) where

import Prelude

import Data.Array (index, snoc) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.HttpClient
  ( HttpClient
  , HttpError(..)
  , Request
  , Response
  , mockHttpClient
  )

-- | The recording client paired with the controller that reads
-- | back what it captured.
type RecordingHttpClient =
  { httpClient :: HttpClient
  , snapshot :: Effect (Array Request)
  }

-- | Allocate a fresh recording client. The supplied array is the
-- | script of canned outcomes consulted in send order. Each
-- | outcome is either the typed `HttpError` to surface or the
-- | `Response` to return.
newRecordingHttpClient
  :: Array (Either HttpError Response)
  -> Aff RecordingHttpClient
newRecordingHttpClient script = liftEffect do
  capturedRef <- Ref.new ([] :: Array Request)
  counterRef <- Ref.new 0
  let
    handler :: Request -> Aff (Either HttpError Response)
    handler req = do
      liftEffect (Ref.modify_ (\xs -> Array.snoc xs req) capturedRef)
      i <- liftEffect (Ref.modify (_ + 1) counterRef)
      let n = i - 1
      pure case Array.index script n of
        Just outcome -> outcome
        Nothing -> Left
          ( HttpTransport
              ( "recording-http-client: no canned response for request "
                  <> show n
              )
          )

    snapshot :: Effect (Array Request)
    snapshot = Ref.read capturedRef
  pure
    { httpClient: mockHttpClient handler
    , snapshot
    }
