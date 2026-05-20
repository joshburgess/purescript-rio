module Test.RIO.Aff.HTTPurple.MiddlewareSpec (spec) where

import Prelude

import Data.Array (any, find) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import HTTPurple (Method(..))
import HTTPurple.Headers (mkRequestHeaders)

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, runRIO, provideAll)
import RIO.Aff.Error (fail) as RIO
import RIO.Aff.Local (Local, newLocalEffect)
import RIO.Aff.Local (get) as Local
import RIO.Aff.Logger (LogLevel(..), Logger)
import RIO.Aff.Test.Clock (newTestClock)
import RIO.Aff.Test.Logger (LogRecord, newRecordingLogger)

import RIO.Aff.HTTPurple.Middleware (withRequestContext)
import RIO.Aff.HTTPurple.Request (RequestContext)

type Env =
  ( logger :: Logger
  , clock :: Clock
  , requestId :: Local String
  )

type Err = (boom :: String)

boomTag :: Proxy "boom"
boomTag = Proxy

sampleCtx :: RequestContext
sampleCtx =
  { method: Get
  , path: "/things/42"
  , requestId: "req-abc"
  , headers: mkRequestHeaders []
  }

buildEnv :: Aff { env :: Record Env, snapshot :: Aff (Array LogRecord) }
buildEnv = do
  rec <- newRecordingLogger
  tc <- newTestClock
  reqIdLocal <- liftEffect (newLocalEffect "<not set>")
  pure
    { env:
        { logger: rec.logger
        , clock: tc.clock
        , requestId: reqIdLocal
        }
    , snapshot: liftEffect rec.snapshot
    }

containsField :: String -> String -> Array (Tuple String String) -> Boolean
containsField k v = Array.any (\(Tuple k' v') -> k == k' && v == v')

spec :: Spec Unit
spec = describe "RIO.Aff.HTTPurple.Middleware.withRequestContext" do
  it "emits 'request received' then 'request completed' on the success path" do
    setup <- buildEnv
    result <- runRIO
      ( provideAll setup.env
          (withRequestContext sampleCtx (pure unit) :: RIO Env Err Unit)
      )
    case result of
      Right _ -> pure unit
      Left _ -> fail "expected success path"
    records <- setup.snapshot
    map _.message records `shouldEqual`
      [ "request received", "request completed" ]
    case Array.find (\r -> r.message == "request completed") records of
      Nothing -> fail "missing 'request completed' record"
      Just r -> do
        r.level `shouldEqual` LogInfo
        containsField "result" "ok" r.fields `shouldEqual` true

  it "emits 'request received' then 'request failed' on the failure path" do
    setup <- buildEnv
    let
      body :: RIO Env Err Unit
      body = RIO.fail boomTag "kaboom"
    result <- runRIO
      ( provideAll setup.env (withRequestContext sampleCtx body)
          :: RIO () Err Unit
      )
    case result of
      Left _ -> pure unit
      Right _ -> fail "expected failure path to propagate the typed failure"
    records <- setup.snapshot
    map _.message records `shouldEqual`
      [ "request received", "request failed" ]
    case Array.find (\r -> r.message == "request failed") records of
      Nothing -> fail "missing 'request failed' record"
      Just r -> do
        r.level `shouldEqual` LogError
        containsField "result" "error" r.fields `shouldEqual` true

  it "stamps request.id / request.method / request.path on every emission inside the block" do
    setup <- buildEnv
    _ <- runRIO
      ( provideAll setup.env
          (withRequestContext sampleCtx (pure unit) :: RIO Env Err Unit)
      )
    records <- setup.snapshot
    let
      assertStamp r = do
        containsField "request.id" "req-abc" r.fields
          `shouldEqual` true
        containsField "request.method" "Get" r.fields
          `shouldEqual` true
        containsField "request.path" "/things/42" r.fields
          `shouldEqual` true
    case records of
      [ a, b ] -> do
        assertStamp a
        assertStamp b
      _ -> fail
        ("expected exactly two records, got " <> show (map _.message records))

  it "sets the requestId Local for the duration of the action" do
    setup <- buildEnv
    let
      body :: RIO Env Err String
      body = Local.get setup.env.requestId
    result <- runRIO
      (provideAll setup.env (withRequestContext sampleCtx body) :: RIO () Err String)
    case result of
      Right rid -> rid `shouldEqual` "req-abc"
      Left _ -> fail "expected success path"

  it "restores the requestId Local after the action exits" do
    setup <- buildEnv
    _ <- runRIO
      ( provideAll setup.env
          (withRequestContext sampleCtx (pure unit) :: RIO Env Err Unit)
      )
    rid <- liftAff
      ( runRIO (provideAll setup.env (Local.get setup.env.requestId))
          :: Aff (Either (Variant ()) String)
      )
    case rid of
      Right s -> s `shouldEqual` "<not set>"
      Left _ -> fail "expected requestId read to succeed"

  it "emits a duration_ms annotation on the completion record" do
    setup <- buildEnv
    _ <- runRIO
      ( provideAll setup.env
          (withRequestContext sampleCtx (pure unit) :: RIO Env Err Unit)
      )
    records <- setup.snapshot
    case Array.find (\r -> r.message == "request completed") records of
      Just r -> case Array.find (\(Tuple k _) -> k == "duration_ms") r.fields of
        Just _ -> pure unit
        Nothing -> fail "expected duration_ms annotation on 'request completed'"
      Nothing -> fail "missing 'request completed' record"
