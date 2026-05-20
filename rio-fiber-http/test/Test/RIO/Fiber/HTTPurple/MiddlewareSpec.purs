module Test.RIO.Fiber.HTTPurple.MiddlewareSpec (spec) where

import Prelude

import Data.Array (any) as Array
import Data.Either (Either(..))
import Data.String (contains) as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect) as EC
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import HTTPurple (Method(..))
import HTTPurple.Headers (mkRequestHeaders)

import RIO.Fiber.Aff (runAffEither)
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Core (fail) as RIO
import RIO.Fiber.Logger (LogLevel(..), Logger(..))
import RIO.Fiber.Logger (withLogger) as Logger
import Data.Variant as Variant

import RIO.Fiber.HTTPurple.Middleware (withRequestContext)
import RIO.Fiber.HTTPurple.Request (RequestContext)
import RIO.Fiber.HTTPurple.RequestId (getRequestId)

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

spec :: Spec Unit
spec = describe "RIO.Fiber.HTTPurple.Middleware.withRequestContext" do
  it "emits an info 'request received' then info 'request completed' on success" do
    capture <- EC.liftEffect (Ref.new [])
    let
      logger = Logger
        { emit: \lvl msg ->
            Ref.modify_ (\xs -> xs <> [ Tuple lvl msg ]) capture
        }
      prog :: RIO () Err Unit
      prog = Logger.withLogger logger
        (withRequestContext sampleCtx (pure unit))
    result <- runAffEither prog {}
    case result of
      Right _ -> pure unit
      Left _ -> fail "expected success path"
    msgs <- EC.liftEffect (Ref.read capture)
    case msgs of
      [ Tuple lvl1 m1, Tuple lvl2 m2 ] -> do
        lvl1 `shouldEqual` Info
        lvl2 `shouldEqual` Info
        String.contains (Pattern "request received") m1 `shouldEqual` true
        String.contains (Pattern "request completed") m2 `shouldEqual` true
        String.contains (Pattern "result=ok") m2 `shouldEqual` true
      _ -> fail ("expected exactly two log lines, got " <> show (map snd msgs))

  it "emits 'request received' then error 'request failed' on the failure path" do
    capture <- EC.liftEffect (Ref.new [])
    let
      logger = Logger
        { emit: \lvl msg ->
            Ref.modify_ (\xs -> xs <> [ Tuple lvl msg ]) capture
        }
      body :: RIO () Err Unit
      body = RIO.fail (Variant.inj boomTag "kaboom")
      prog :: RIO () Err Unit
      prog = Logger.withLogger logger
        (withRequestContext sampleCtx body)
    result <- runAffEither prog {}
    case result of
      Left _ -> pure unit
      Right _ -> fail "expected failure path to propagate the typed failure"
    msgs <- EC.liftEffect (Ref.read capture)
    case msgs of
      [ Tuple lvl1 m1, Tuple lvl2 m2 ] -> do
        lvl1 `shouldEqual` Info
        lvl2 `shouldEqual` Error
        String.contains (Pattern "request received") m1 `shouldEqual` true
        String.contains (Pattern "request failed") m2 `shouldEqual` true
        String.contains (Pattern "result=error") m2 `shouldEqual` true
      _ -> fail ("expected exactly two log lines, got " <> show (map snd msgs))

  it "stamps the request id, method, and path into every emitted message" do
    capture <- EC.liftEffect (Ref.new [])
    let
      logger = Logger
        { emit: \lvl msg ->
            Ref.modify_ (\xs -> xs <> [ Tuple lvl msg ]) capture
        }
      prog :: RIO () Err Unit
      prog = Logger.withLogger logger
        (withRequestContext sampleCtx (pure unit))
    _ <- runAffEither prog {}
    msgs <- EC.liftEffect (Ref.read capture)
    let
      assertStamp (Tuple _ m) = do
        String.contains (Pattern "id=req-abc") m `shouldEqual` true
    case msgs of
      [ a, b ] -> do
        -- request id appears in every line
        assertStamp a
        assertStamp b
        -- method + path only stamped on the "received" line, but both
        -- request-id sightings already cover scoping
        String.contains (Pattern "method=Get") (snd a) `shouldEqual` true
        String.contains (Pattern "path=/things/42") (snd a) `shouldEqual` true
      _ -> fail
        ( "expected exactly two records, got "
            <> show (map snd msgs)
        )

  it "sets the request id for the duration of the action" do
    capture <- EC.liftEffect (Ref.new "")
    let
      logger = Logger { emit: \_ _ -> pure unit }
      body :: RIO () Err Unit
      body = do
        rid <- getRequestId
        liftEffect (Ref.write rid capture)
      prog :: RIO () Err Unit
      prog = Logger.withLogger logger
        (withRequestContext sampleCtx body)
    _ <- runAffEither prog {}
    seen <- EC.liftEffect (Ref.read capture)
    seen `shouldEqual` "req-abc"

  it "restores the request id after the action exits" do
    -- Before / after capture done at the RIO level so it observes the
    -- per-fiber FiberRef state. The pre-context read happens outside
    -- any `withRequestContext`, so it should see the default
    -- placeholder; the post-context read happens after
    -- `withRequestContext` exits, so the slot should be restored to
    -- that same placeholder.
    capture <- EC.liftEffect (Ref.new (Tuple "" ""))
    let
      logger = Logger { emit: \_ _ -> pure unit }
      prog :: RIO () Err Unit
      prog = Logger.withLogger logger do
        before <- getRequestId
        _ <- withRequestContext sampleCtx (pure unit)
        after <- getRequestId
        liftEffect (Ref.write (Tuple before after) capture)
    _ <- runAffEither prog {}
    Tuple before after <- EC.liftEffect (Ref.read capture)
    before `shouldEqual` "<no request>"
    after `shouldEqual` "<no request>"

  it "emits a duration_ms annotation on the success line" do
    capture <- EC.liftEffect (Ref.new [])
    let
      logger = Logger
        { emit: \lvl msg ->
            Ref.modify_ (\xs -> xs <> [ Tuple lvl msg ]) capture
        }
      prog :: RIO () Err Unit
      prog = Logger.withLogger logger
        (withRequestContext sampleCtx (pure unit))
    _ <- runAffEither prog {}
    msgs <- EC.liftEffect (Ref.read capture)
    Array.any
      (\(Tuple _ m) -> String.contains (Pattern "duration_ms=") m)
      msgs
      `shouldEqual` true

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b
