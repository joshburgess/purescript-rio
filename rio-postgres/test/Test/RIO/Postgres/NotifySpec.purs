-- | Integration tests for `RIO.Postgres.Notify` against a real
-- | Postgres instance. Each `it` block builds a fresh
-- | `postgresLayer + notifyLayer` pair, registers a handler on a
-- | unique channel, calls `notify`, and waits for the handler to
-- | write to a `Ref` (the handler runs in `Effect`, so a `Ref` is
-- | the natural cross-thread signal here).
module Test.RIO.Postgres.NotifySpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, provideLayer, runRIO)
import RIO.Layer ((<+>))
import RIO.Postgres (PgError, Postgres, pgErrorMessage)
import RIO.Postgres.Layer (postgresLayer)
import RIO.Postgres.Notify (Notify, notify, withListen)
import RIO.Postgres.Notify.Layer (notifyLayer)
import Data.Variant as Variant

dbTag :: Proxy "db"
dbTag = Proxy

type DbErr = (db :: PgError)

type AppRow = (postgres :: Postgres, notify :: Notify)

runApp
  :: forall e a
   . String
  -> RIO AppRow e a
  -> Aff (Either (Variant e) a)
runApp conn program =
  runRIO
    ( provideLayer
        ( postgresLayer { connectionString: conn }
            <+> notifyLayer { connectionString: conn }
        )
        program
    )

-- | Wait until `ref` has a `Just _`, polling every 50ms up to
-- | `timeout`. Returns the final value (which may still be
-- | `Nothing` on timeout).
waitFor :: forall a. Milliseconds -> Ref (Maybe a) -> Aff (Maybe a)
waitFor (Milliseconds total) ref = go total
  where
  step = 50.0
  go remaining = do
    v <- liftEffect (Ref.read ref)
    if isJust v || remaining <= 0.0 then pure v
    else do
      delay (Milliseconds step)
      go (remaining - step)

spec :: String -> Spec Unit
spec conn = do
  describe "RIO.Postgres.Notify (integration)" do

    it "delivers a NOTIFY payload to a withListen handler on the same channel" do
      payloadRef <- liftEffect (Ref.new Nothing)
      let
        channel = "rio_test_chan_basic"
        payload = "hello-listen"

        program :: RIO AppRow DbErr (Maybe String)
        program = withListen dbTag channel
          ( \n -> Ref.write n.payload payloadRef
          )
          do
            -- the subscriber client runs LISTEN synchronously before
            -- registerSubscriber returns, so the subsequent NOTIFY
            -- is guaranteed to be visible.
            notify dbTag channel payload
            liftAff (waitFor (Milliseconds 2000.0) payloadRef)

      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right got -> got `shouldEqual` Just payload

    it "delivers to two handlers on the same channel" do
      aRef <- liftEffect (Ref.new Nothing)
      bRef <- liftEffect (Ref.new Nothing)
      let
        channel = "rio_test_chan_fanout"
        payload = "fan-out"

        program :: RIO AppRow DbErr (Maybe String /\ Maybe String)
        program =
          withListen dbTag channel
            ( \n -> Ref.write n.payload aRef
            )
            ( withListen dbTag channel
                ( \n -> Ref.write n.payload bRef
                )
                do
                  notify dbTag channel payload
                  liftAff do
                    a <- waitFor (Milliseconds 2000.0) aRef
                    b <- waitFor (Milliseconds 2000.0) bRef
                    pure (a /\ b)
            )
      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right (a /\ b) -> do
          a `shouldEqual` Just payload
          b `shouldEqual` Just payload

    it "does not deliver to a handler on a different channel" do
      ref <- liftEffect (Ref.new Nothing)
      let
        program :: RIO AppRow DbErr (Maybe String)
        program = withListen dbTag "rio_test_chan_a"
          ( \n -> Ref.write n.payload ref
          )
          do
            notify dbTag "rio_test_chan_b" "ignored"
            liftAff (delay (Milliseconds 300.0))
            liftEffect (Ref.read ref)
      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right got -> got `shouldEqual` Nothing
