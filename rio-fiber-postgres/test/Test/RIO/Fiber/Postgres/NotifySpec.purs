-- | Integration tests for `RIO.Fiber.Postgres.Notify` against a real
-- | Postgres instance. Each `it` block builds a fresh
-- | `postgresLayer + notifyLayer` pair, registers a handler on a
-- | unique channel, calls `notify`, and waits for the handler to
-- | write to a `Ref` (the handler runs in `Effect`, so a `Ref` is
-- | the natural cross-thread signal here).
module Test.RIO.Fiber.Postgres.NotifySpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.Row (class Cons) as Row
import Record as Record
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Fiber.Aff (fromAff, runAffEither)
import RIO.Fiber.Core (catchAll, causeOf, fail, fork, interrupt, join, sleep) as RIO
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Layer (provideScoped)
import RIO.Fiber.Postgres (PgError, Postgres, pgErrorMessage, withTransaction)
import RIO.Fiber.Postgres.Layer (postgresLayer)
import RIO.Fiber.Postgres.Notify (Notify, notify, notifyUsing, withListen)
import RIO.Fiber.Postgres.Notify.Layer (notifyLayer)

dbTag :: Proxy "db"
dbTag = Proxy

forcedTag :: Proxy "forced"
forcedTag = Proxy

type DbErr = (db :: PgError)
type AppRow = (postgres :: Postgres, notify :: Notify)
type ErrPlus = (db :: PgError, forced :: Unit)

-- | Local `catchTag`: catches one tag in a variant-row error and
-- | lets the rest of the row re-raise on the smaller row.
catchTag
  :: forall sym x r e e' a
   . IsSymbol sym
  => Row.Cons sym x e' e
  => Proxy sym
  -> (x -> RIO r e' a)
  -> RIO r e a
  -> RIO r e' a
catchTag sym handler =
  RIO.catchAll (\v -> Variant.on sym handler RIO.fail v)

runApp
  :: forall e a
   . String
  -> RIO AppRow e a
  -> Aff (Either (Variant e) a)
runApp conn program =
  runAffEither
    ( provideScoped
        ( \scope -> do
            pg <- postgresLayer { connectionString: conn } scope
            nt <- notifyLayer { connectionString: conn } scope
            pure (Record.merge pg nt)
        )
        program
    )
    {}

-- | Wait until `ref` has a `Just _`, polling every 50ms up to
-- | `timeout`.
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
  describe "RIO.Fiber.Postgres.Notify (integration)" do

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
            notify dbTag channel payload
            fromAff (waitFor (Milliseconds 2000.0) payloadRef)

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
                  fromAff do
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
            fromAff (delay (Milliseconds 300.0))
            fromAff (liftEffect (Ref.read ref))
      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right got -> got `shouldEqual` Nothing

    it "notifyUsing inside a committed transaction delivers" do
      ref <- liftEffect (Ref.new Nothing)
      let
        channel = "rio_test_chan_tx_commit"
        payload = "committed"

        program :: RIO AppRow DbErr (Maybe String)
        program = withListen dbTag channel
          ( \n -> Ref.write n.payload ref
          )
          do
            withTransaction dbTag \client ->
              notifyUsing dbTag channel payload client
            fromAff (waitFor (Milliseconds 2000.0) ref)
      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right got -> got `shouldEqual` Just payload

    it "notifyUsing inside a rolled-back transaction does not deliver" do
      ref <- liftEffect (Ref.new Nothing)
      let
        channel = "rio_test_chan_tx_rollback"
        payload = "rolled-back"

        attempt :: RIO AppRow ErrPlus Unit
        attempt = withTransaction dbTag \client -> do
          notifyUsing dbTag channel payload client
          RIO.fail (Variant.inj forcedTag unit)

        program :: RIO AppRow DbErr (Maybe String)
        program = withListen dbTag channel
          ( \n -> Ref.write n.payload ref
          )
          do
            catchTag forcedTag (\_ -> pure unit) attempt
            fromAff (delay (Milliseconds 300.0))
            fromAff (liftEffect (Ref.read ref))
      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right got -> got `shouldEqual` Nothing

    it "withListen: interrupting the body unsubscribes the handler" do
      ref <- liftEffect (Ref.new Nothing)
      let
        channel = "rio_test_chan_cancel"
        payload = "should-not-arrive"

        program :: RIO AppRow DbErr (Maybe String)
        program = do
          fib <- RIO.fork
            ( withListen dbTag channel
                (\n -> Ref.write n.payload ref)
                (RIO.sleep (Milliseconds 1000.0))
            )
          -- give the listener time to register, then kill it.
          RIO.sleep (Milliseconds 200.0)
          RIO.interrupt fib
          _ <- RIO.causeOf (RIO.join fib)
          -- after interruption the UNLISTEN finalizer should have
          -- run; a notify on the channel must not call the handler.
          notify dbTag channel payload
          fromAff (delay (Milliseconds 300.0))
          fromAff (liftEffect (Ref.read ref))
      result <- runApp conn program
      case result of
        Left v -> fail
          ( "program failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
        Right got -> got `shouldEqual` Nothing
