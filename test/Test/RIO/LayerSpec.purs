module Test.RIO.LayerSpec (spec) where

import Prelude hiding ((>>>))

import Data.Array (snoc)
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core
  ( Layer
  , RIO
  , addFinalizer
  , ask
  , buildLayer
  , die
  , fail
  , fromRIO
  , fromRecord
  , passthrough
  , provideLayer
  , runRIO
  )
import RIO.Layer ((<+>), (>>>))

type Logger = { log :: String -> Aff Unit }
type Database = { find :: Int -> Aff Int }
type UserService = { greet :: Int -> Aff String }

spec :: Spec Unit
spec = do
  describe "RIO.Layer (Phase 5.1)" do
    describe "buildLayer" do
      it "builds a trivial logger layer from fromRecord" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          logger :: Logger
          logger = { log: \s -> push ("log:" <> s) }
          layer = fromRecord { logger }
        result <- buildLayer layer
        case result of
          Left _ ->
            liftEffect
              (Ref.modify_ (\xs -> snoc xs "should-not-happen") events)
          Right rec -> liftAff (rec.logger.log "hi")
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "log:hi" ]

      it "propagates a typed failure from inside fromRIO" do
        let
          layer :: Layer () (boom :: Unit) (unused :: Int)
          layer = fromRIO (fail (Proxy :: Proxy "boom") unit)
        result <- buildLayer layer
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "runs scope finalizers before buildLayer returns" do
        events <- liftEffect (Ref.new [])
        let
          layer :: Layer () () (handle :: Unit)
          layer = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            _ <- addFinalizer scope
              (liftEffect (Ref.modify_ (\xs -> snoc xs "release") events))
            liftAff
              (liftEffect (Ref.modify_ (\xs -> snoc xs "acquire") events))
            pure { handle: unit }
        result <- buildLayer layer
        case result of
          Right _ -> pure unit
          Left _ -> 1 `shouldEqual` 0
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "acquire", "release" ]

  describe "RIO.Layer (Phase 5.2)" do
    describe "andThen / (>>>)" do
      it "feeds first layer's output to second layer" do
        let
          dbLayer :: forall e. Layer () e (database :: Database)
          dbLayer = fromRecord { database: { find: \i -> pure (i + 100) } }

          userServiceLayer
            :: forall e. Layer (database :: Database) e (userService :: UserService)
          userServiceLayer = fromRIO do
            db <- ask (Proxy :: Proxy "database")
            pure
              { userService:
                  { greet: \uid -> do
                      n <- db.find uid
                      pure ("user-" <> show n)
                  }
              }

          composed :: Layer () () (userService :: UserService)
          composed = dbLayer >>> userServiceLayer
        result <- buildLayer composed
        case result of
          Left _ -> 1 `shouldEqual` 0
          Right rec -> do
            greeting <- liftAff (rec.userService.greet 7)
            greeting `shouldEqual` "user-107"

      it "short-circuits the second layer if the first fails" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          failing :: Layer () (boom :: Unit) (database :: Database)
          failing = fromRIO (fail (Proxy :: Proxy "boom") unit)

          downstream
            :: Layer (database :: Database) (boom :: Unit)
                 (userService :: UserService)
          downstream = fromRIO do
            liftAff (push "downstream-ran")
            db <- ask (Proxy :: Proxy "database")
            pure { userService: { greet: \i -> db.find i *> pure "x" } }
        result <- buildLayer (failing >>> downstream)
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        order <- liftEffect (Ref.read events)
        order `shouldEqual` []

      it "finalizers from both layers fire LIFO when the scope exits" do
        -- Docstring promise: "Both layers run in the same
        -- surrounding scope, so finalizers from either fire (in
        -- LIFO order) when that scope exits." Pin the LIFO
        -- ordering directly: register one finalizer per layer in
        -- a two-layer `andThen`, then drive the chain through
        -- `buildLayer` (which opens and closes a scope around the
        -- whole composition). The second layer registers last, so
        -- its finalizer must fire first.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          layerA :: forall e. Layer () e (database :: Database)
          layerA = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open-A")
            _ <- addFinalizer scope (push "close-A")
            pure { database: { find: \i -> pure (i + 1) } }

          layerB
            :: forall e
             . Layer (database :: Database) e (userService :: UserService)
          layerB = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open-B")
            _ <- addFinalizer scope (push "close-B")
            db <- ask (Proxy :: Proxy "database")
            pure { userService: { greet: \i -> map show (db.find i) } }
        result <- buildLayer (layerA >>> layerB)
        case result of
          Left _ -> 1 `shouldEqual` 0
          Right _ -> pure unit
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "open-A", "open-B", "close-B", "close-A" ]

    describe "combine / (<+>)" do
      it "merges output rows of two layers sharing input requirements" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          loggerLayer :: forall e. Layer () e (logger :: Logger)
          loggerLayer = fromRecord
            { logger: { log: \s -> push ("log:" <> s) } }

          dbLayer :: forall e. Layer () e (database :: Database)
          dbLayer = fromRecord { database: { find: \i -> pure (i * 2) } }

          combined
            :: Layer () () (logger :: Logger, database :: Database)
          combined = loggerLayer <+> dbLayer
        result <- buildLayer combined
        case result of
          Left _ -> 1 `shouldEqual` 0
          Right rec -> liftAff do
            rec.logger.log "ready"
            n <- rec.database.find 21
            liftEffect (Ref.modify_ (\xs -> snoc xs ("find:" <> show n)) events)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "log:ready", "find:42" ]

      it "finalizers from both layers fire LIFO when the scope exits" do
        -- Docstring promise on `combine`: "Both layers run in the
        -- same surrounding scope; their finalizers join the
        -- scope's stack and fire LIFO on exit." Same shape as the
        -- `andThen` LIFO test above but pinned specifically for
        -- the horizontal combinator. Layer 1 registers `close-1`,
        -- layer 2 registers `close-2`; on `buildLayer` exit,
        -- `close-2` must fire before `close-1`.
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          layer1 :: forall e. Layer () e (logger :: Logger)
          layer1 = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open-1")
            _ <- addFinalizer scope (push "close-1")
            pure { logger: { log: \s -> push ("log:" <> s) } }

          layer2 :: forall e. Layer () e (database :: Database)
          layer2 = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open-2")
            _ <- addFinalizer scope (push "close-2")
            pure { database: { find: \i -> pure (i + 1) } }
        result <- buildLayer (layer1 <+> layer2)
        case result of
          Left _ -> 1 `shouldEqual` 0
          Right _ -> pure unit
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "open-1", "open-2", "close-2", "close-1" ]

      it "propagates failure from either side" do
        let
          failing :: Layer () (boom :: Unit) (a :: Int)
          failing = fromRIO (fail (Proxy :: Proxy "boom") unit)

          ok :: Layer () (boom :: Unit) (b :: Int)
          ok = fromRecord { b: 99 }
        leftFail <- buildLayer (failing <+> ok)
        rightFail <- buildLayer (ok <+> failing)
        case leftFail, rightFail of
          Left _, Left _ -> pure unit
          _, _ -> 1 `shouldEqual` 0

  describe "RIO.Layer (Phase 5.3)" do
    describe "provideLayer" do
      it "feeds the layer's services into the program and runs it" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          loggerLayer :: forall e. Layer () e (logger :: Logger)
          loggerLayer = fromRecord
            { logger: { log: \s -> push ("log:" <> s) } }

          dbLayer :: forall e. Layer () e (database :: Database)
          dbLayer = fromRecord { database: { find: \i -> pure (i + 1) } }

          program
            :: RIO (logger :: Logger, database :: Database) () Int
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            db <- ask (Proxy :: Proxy "database")
            n <- liftAff (db.find 41)
            liftAff (logger.log ("got " <> show n))
            pure n

          provided :: RIO () () Int
          provided = provideLayer (loggerLayer <+> dbLayer) program
        result <- runRIO provided
        case result of
          Left _ -> 1 `shouldEqual` 0
          Right n -> n `shouldEqual` 42
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "log:got 42" ]

      it "unions layer errors and program errors into one row" do
        let
          failingLayer :: Layer () (layerBoom :: Unit) (logger :: Logger)
          failingLayer = fromRIO (fail (Proxy :: Proxy "layerBoom") unit)

          program
            :: RIO (logger :: Logger) (progBoom :: Unit) Int
          program = fail (Proxy :: Proxy "progBoom") unit

          provided
            :: RIO () (layerBoom :: Unit, progBoom :: Unit) Int
          provided = provideLayer failingLayer program
        result <- runRIO provided
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "runs layer finalizers after the program completes" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          resourceLayer :: forall e. Layer () e (logger :: Logger)
          resourceLayer = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open")
            _ <- addFinalizer scope (push "close")
            pure { logger: { log: \s -> push ("log:" <> s) } }

          program :: RIO (logger :: Logger) () Unit
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            liftAff (logger.log "running")

          provided :: RIO () () Unit
          provided = provideLayer resourceLayer program
        result <- runRIO provided
        case result of
          Left _ -> 1 `shouldEqual` 0
          Right _ -> pure unit
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "open", "log:running", "close" ]

  describe "RIO.Layer (Phase 5.4)" do
    describe "resource-safe layers" do
      it "release fires on a typed-failure program path" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          layer :: Layer () (boom :: Unit) (logger :: Logger)
          layer = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open")
            _ <- addFinalizer scope (push "close")
            pure { logger: { log: \s -> push ("log:" <> s) } }

          program :: RIO (logger :: Logger) (boom :: Unit) Unit
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            liftAff (logger.log "before")
            fail (Proxy :: Proxy "boom") unit
        result <- runRIO (provideLayer layer program)
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "open", "log:before", "close" ]

      it "release fires on a defect program path" do
        events <- liftEffect (Ref.new [])
        let
          push s = liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

          layer :: Layer () () (logger :: Logger)
          layer = fromRIO do
            scope <- ask (Proxy :: Proxy "scope")
            liftAff (push "open")
            _ <- addFinalizer scope (push "close")
            pure { logger: { log: \s -> push ("log:" <> s) } }

          program :: RIO (logger :: Logger) () Unit
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            liftAff (logger.log "before")
            die (error "kaboom")

          provided :: RIO () () Unit
          provided = provideLayer layer program
        _ <- attempt (runRIO provided)
        order <- liftEffect (Ref.read events)
        order `shouldEqual` [ "open", "log:before", "close" ]

  describe "RIO.Layer passthrough" do
    it "makes input services visible to downstream consumers" do
      let
        configLayer :: Layer () () (config :: { port :: Int })
        configLayer = fromRecord { config: { port: 8080 } }

        -- A layer that builds a "logger" from config. With plain
        -- `>>>`, only `logger` would be visible downstream; with
        -- `passthrough`, both `config` and `logger` are visible.
        loggerLayer :: Layer (config :: { port :: Int }) () (logger :: Logger)
        loggerLayer = fromRIO do
          _ <- ask (Proxy :: Proxy "config")
          pure { logger: { log: \_ -> pure unit } }

        appLayer :: Layer () () (config :: { port :: Int }, logger :: Logger)
        appLayer = configLayer >>> passthrough loggerLayer

        program
          :: RIO (config :: { port :: Int }, logger :: Logger) () Int
        program = do
          cfg <- ask (Proxy :: Proxy "config")
          _ <- ask (Proxy :: Proxy "logger")
          pure cfg.port

      result <- runRIO (provideLayer appLayer program)
      result `shouldEqual` (Right 8080 :: Either _ Int)
