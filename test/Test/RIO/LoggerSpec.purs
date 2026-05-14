module Test.RIO.LoggerSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), attempt, delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, catchTag, die, fail, fork, join, provideAll, runRIO, runRIO')
import RIO.Logger
  ( LogLevel(..)
  , Logger
  , logDebug
  , logError
  , logInfo
  , logTrace
  , logWarn
  , noopLogger
  , withField
  , withFields
  )
import RIO.Test.Logger (newRecordingLogger)

spec :: Spec Unit
spec = describe "RIO.Logger" do
  describe "LogLevel instances" do
    -- The docstring promises a five-band order from `LogTrace`
    -- (noisiest) to `LogError` (loudest non-defect signal). Pin
    -- the derived `Ord` follows that order, and the `Show`
    -- instance renders each constructor by its name so log
    -- pipelines that rely on these instances don't silently
    -- break.
    it "Show renders each level by its constructor name" do
      show LogTrace `shouldEqual` "LogTrace"
      show LogDebug `shouldEqual` "LogDebug"
      show LogInfo `shouldEqual` "LogInfo"
      show LogWarn `shouldEqual` "LogWarn"
      show LogError `shouldEqual` "LogError"

    it "Ord orders LogTrace < LogDebug < LogInfo < LogWarn < LogError" do
      (LogTrace < LogDebug) `shouldEqual` true
      (LogDebug < LogInfo) `shouldEqual` true
      (LogInfo < LogWarn) `shouldEqual` true
      (LogWarn < LogError) `shouldEqual` true

  describe "level smart constructors" do
    it "logTrace emits at LogTrace" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logTrace "trace-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> do
          r.level `shouldEqual` LogTrace
          r.message `shouldEqual` "trace-msg"
          r.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "logDebug emits at LogDebug" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logDebug "debug-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogDebug
        _ -> 1 `shouldEqual` Array.length records

    it "logInfo emits at LogInfo" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logInfo "info-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogInfo
        _ -> 1 `shouldEqual` Array.length records

    it "logWarn emits at LogWarn" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logWarn "warn-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogWarn
        _ -> 1 `shouldEqual` Array.length records

    it "logError emits at LogError" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logError "error-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogError
        _ -> 1 `shouldEqual` Array.length records

  describe "withField / withFields" do
    it "withField attaches a single field to every emission inside the block" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withField "request.id" "abc-123" do
          logInfo "first"
          logInfo "second"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.message `shouldEqual` "first"
          r1.fields `shouldEqual` [ Tuple "request.id" "abc-123" ]
          r2.message `shouldEqual` "second"
          r2.fields `shouldEqual` [ Tuple "request.id" "abc-123" ]
        _ -> 1 `shouldEqual` Array.length records

    it "withFields attaches multiple fields and preserves their input order" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withFields
          [ Tuple "request.id" "abc"
          , Tuple "user" "alice"
          , Tuple "tenant" "acme"
          ]
          (logInfo "hello")
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] ->
          r.fields `shouldEqual`
            [ Tuple "request.id" "abc"
            , Tuple "user" "alice"
            , Tuple "tenant" "acme"
            ]
        _ -> 1 `shouldEqual` Array.length records

    it "annotation set is restored after withFields exits (success)" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          withField "scope" "inner" (logInfo "inside")
          logInfo "outside"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.fields `shouldEqual` [ Tuple "scope" "inner" ]
          r2.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "annotation set is restored after withFields exits on typed failure" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure unit)
            ( withField "scope" "inner" do
                logInfo "before-fail"
                fail (Proxy :: Proxy "boom") unit
            )
          logInfo "after-catch"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.message `shouldEqual` "before-fail"
          r1.fields `shouldEqual` [ Tuple "scope" "inner" ]
          r2.message `shouldEqual` "after-catch"
          r2.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "annotation set is restored after withFields exits on defect" do
      -- Docstring promise: "Restoration is guaranteed by
      -- `Aff.finally` on every termination path (success, typed
      -- failure, defect, fiber interruption)". Success and
      -- typed-failure are pinned above; pin the defect path so
      -- the full bracket contract is documented.
      rec <- liftAff newRecordingLogger
      let
        inner :: RIO (logger :: Logger) () Unit
        inner = withField "scope" "inner" (die (error "kaboom"))
      _ <- attempt
        (runRIO' (provideAll { logger: rec.logger } inner))
      -- A fresh emission through the same logger should observe
      -- the previous (empty) annotation set restored.
      let
        observe :: RIO (logger :: Logger) () Unit
        observe = logInfo "after-defect"
      _ <- runRIO' (provideAll { logger: rec.logger } observe)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> do
          r.message `shouldEqual` "after-defect"
          r.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "annotation set is restored after the fiber is killed mid-body" do
      -- Pin the last termination path the `withFields` docstring
      -- promises: a fiber kill mid-action must still trigger the
      -- `finally`-wired restore.
      rec <- liftAff newRecordingLogger
      let
        inner :: RIO (logger :: Logger) () Unit
        inner = withField "scope" "inner"
          (liftAff (delay (Milliseconds 50.0)))
      f <- forkAff (runRIO' (provideAll { logger: rec.logger } inner))
      delay (Milliseconds 5.0)
      killFiber (error "test-cancel") f
      delay (Milliseconds 10.0)
      let
        observe :: RIO (logger :: Logger) () Unit
        observe = logInfo "after-kill"
      _ <- runRIO' (provideAll { logger: rec.logger } observe)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> do
          r.message `shouldEqual` "after-kill"
          r.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "nested withFields: inner shadows outer; outer restored on inner exit" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withFields
          [ Tuple "request.id" "outer-id"
          , Tuple "tenant" "acme"
          ]
          do
            logInfo "at-outer"
            withField "request.id" "inner-id" (logInfo "at-inner")
            logInfo "back-at-outer"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2, r3 ] -> do
          r1.fields `shouldEqual`
            [ Tuple "request.id" "outer-id"
            , Tuple "tenant" "acme"
            ]
          r2.fields `shouldEqual`
            [ Tuple "tenant" "acme"
            , Tuple "request.id" "inner-id"
            ]
          r3.fields `shouldEqual`
            [ Tuple "request.id" "outer-id"
            , Tuple "tenant" "acme"
            ]
        _ -> 1 `shouldEqual` Array.length records

    it "fields outside any withFields block are empty" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logInfo "bare"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

  describe "fork inheritance (implicit-context semantics)" do
    -- The "Concurrency and fork inheritance" section of the
    -- Logger docstring promises two things: (1) "a forked fiber
    -- that emits a log line reads whatever annotations are
    -- current at emission time", and (2) "writes from any fiber
    -- are visible to every fiber". These are the same trade-offs
    -- `RIO.Local` and `RIO.Tracer` document for their implicit-
    -- context model. `RIO.Local` already pins this; pin it for
    -- `Logger` so a future refactor that switches to per-fiber
    -- isolation can't slip through silently.
    it "a forked fiber inherits the parent's annotations at emit-time" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withField "request.id" "outer" do
          child <- fork (logInfo "from-child")
          logInfo "from-parent"
          join child
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      Array.length records `shouldEqual` 2
      case records of
        [ r1, r2 ] -> do
          r1.fields `shouldEqual` [ Tuple "request.id" "outer" ]
          r2.fields `shouldEqual` [ Tuple "request.id" "outer" ]
        _ -> 1 `shouldEqual` Array.length records

    it "a child's withFields write is visible to a later parent emission" do
      -- The docstring is explicit that the underlying `Effect.Ref`
      -- is shared across fibers, so a write inside the child is
      -- observable to the parent once `join` returns. Pin that
      -- shared-state behaviour: it is the trade-off the docstring
      -- documents, and changing it would be a breaking change.
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          child <- fork
            (withField "child.id" "c1" (logInfo "from-child"))
          join child
          logInfo "from-parent"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.message `shouldEqual` "from-child"
          r1.fields `shouldEqual` [ Tuple "child.id" "c1" ]
          r2.message `shouldEqual` "from-parent"
          -- After the child's withFields exits, its `finally`
          -- restores the previous (empty) annotation set, so the
          -- parent's later emission sees no fields.
          r2.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

  describe "noopLogger" do
    it "runs every emission without crashing and produces no observable output" do
      logger <- liftEffect noopLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          logTrace "trace"
          logDebug "debug"
          logInfo "info"
          logWarn "warn"
          logError "error"
      result <- runRIO (provideAll { logger } program)
      case result of
        Right _ -> pure unit
        Left _ -> 1 `shouldEqual` 0

    it "scopes annotations through withFields even with discarded emissions" do
      -- noopLogger's docstring promises that withFields still cycles
      -- annotations via internal state. We can't observe emissions, but
      -- a typed failure inside withFields must not crash because of
      -- broken annotation save/restore.
      logger <- liftEffect noopLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure unit)
            ( withField "scope" "inner" do
                logInfo "inside"
                fail (Proxy :: Proxy "boom") unit
            )
          logInfo "after"
      result <- runRIO (provideAll { logger } program)
      case result of
        Right _ -> pure unit
        Left _ -> 1 `shouldEqual` 0
