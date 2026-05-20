-- | Graceful-shutdown signal handling for long-running RIO
-- | programs running on Node.
-- |
-- | A production RIO program (an HTTP server, a worker pool, a
-- | message-queue consumer) typically lives inside a `scoped`
-- | block whose finalizers drain connections, flush pending work,
-- | and close handles when the program ends. Wiring those
-- | finalizers to OS signals (SIGINT from Ctrl-C, SIGTERM from an
-- | orchestrator) requires racing the program against a signal
-- | waiter so that, when a signal arrives, the program's fiber is
-- | killed and every finalizer along the way runs as part of the
-- | kill.
-- |
-- | This module provides:
-- |
-- |   * `awaitShutdown` — an `Aff Signal` that resolves on the
-- |     first matching signal, with one-shot semantics and
-- |     automatic listener removal on resolution or cancellation.
-- |   * `withShutdown` — the canonical wrapper that races a
-- |     program against `awaitShutdown` and returns `Nothing`
-- |     when shutdown wins, `Just a` when the program completes
-- |     naturally.
-- |   * `defaultShutdownSignals` — the conventional `[SIGINT,
-- |     SIGTERM]` set.
-- |   * `withShutdownOn` — `withShutdown` for a caller-supplied
-- |     "shutdown trigger" `Aff Unit`, useful in tests where a
-- |     real OS signal is impractical to deliver.
-- |
-- | ```purescript
-- | main :: Effect Unit
-- | main = launchAff_ do
-- |   result <- withShutdown defaultShutdownSignals do
-- |     env <- buildEnv
-- |     runtimeRun (Runtime.make env) program
-- |   case result of
-- |     Just a -> log ("server returned " <> show a)
-- |     Nothing -> log "shut down on signal"
-- | ```
module RIO.Fiber.Node.Shutdown
  ( awaitShutdown
  , defaultShutdownSignals
  , withShutdown
  , withShutdownOn
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Posix.Signal (Signal(..))
import Effect.Aff (Aff, Canceler(..), makeAff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.EventEmitter (on) as EE
import Node.Process (mkSignalH, process)

-- | The signal set a long-running RIO program usually drains on:
-- | SIGINT (Ctrl-C from a terminal) and SIGTERM (a stop request
-- | from `systemd`, `kubelet`, `pm2`, Docker, etc.).
defaultShutdownSignals :: Array Signal
defaultShutdownSignals = [ SIGINT, SIGTERM ]

-- | Block the current `Aff` fiber until one of `signals` arrives,
-- | and return the signal that fired.
-- |
-- | Single-shot: the first signal to fire resolves the `Aff` and
-- | removes every listener this call installed. If the surrounding
-- | fiber is killed before any signal fires, the listeners are
-- | removed by the canceler.
-- |
-- | Safe to call multiple times concurrently: each call manages
-- | its own listener set and resolves independently.
awaitShutdown :: Array Signal -> Aff Signal
awaitShutdown signals = makeAff \done -> do
  fired <- Ref.new false
  removers <- Ref.new []
  let
    cleanup = do
      rs <- Ref.read removers
      Ref.write [] removers
      traverse_ identity rs
    fire sig = do
      already <- Ref.read fired
      unless already do
        Ref.write true fired
        cleanup
        done (Right sig)
  for_ signals \sig -> do
    remove <- EE.on (mkSignalH sig) (fire sig) process
    Ref.modify_ (\xs -> [ remove ] <> xs) removers
  pure (Canceler \_ -> liftEffect cleanup)

-- | Run `program` and cancel it the moment one of `signals`
-- | arrives. Returns `Just a` when the program finishes naturally
-- | and `Nothing` when shutdown wins.
-- |
-- | When the signal wins, the underlying `Aff` race kills the
-- | program's fiber, which runs every `bracket` / `scoped` /
-- | `ensuring` / `onInterrupt` finalizer along the way in the
-- | uninterruptible release phase. Listener cleanup for the loser
-- | branch is also part of the race-cancellation path, so calling
-- | `withShutdown` does not leak signal handlers either way.
withShutdown :: forall a. Array Signal -> Aff a -> Aff (Maybe a)
withShutdown signals = withShutdownOn (void (awaitShutdown signals))

-- | Like `withShutdown`, but races against any caller-supplied
-- | `Aff Unit`. Useful in tests where delivering a real OS signal
-- | is impractical: the caller can pass an `AVar.take` against a
-- | unit-valued AVar (or any other "this resolves when we want to
-- | shut down" effect) and trigger shutdown by completing it.
withShutdownOn :: forall a. Aff Unit -> Aff a -> Aff (Maybe a)
withShutdownOn waiter program = sequential
  ( parallel (Just <$> program)
      <|> parallel (Nothing <$ waiter)
  )
