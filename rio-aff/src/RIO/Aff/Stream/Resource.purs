-- | Resource-safe stream constructors.
-- |
-- | The base `RIO.Aff.Stream` is environment-polymorphic and does not
-- | itself know about `Scope`. That keeps the simple combinators
-- | (`map`, `filter`, `concat`, ...) free of row-constraint noise.
-- |
-- | Once you reach for a stream backed by an OS resource (a file
-- | handle, a Postgres cursor, a network socket), the resource's
-- | lifetime has to be tied to something. This module pins it to
-- | the enclosing `Scope`: the stream's resource is released when
-- | the surrounding `scoped` block exits, on every termination path
-- | (success, typed failure, defect, or fiber kill).
-- |
-- | The scope-as-lifetime model is the same one ZIO uses (`ZStream`
-- | requires `Scope` in its environment when the stream owns
-- | resources).
-- |
-- | ```purescript
-- | -- open a file, stream its lines, guarantee the handle closes
-- | -- when the surrounding `scoped` block exits
-- | program = scoped do
-- |   linesOut <- runCollect
-- |     ( flatMap
-- |         (bracketStream openFile closeFile)
-- |         (\handle -> linesFrom handle)
-- |     )
-- |   pure linesOut
-- | ```
module RIO.Aff.Stream.Resource
  ( Emit(..)
  , acquireReleaseStream
  , async
  , bracketStream
  ) where

import Prelude

import Data.Array (snoc, uncons) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (asyncInterrupt)
import RIO.Aff.Core (RIO)
import RIO.Aff.Env (ask)
import RIO.Aff.Error (die, rethrow)
import RIO.Aff.Resource (Scope, addFinalizer)
import RIO.Aff.Stream (Step(..), Stream(..), empty)

-- | A single-element stream that acquires a resource and registers
-- | its release with the enclosing scope. The resource is released
-- | when the scope exits, not when the stream itself completes.
-- |
-- | Compose with `flatMap` to build a multi-element stream that
-- | uses the acquired resource across many yields.
-- |
-- | If `acquire` fails (typed or defect), the release action is not
-- | registered (there is nothing to release) and the failure
-- | propagates unchanged. If the consumer drains only part of the
-- | resulting stream, the resource still releases when the scope
-- | exits.
bracketStream
  :: forall r e a
   . RIO (scope :: Scope | r) e a
  -> (a -> Aff Unit)
  -> Stream (scope :: Scope | r) e a
bracketStream acquire release = Stream do
  scope <- ask (Proxy :: Proxy "scope")
  a <- acquire
  addFinalizer scope (release a)
  pure (Yield a empty)

-- | Acquire a resource on the first pull, register its release with
-- | the supplied scope, then defer to the use-stream built from that
-- | resource. The release fires when the scope exits, regardless of
-- | how the consumer terminates: normal `Done`, mid-stream typed
-- | failure, defect, fiber kill, or an early stop by an upstream
-- | `take` (the scope simply closes when the enclosing `scoped`
-- | body exits).
-- |
-- | Equivalent in shape to `bracketStream` plus a continuation: the
-- | acquired resource is not just yielded once, it is fed to a
-- | resource-using `Stream` that can pull many elements off the
-- | underlying handle. If `acquire` fails (typed or defect), the
-- | release is not registered and the failure propagates unchanged.
-- |
-- | ```purescript
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   let s = acquireReleaseStream scope openFile closeFile linesOf
-- |   runCollect (take 100 s)
-- | ```
acquireReleaseStream
  :: forall r e a b
   . Scope
  -> RIO r e a
  -> (a -> Aff Unit)
  -> (a -> Stream r e b)
  -> Stream r e b
acquireReleaseStream scope acquire release use = Stream do
  resource <- acquire
  addFinalizer scope (release resource)
  case use resource of
    Stream pull -> pull

-- | A push-side event delivered to the emit callback handed out by
-- | `async`. Each call delivers exactly one of:
-- |
-- |   * `EmitValue a`     - emit a single element
-- |   * `EmitEnd`         - end the stream cleanly (subsequent pulls return `Done`)
-- |   * `EmitFailure v`   - terminate the stream with a typed failure
-- |   * `EmitDefect e`    - terminate the stream with a defect
-- |
-- | Once any terminal event is delivered (`EmitEnd`, `EmitFailure`,
-- | `EmitDefect`), later emits are ignored: the stream has already
-- | committed to a single outcome.
data Emit e a
  = EmitValue a
  | EmitEnd
  | EmitFailure (Variant e)
  | EmitDefect Error

-- | Build a stream from an external push-style producer.
-- |
-- | `register` runs once (in `Effect`) at the first pull and receives
-- | an emit callback that the producer uses to push `Emit` events. It
-- | returns a best-effort cleanup `Effect` that is registered as a
-- | finalizer on the supplied scope; the cleanup fires when the scope
-- | closes, regardless of how the stream terminated (`EmitEnd`,
-- | failure, defect, or downstream halt).
-- |
-- | The internal buffer is unbounded: emits never block and never drop
-- | elements. If you need to bound memory under a slow consumer, layer
-- | a bounded `Queue` between the producer and the stream or apply
-- | `throttle` downstream.
-- |
-- | Typical use: bridge a callback-style source like a WebSocket or
-- | DOM event handler into a `Stream`:
-- |
-- | ```purescript
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   let
-- |     events :: Stream r e Message
-- |     events = async scope \emit -> do
-- |       socket <- openSocket "..."
-- |       onMessage socket (\msg -> emit (EmitValue msg))
-- |       onClose socket (\_ -> emit EmitEnd)
-- |       pure (closeSocket socket)
-- |   runCollect (take 100 events)
-- | ```
async
  :: forall r e a
   . Scope
  -> ((Emit e a -> Effect Unit) -> Effect (Effect Unit))
  -> Stream r e a
async scope register = Stream do
  state <- liftEffect (Ref.new (initialAsyncState :: AsyncState e a))
  cleanup <- liftEffect (register (asyncEmit state))
  addFinalizer scope (liftEffect cleanup)
  case asyncLoop state of
    Stream pull -> pull

type AsyncState e a =
  { buffer :: Array (Emit e a)
  , waiter :: Maybe (Emit e a -> Effect Unit)
  , halted :: Boolean
  , terminal :: Maybe (Emit e a)
  }

initialAsyncState :: forall e a. AsyncState e a
initialAsyncState =
  { buffer: []
  , waiter: Nothing
  , halted: false
  , terminal: Nothing
  }

asyncEmit
  :: forall e a
   . Ref (AsyncState e a)
  -> Emit e a
  -> Effect Unit
asyncEmit ref event = do
  st <- Ref.read ref
  if st.halted then pure unit
  else case st.waiter of
    Just k -> do
      Ref.write
        (st { waiter = Nothing, halted = isTerminal event })
        ref
      k event
    Nothing -> case event of
      EmitValue _ ->
        Ref.write (st { buffer = Array.snoc st.buffer event }) ref
      _ ->
        -- Terminal event with no current waiter: stash it as `terminal`
        -- so the next pull drains the buffer first, then surfaces the
        -- terminal cause without admitting later emits.
        Ref.write
          (st { halted = true, terminal = Just event }) ref

isTerminal :: forall e a. Emit e a -> Boolean
isTerminal = case _ of
  EmitValue _ -> false
  _ -> true

asyncLoop :: forall r e a. Ref (AsyncState e a) -> Stream r e a
asyncLoop ref = Stream do
  event <- asyncInterrupt \cb -> do
    st <- Ref.read ref
    case Array.uncons st.buffer of
      Just { head, tail } -> do
        Ref.write (st { buffer = tail }) ref
        cb (Right head)
        pure (pure unit)
      Nothing -> case st.terminal of
        Just term -> do
          Ref.write (st { terminal = Nothing }) ref
          cb (Right term)
          pure (pure unit)
        Nothing
          | st.halted -> do
              cb (Right EmitEnd)
              pure (pure unit)
          | otherwise -> do
              Ref.write (st { waiter = Just (\e -> cb (Right e)) }) ref
              pure (Ref.modify_ (_ { waiter = Nothing }) ref)
  case event of
    EmitValue a -> pure (Yield a (asyncLoop ref))
    EmitEnd -> pure Done
    EmitFailure v -> rethrow v
    EmitDefect e -> die e
