-- | RIO-flavoured wrappers around `Node.EventEmitter`.
-- |
-- | The Node `EventEmitter` API is value-shaped: each subscription
-- | operation returns a removal callback rather than going through
-- | a capability handle. This module mirrors that shape and simply
-- | lifts every `Effect` operation into `RIO`. Listener-removal
-- | callbacks come back as `RIO r e Unit` so they compose with the
-- | rest of an RIO program without explicit `liftEffect`s.
-- |
-- | The `EventHandle` constructor and the canonical `EventHandleN`
-- | helpers (from `Node.EventEmitter.UtilTypes`) are re-exported
-- | so callers can declare event handles without importing
-- | `Node.EventEmitter.*` directly.
module RIO.Node.EventEmitter
  ( module Exports
  , eventNames
  , getMaxListeners
  , listenerCount
  , new
  , newListenerH
  , on
  , on_
  , once
  , once_
  , prependListener
  , prependListener_
  , prependOnceListener
  , prependOnceListener_
  , removeListenerH
  , setMaxListeners
  , setUnlimitedListeners
  ) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1)
import Node.EventEmitter (EventEmitter, EventHandle(..), SymbolOrStr) as Exports
import Node.EventEmitter (EventEmitter, EventHandle, SymbolOrStr)
import Node.EventEmitter as NE
import Node.EventEmitter.UtilTypes
  ( EventHandle0
  , EventHandle1
  , EventHandle2
  , EventHandle3
  , EventHandle4
  , EventHandle5
  , EventHandle6
  , EventHandle7
  ) as Exports
import Node.Symbol (JsSymbol) as Exports
import Node.Symbol (JsSymbol)

import RIO.Core (RIO)

-- | Allocate a fresh `EventEmitter`.
new :: forall r e. RIO r e EventEmitter
new = liftEffect NE.new

-- | The event names this emitter currently has listeners for.
-- | Stays pure: `Node.EventEmitter.eventNames` is not
-- | `Effect`-valued in the underlying binding.
eventNames :: EventEmitter -> Array (Either JsSymbol String)
eventNames = NE.eventNames

-- | The current per-event listener cap (default 10).
getMaxListeners :: forall r e. EventEmitter -> RIO r e Int
getMaxListeners ee = liftEffect (NE.getMaxListeners ee)

-- | Number of listeners currently registered for `eventName`.
listenerCount
  :: forall r e
   . EventEmitter
  -> String
  -> RIO r e Int
listenerCount ee name = liftEffect (NE.listenerCount ee name)

-- | Set the per-event listener cap.
setMaxListeners
  :: forall r e
   . Int
  -> EventEmitter
  -> RIO r e Unit
setMaxListeners n ee = liftEffect (NE.setMaxListeners n ee)

-- | Allow an unbounded number of listeners on this emitter.
setUnlimitedListeners :: forall r e. EventEmitter -> RIO r e Unit
setUnlimitedListeners ee = liftEffect (NE.setUnlimitedListeners ee)

-- | Add a listener to the end of the event's listener array.
-- | Returns a `RIO` action that, when executed, removes the
-- | listener. Use `on_` when no removal handle is needed.
on
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e (RIO r e Unit)
on h psCb ee = do
  remove <- liftEffect (NE.on h psCb ee)
  pure (liftEffect remove)

-- | `on` without a removal handle.
on_
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e Unit
on_ h psCb ee = liftEffect (NE.on_ h psCb ee)

-- | Add a one-shot listener (fires once, then auto-removes).
-- | Returns a manual removal handle so callers can unsubscribe
-- | before the event ever fires.
once
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e (RIO r e Unit)
once h psCb ee = do
  remove <- liftEffect (NE.once h psCb ee)
  pure (liftEffect remove)

-- | `once` without a removal handle.
once_
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e Unit
once_ h psCb ee = liftEffect (NE.once_ h psCb ee)

-- | Add a listener to the *start* of the listener array.
prependListener
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e (RIO r e Unit)
prependListener h psCb ee = do
  remove <- liftEffect (NE.prependListener h psCb ee)
  pure (liftEffect remove)

-- | `prependListener` without a removal handle.
prependListener_
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e Unit
prependListener_ h psCb ee = liftEffect (NE.prependListener_ h psCb ee)

-- | Add a one-shot listener to the *start* of the listener array.
prependOnceListener
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e (RIO r e Unit)
prependOnceListener h psCb ee = do
  remove <- liftEffect (NE.prependOnceListener h psCb ee)
  pure (liftEffect remove)

-- | `prependOnceListener` without a removal handle.
prependOnceListener_
  :: forall emitter psCb jsCb r e
   . EventHandle emitter psCb jsCb
  -> psCb
  -> emitter
  -> RIO r e Unit
prependOnceListener_ h psCb ee = liftEffect (NE.prependOnceListener_ h psCb ee)

-- | The built-in `"newListener"` event handle.
newListenerH
  :: EventHandle
       EventEmitter
       (Either JsSymbol String -> Effect Unit)
       (EffectFn1 SymbolOrStr Unit)
newListenerH = NE.newListenerH

-- | The built-in `"removeListener"` event handle.
removeListenerH
  :: EventHandle
       EventEmitter
       (Either JsSymbol String -> Effect Unit)
       (EffectFn1 SymbolOrStr Unit)
removeListenerH = NE.removeListenerH
